{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , TemplateHaskell #-}
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC as DB
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.CGI as WCGI
import qualified Network.Wai.Parse as WP
import qualified Network.Wai as W
import qualified Network.HTTP.Types as HT
import qualified Text.Templating.Heist as H
import qualified Data.Yaml as Y
import qualified Data.Digest.Pure.SHA as S
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Blaze.ByteString.Builder as BB
import qualified Control.Monad.Trans.Resource as Re
import qualified Text.XmlHtml as X
import Data.String
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import Control.Applicative

import Development.Placeholders
import Debug.Trace

data HandlerContext = HandlerContext
    { heistState :: H.HeistState Handler
    , request :: W.Request
    , dbConnection :: DB.ConnWrapper
    , config :: Config
    }

type Handler = ErrorT EResponse (ReaderT HandlerContext (Re.ResourceT IO))

newtype EResponse = EResponse {unEResponse :: W.Response}

instance Error EResponse where
    strMsg = EResponse . W.responseLBS HT.status404 [] . fromString

data Attendee = Attendee
    { attendeeName :: T.Text
    , attendeeComment :: T.Text
    , attendeeEncryptedPassword :: BSL.ByteString
    }

data Config = Config
    { configSalt :: BSL.ByteString
    , localizeTable :: M.Map BS.ByteString T.Text
    , scriptPath :: T.Text
    }

runHandler :: Handler W.Response -> HandlerContext -> Re.ResourceT IO W.Response
runHandler h ctx = either unEResponse id <$> runReaderT (runErrorT h) ctx

application :: H.HeistState Handler -> DB.ConnWrapper -> Config -> W.Application
application h conn c req = runHandler (routing req) HandlerContext
    { heistState = h
    , request = req
    , dbConnection = conn
    , config = c
    }

routing :: W.Request -> Handler W.Response
routing req = case W.requestMethod req of
    "GET" -> handleGET $ W.pathInfo req
    "POST" -> handlePOST $ W.pathInfo req
    _ -> fatalResponse
  where
    handleGET [] = handlerIndex
    handleGET _ = fatalResponse

    handlePOST ["new"] = handlerNewAttendee
    handlePOST ["delete"] = handlerDeleteAttendee
    handlePOST _ = fatalResponse

handlerIndex :: Handler W.Response
handlerIndex = topPageResponse []

handlerNewAttendee :: Handler W.Response
handlerNewAttendee = do
    params <- getParams
    name <- refParam "attendee-name" params
    comment <- refParam "attendee-comment" params
    encrypted <- refEncryptedPassword "attendee-password" params
    validate Attendee
        { attendeeName = name
        , attendeeComment = comment
        , attendeeEncryptedPassword = encrypted
        }
    conn <- asks dbConnection
    void $ liftIO $ DB.run conn
        "INSERT INTO attendees (name, comment, encrypted_password) VALUES (?, ?, ?)"
        [ DB.toSql name
        , DB.toSql comment
        , DB.toSql encrypted
        ]
    liftIO $ DB.commit conn
    redirectResponse "/"

handlerDeleteAttendee :: Handler W.Response
handlerDeleteAttendee = do
    params <- getParams
    id_ <- refParam "attendee-id" params
    pw <- refEncryptedPassword "attendee-password" params
    conn <- asks dbConnection
    nrows <- liftIO $ DB.run conn
        "DELETE FROM attendees WHERE id = ? AND encrypted_password = ?"
        [ DB.toSql id_
        , DB.toSql pw
        ]
    unless (nrows == 1)
        $ errorResponse =<< mapM localize ["delete-failed"]
    liftIO $ DB.commit conn
    redirectResponse "/"

topPageResponse :: [T.Text] -> Handler W.Response
topPageResponse notice = do
    conn <- asks dbConnection
    stmt <- liftIO $ DB.prepare conn "SELECT id, name, comment FROM attendees ORDER BY id"
    void $ liftIO $ DB.execute stmt []
    rows <- liftIO $ DB.fetchAllRows stmt
    spath <- scriptPath <$> asks config
    viewResponse
        [ ("attendees", H.mapSplices mapRow rows)
        , ("notice", noticeSplice)
        , ("script-path", H.textSplice spath)
        ]
  where
    mapRow [id_, name, comment] = return [X.Element "tr" []
        [ X.Element "td" [] [X.TextNode $ DB.fromSql id_]
        , X.Element "td" [] [X.TextNode $ DB.fromSql name]
        , X.Element "td" [] [X.TextNode $ DB.fromSql comment]
        ]]
    mapRow _ = undefined

    noticeSplice = return [X.Element "ul" [] $ toElem <$> notice]
      where
        toElem msg = X.Element "li" [] [X.TextNode msg]

validate :: Attendee -> Handler ()
validate a = do
    es <- execWriterT $ do
        when (T.null $ attendeeName a) $ do
            msg <- lift $ T.concat <$> mapM localize ["attendee-name", "is-empty"]
            tell [msg]
        when (T.null $ attendeeComment a) $ do
            msg <- lift $ T.concat <$> mapM localize ["attendee-comment", "is-empty"]
            tell [msg]
    unless (null es) $ errorResponse es

refParam :: BS.ByteString -> [(BS.ByteString, T.Text)] -> Handler T.Text
refParam key params = maybe fatalResponse pure $ lookup key params

refEncryptedPassword :: BS.ByteString -> [(BS.ByteString, T.Text)] -> Handler BSL.ByteString
refEncryptedPassword key params = encrypt
    =<< BSL.fromChunks . return . TE.encodeUtf8
    <$> refParam key params
  where
    encrypt str = S.bytestringDigest . S.sha1 <$> (BSL.append
        <$> (configSalt <$> asks config)
        <*> pure str)

viewResponse :: [(T.Text, H.Splice Handler)] -> Handler W.Response
viewResponse splices = do
    heist <- asks heistState
    result <- H.renderTemplate (H.bindSplices splices heist) "index"
    maybe fatalResponse mkResponse result
  where
    mkResponse = return
        . W.responseLBS HT.status200 [HT.headerContentType "text/html"]
        . BB.toLazyByteString
        . fst

redirectResponse :: BS.ByteString -> Handler W.Response
redirectResponse path = return $ W.responseLBS HT.status301 [("Location", path)] ""

errorResponse :: [T.Text] -> Handler a
errorResponse = throwError . EResponse <=< topPageResponse

fatalResponse :: Handler a
fatalResponse = throwError $ strMsg "invalid request"

getParams :: Handler [(BS.ByteString, T.Text)]
getParams = do
    req <- asks request
    (params, _) <- lift . lift $ WP.parseRequestBody WP.lbsBackEnd req
    return $ toText <$> params
  where
    toText (k, v) = (k, TE.decodeUtf8 v)

localize :: BS.ByteString -> Handler T.Text
localize key = localizeTable <$> asks config
    >>= maybe fatalResponse pure . M.lookup key

loadConfig :: Y.FromJSON a => FilePath -> IO a
loadConfig = maybe (error "valid yaml file?") pure . Y.decode <=< BS.readFile

main :: IO ()
main = do
    (conf :: M.Map BS.ByteString String) <- loadConfig configPath
    heist <- either error id
        <$> H.loadTemplates (conf M.! "templates-dir") H.defaultHeistState
    conn <- DB.ConnWrapper <$> Sqlite3.connectSqlite3 (conf M.! "database-path")
    ltable <- loadConfig $ conf M.! "locale-path"
    WCGI.run $ application heist conn Config
        { configSalt = fromString $ conf M.! "salt"
        , localizeTable = ltable
        , scriptPath = fromString $ conf M.! "script-path"
        }

configPath :: FilePath
configPath = ""
