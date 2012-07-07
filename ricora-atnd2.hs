{-# LANGUAGE OverloadedStrings
           , TemplateHaskell #-}
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC as DB
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.CGI as WCGI
import qualified Network.Wai.Parse as WP
import qualified Network.Wai as W
import qualified Network.HTTP.Types as HT
import qualified Text.Templating.Heist as H
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
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
    }

type Handler = ErrorT EResponse (ReaderT HandlerContext (Re.ResourceT IO))

newtype EResponse = EResponse {unEResponse :: W.Response}

instance Error EResponse where
    strMsg = EResponse . W.responseLBS HT.status404 [] . fromString

data Attendee = Attendee
    { attendeeName :: T.Text
    , attendeeComment :: T.Text
    }

runHandler :: Handler W.Response -> HandlerContext -> Re.ResourceT IO W.Response
runHandler h ctx = either unEResponse id <$> runReaderT (runErrorT h) ctx

application :: H.HeistState Handler -> DB.ConnWrapper -> W.Application
application h conn req = runHandler (routing req) HandlerContext
    { heistState = h
    , request = req
    , dbConnection = conn
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
    validate Attendee {attendeeName = name, attendeeComment = comment}
    conn <- asks dbConnection
    void $ liftIO $ DB.run conn "INSERT INTO attendees (name, comment) VALUES (?, ?)"
        [ DB.toSql name
        , DB.toSql comment
        ]
    liftIO $ DB.commit conn
    redirectResponse "/"

handlerDeleteAttendee :: Handler W.Response
handlerDeleteAttendee = do
    params <- getParams
    id_ <- refParam "attendee-id" params
    pw <- refParam "attendee-password" params
    conn <- asks dbConnection
    nrows <- liftIO $ DB.run conn "DELETE FROM attendees WHERE id = ?" [DB.toSql id_]
    unless (nrows == 1)
        $ errorResponse ["invalid id or password"]
    liftIO $ DB.commit conn
    redirectResponse "/"

topPageResponse :: [T.Text] -> Handler W.Response
topPageResponse notice = do
    conn <- asks dbConnection
    stmt <- liftIO $ DB.prepare conn "SELECT id, name, comment FROM attendees ORDER BY id"
    void $ liftIO $ DB.execute stmt []
    rows <- liftIO $ DB.fetchAllRows stmt
    viewResponse
        [ ("attendees", H.mapSplices mapRow rows)
        , ("notice", noticeSplice)
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
validate a = unless (null errors) $ errorResponse errors
  where
    errors = execWriter $ do
        when (T.null $ attendeeName a)
            $ tell ["no name"]
        when (T.null $ attendeeComment a)
            $ tell ["no comment"]

refParam :: BS.ByteString -> [(BS.ByteString, T.Text)] -> Handler T.Text
refParam key params = maybe fatalResponse pure $ lookup key params

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

main :: IO ()
main = serverMain =<< loadApp
  where
    serverMain app = do
        putStrLn $ "listening on " ++ show port
        Warp.run port app
      where
        port = 3000

    cgiMain = WCGI.run

    reloading req = do
        app <- liftIO $ loadApp
        app req

    loadApp = application
        <$> (either error id <$> H.loadTemplates "./templates" H.defaultHeistState)
        <*> (DB.ConnWrapper <$> Sqlite3.connectSqlite3 "./test.sqlite3")
