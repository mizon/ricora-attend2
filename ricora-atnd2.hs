{-# LANGUAGE ScopedTypeVariables #-}
import qualified Network.Wai.Handler.CGI as WCGI
import RicoraAtnd2
import Control.Exception
import System.IO
import qualified System.Directory as D
import System.FilePath
import Control.Applicative
import Prelude hiding (catch)

main :: IO ()
main = do
    conf <- (</> ".ricora-atnd2.yml") <$> D.getHomeDirectory
    debug <- (</> "ricora-atnd2.debug") <$> D.getHomeDirectory
    (WCGI.run =<< loadApp conf) `catch` \(e :: SomeException) ->
        bracket (openFile debug AppendMode) hClose $ flip hPrint e
