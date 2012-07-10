{-# LANGUAGE ScopedTypeVariables #-}
import qualified Network.Wai.Handler.CGI as WCGI
import RicoraAtnd2
import Control.Exception
import System.IO
import Prelude hiding (catch)

main :: IO ()
main = (WCGI.run =<< loadApp "./ricora-atnd2.yml") `catch` \(e :: SomeException) ->
    bracket (openFile "./debug.log" AppendMode) hClose $ flip hPrint e
