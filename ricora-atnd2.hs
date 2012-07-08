import qualified Network.Wai.Handler.CGI as WCGI
import RicoraAtnd2

main :: IO ()
main = WCGI.run =<< loadApp "./ricora-atnd2.yml"
