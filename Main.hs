import Program
import Data.Maybe
import Network.HTTP
import Text.HTML.TagSoup
import Network.Stream
import qualified Data.List as L
import qualified Control.Exception as E

main :: IO ()
main = do
  let input = ["http://pivotal.io/labs"]
  companyNames <- program input
  mapM_ putStrLn companyNames

