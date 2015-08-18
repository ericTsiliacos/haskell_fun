import Data.Maybe
import Network.HTTP
import Network.Stream

type URL = String
type CompanyName = String
type PageContent = String
type Error = String

main :: IO ()
main = do
  let input = ["http://www.google.com/", "http://www.cnn.com/us"]
  companyNames <- program input
  mapM_ putStrLn companyNames

program :: [URL] -> IO ([CompanyName])
program urls = c . b $ a urls

getCompanyName :: PageContent -> Maybe CompanyName
getCompanyName x = Just x

getPageContent :: URL -> IO (Either Error PageContent)
getPageContent url = fmap f response
  where response = simpleHTTP (getRequest url)

a :: [URL] -> [IO (Either Error PageContent)]
a = fmap getPageContent

b :: [IO (Either Error PageContent)] -> IO ([Either Error PageContent])
b ios = sequence ios

c :: IO ([Either Error PageContent]) -> IO ([CompanyName])
c = fmap (catMaybes . d)

d :: [Either Error PageContent] -> [Maybe CompanyName]
d = fmap e

e :: Either Error PageContent -> Maybe CompanyName
e (Left _) = Nothing
e (Right a) = getCompanyName a

f :: Result Response_String -> Either Error PageContent
f (Left _) = Left "failed to find url"
f (Right (Response _ _ _ a)) = Right a
