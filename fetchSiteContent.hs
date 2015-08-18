import Data.Maybe
import Network.HTTP
import Network.Stream

type URL = String
type CompanyName = String
type PageContent = String
type Error = String

main :: IO ()
main = do
  let input = ["http://www.google.com/"]
  companyNames <- program input
  mapM_ putStrLn companyNames

program :: [URL] -> IO ([CompanyName])
program urls = c . sequence $ a urls

getCompanyName :: PageContent -> Maybe CompanyName
getCompanyName x = Just x

getPageContent :: URL -> IO (Either Error PageContent)
getPageContent url = fmap f response
  where response = simpleHTTP (getRequest url)

a :: [URL] -> [IO (Either Error PageContent)]
a = fmap getPageContent

c :: IO ([Either Error PageContent]) -> IO ([CompanyName])
c = fmap (catMaybes . fmap d)

d :: Either Error PageContent -> Maybe CompanyName
d (Left _) = Nothing
d (Right a) = getCompanyName a

f :: Result Response_String -> Either Error PageContent
f (Left _) = Left "failed to find url"
f (Right (Response _ _ _ a)) = Right a
