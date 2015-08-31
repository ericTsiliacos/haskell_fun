module Program where

import Data.Maybe
import Network.HTTP
import Network.Stream

type URL = String
type CompanyName = String
type PageContent = String
type Error = String

program :: [URL] -> IO ([CompanyName])
program urls = a . sequence $ fmap getPageContent urls

getCompanyName :: PageContent -> Maybe CompanyName
getCompanyName x = Just x

getPageContent :: URL -> IO (Either Error PageContent)
getPageContent url = fmap c response
  where response = simpleHTTP (getRequest url)

a :: IO ([Either Error PageContent]) -> IO ([CompanyName])
a = fmap (catMaybes . fmap b)

b :: Either Error PageContent -> Maybe CompanyName
b (Left _) = Nothing
b (Right a) = getCompanyName a

c :: Result Response_String -> Either Error PageContent
c (Left _) = Left "failed to find url"
c (Right (Response _ _ _ a)) = Right a
