module Program where

import Data.Maybe
import Network.HTTP
import Network.Stream
import Text.HTML.TagSoup
import Data.List
import qualified Control.Exception as E

type URL = String
type CompanyName = String
type PageContent = String
type Error = String

program :: [URL] -> IO ([CompanyName])
program urls = a . sequence $ map getPageContent urls

getPageContent :: URL -> IO (Either Error PageContent)
getPageContent url = fmap parseResponse response
  where response = simpleHTTP (getRequest url)

a :: IO ([Either Error PageContent]) -> IO ([CompanyName])
a = fmap (catMaybes . fmap b)

b :: Either Error PageContent -> Maybe CompanyName
b (Left _) = Nothing
b (Right a) = findParentCompanyName a

parseResponse :: Result Response_String -> Either Error PageContent
parseResponse (Left _) = Left "failed to find url"
parseResponse (Right (Response _ _ _ a)) = Right a

findParentCompanyName :: PageContent -> Maybe CompanyName
findParentCompanyName x = find (\ el -> isInfixOf "Rights Reserved" el) (catMaybes (map maybeTagText (parseTags x)))

