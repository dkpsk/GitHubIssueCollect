{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy as B(readFile)
import Network.HTTP.Simple
import System.Environment(getArgs)


data Issue = Issue { number :: Int, type_ :: Maybe Object, title :: String, date :: String }
type Issues = [Issue]
data IssueType = P | I deriving Show

instance Show Issue where
  show i = show(number i) ++ " | " ++ show(f(type_ i)) ++ " | " ++ date i ++ " | " ++ title i where
    f :: Maybe a -> IssueType
    f = maybe I (const P)

instance FromJSON Issue where
  parseJSON (Object v) = Issue <$>
                         v .:  "number" <*>
                         v .:? "pull_request" <*> -- Parser IssueType にmapできるはずなんだけど…。
                         v .:  "title" <*>
                         v .:  "created_at"
  parseJSON _ = error "Unknown json" 

type RepositoryName = String
type UserName = String

fetchIssues :: UserName -> RepositoryName -> IO (Response Issues)
fetchIssues user repo = httpJSON $ request where
  endpoint = "https://api.github.com/repos/" ++ user ++ "/" ++ repo ++ "/issues"
  request = setRequestSecure True $ addRequestHeader "User-Agent" "user/dkpsk"  $ parseRequest_ endpoint

main :: IO ()
main = do
  user:repo:_ <- getArgs
  (code, body) <- (\r -> (getResponseStatusCode r, getResponseBody r)) <$> fetchIssues user repo
  if code == 200
    then sequence_ $ putStrLn.show <$> take 12 body
    else error $ "Error: StatusCode " ++ show code
