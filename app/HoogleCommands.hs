{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module HoogleCommands (
  HoogleJsonResponse(..),
  queryHoogleAPIFor,
  getImportantInfo
)
where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Simple


data ModuleInfo = ModuleInfo
  {
    name :: String,
    url :: String
  } deriving (Show, Generic)

data PackageInfo = PackageInfo
  {
    name :: String,
    url :: String
  } deriving (Show, Generic)

data HoogleJsonResponse = HoogleJsonResponse
  {
    docs :: String,
    item :: String,
    moduleInfo :: ModuleInfo,
    packageInfo :: PackageInfo,
    typeInfo :: String,
    url :: String
  } deriving (Show, Generic)


instance FromJSON ModuleInfo
instance ToJSON ModuleInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PackageInfo
instance ToJSON PackageInfo where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HoogleJsonResponse where
  parseJSON = withObject "HoogleJsonResponse" $ \o -> do
    docs <- o .: "docs"
    item <- o .: "item"
    moduleInfo <- o .: "module"
    packageInfo <- o .: "package"
    typeInfo <- o .: "type"
    url <- o .: "url"
    return $ HoogleJsonResponse docs item moduleInfo packageInfo typeInfo url

instance ToJSON HoogleJsonResponse where
  toEncoding = genericToEncoding defaultOptions


queryHoogleAPIFor :: String -> String -> IO [HoogleJsonResponse]
queryHoogleAPIFor targetFuncName howManyVersionsToShow = do
  let url = "https://hoogle.haskell.org?mode=json&hoogle=" ++ targetFuncName ++ "&start=1&count=" ++ howManyVersionsToShow ++ "&format=text"
  request <- parseRequest url
  response <- httpJSON request :: IO (Response [HoogleJsonResponse])
  return $ getResponseBody response


-- TODO: ...
getFunctionSignatures :: [HoogleJsonResponse] -> [String]
getFunctionSignatures = map item


getImportantInfo :: [HoogleJsonResponse] -> [(String, String, String)]
getImportantInfo = map (\r -> (r.packageInfo.name, r.moduleInfo.name, r.item))
