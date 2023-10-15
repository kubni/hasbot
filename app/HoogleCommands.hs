{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module HoogleCommands (
  queryHoogleAPIFor
)
where

import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Simple

data ModuleInfo = ModuleInfo {
                                name :: String,
                                url :: String
                             } deriving (Show, Generic)

data PackageInfo = PackageInfo {
                                  name :: String,
                                  url :: String
                               } deriving (Show, Generic)

data HoogleJsonResponse = HoogleJsonResponse {
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


queryHoogleAPIFor :: String -> IO ()
queryHoogleAPIFor targetFuncName = do
  let url = "https://hoogle.haskell.org?mode=json&hoogle=" ++ targetFuncName ++ "&start=1&count=2&format=text"
  request <- parseRequest url
  response <- httpJSON request :: IO (Response [HoogleJsonResponse])
  let responseBody = getResponseBody response
  print responseBody
