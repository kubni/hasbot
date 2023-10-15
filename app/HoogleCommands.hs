{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module HoogleCommands (
                        testCurl
                      )
where

import Data.Aeson
import Data.Text
import GHC.Generics

import Network.Curl

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

instance FromJSON HoogleJsonResponse
instance ToJSON HoogleJsonResponse where
  toEncoding = genericToEncoding defaultOptions


-- Manual way:
-- instance FromJSON ModuleInfo where
--   parseJSON = withObject "ModuleInfo" $ \o -> do
--     name <- o .: "name"
--     url <- o .: "url"
--     return $ ModuleInfo name url

testCurl = do
  pair <- curlGetString "https://hoogle.haskell.org?mode=json&hoogle=map&start=1&count=2" []
  let jsonString = snd pair
  let encodedJson = encode jsonString
  print encodedJson
