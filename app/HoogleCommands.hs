{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HoogleCommands where

import Data.Aeson
import Data.Text
import GHC.Generics

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

instance ToJSON ModuleInfo where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON PackageInfo where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON HoogleJsonResponse where
  toEncoding = genericToEncoding defaultOptions
