{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module HoogleCommands (
  HoogleJsonResponse(..),
  produceBotResponseForHoogleCommand,
  produceBotResponseForHelpCommand
)
where

import Data.List
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
queryHoogleAPIFor targetFuncName howManyImplementationsToShow = do
  let url = "https://hoogle.haskell.org?mode=json&hoogle=" ++ targetFuncName ++ "&start=1&count=" ++ howManyImplementationsToShow ++ "&format=text"
  request <- parseRequest url
  response <- httpJSON request :: IO (Response [HoogleJsonResponse])
  return $ getResponseBody response


getFunctionSignatures :: [HoogleJsonResponse] -> [String]
getFunctionSignatures = map item


getImportantInfoAboutAllImplementations :: [HoogleJsonResponse] -> [(String, String, String)]
getImportantInfoAboutAllImplementations = map (\r -> (r.packageInfo.name, r.moduleInfo.name, r.item))


produceBotResponseForHoogleCommand targetFuncName howManyImplementationsToShow = do
  hoogleResponses <- queryHoogleAPIFor targetFuncName howManyImplementationsToShow
  let importantInfos = getImportantInfoAboutAllImplementations hoogleResponses
  let formattedInfos = map (\(p, m, s) -> "```json" ++ "\n \
                                          \ \"Package\": " ++ p ++ "\n \
                                          \ \"Module\": " ++ m ++ "\n \
                                          \ \"Signature\": " ++ s ++ "```"
                            ) importantInfos
  return $ concat formattedInfos


getAllAvailableCommands :: [String]
getAllAvailableCommands = ["hoogle", "help"]

produceBotResponseForHelpCommand :: String
produceBotResponseForHelpCommand = "Available commands: \n```" ++ intercalate "\n" (map ("- " ++) getAllAvailableCommands) ++ "```"
