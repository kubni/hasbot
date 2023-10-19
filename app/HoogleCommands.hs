{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module HoogleCommands (
  HoogleJsonResponse(..),
  hoogleSignatures,
  hoogleDocs
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
queryHoogleAPIFor targetFuncName howManyImplementationsToShow = do
  let url = "https://hoogle.haskell.org?mode=json&hoogle=" ++ targetFuncName ++ "&start=1&count=" ++ howManyImplementationsToShow ++ "&format=text"
  request <- parseRequest url
  response <- httpJSON request :: IO (Response [HoogleJsonResponse])
  return $ getResponseBody response


indentDocumentation :: String -> String
indentDocumentation doc = unlines $ map ("\t" ++) $ lines doc


getFunctionDocs :: [HoogleJsonResponse] -> [(String, String, String)]
getFunctionDocs = map (\r -> (r.packageInfo.name, r.moduleInfo.name, indentDocumentation r.docs))


getFunctionSignatures :: [HoogleJsonResponse] -> [(String, String, String)]
getFunctionSignatures = map (\r -> (r.packageInfo.name, r.moduleInfo.name, r.item))

hoogleDocs :: String -> String -> IO String
hoogleDocs targetFuncName nImplementations = do
  hoogleResponses <- queryHoogleAPIFor targetFuncName nImplementations
  let formattedDocs = map (\(p, m, d) -> "```json\n \
                                         \\"Package\": " ++ p ++ "\n \
                                         \\"Module\": " ++ m ++ "\n \
                                         \\"Documentation\" : \n" ++ d ++ "```"
                          ) (getFunctionDocs hoogleResponses)
  return $ concat formattedDocs

hoogleSignatures :: String -> String -> IO String
hoogleSignatures targetFuncName nImplementations = do
  hoogleResponses <- queryHoogleAPIFor targetFuncName nImplementations
  let importantInfos = getFunctionSignatures hoogleResponses
  let formattedInfos = map (\(p, m, s) -> "```json" ++ "\n \
                                          \\"Package\": " ++ p ++ "\n \
                                          \\"Module\": " ++ m ++ "\n \
                                          \\"Signature\": " ++ s ++ "```"
                           ) importantInfos
  return $ concat formattedInfos


