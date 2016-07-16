{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

module Instances (Example (..)) where

import Test.QuickCheck (Arbitrary (..), Gen, frequency, choose)

import GHC.Generics (Generic)
import Control.Applicative ((<$>), pure, (<*>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson.Types as Aeson

import Data.JsonRpc
  (Id(..), Request (..), request,
   Success (..), success,
   Failure (..), Error (..), ErrorStatus (..), failure, makeError,
   Response (..),
   genericParseJSONRPC, defaultJsonRpcOptions, )


genText :: Gen Text
genText = T.pack <$> arbitrary

instance Arbitrary Id where
  arbitrary =
    frequency
    [ (3, NumberId <$> arbitrary)
    , (2, StringId . T.pack <$> arbitrary)
    ]

instance Arbitrary a => Arbitrary (Request a) where
  arbitrary =
    request
    <$> genText
    <*> arbitrary
    <*> arbitrary

instance Arbitrary a => Arbitrary (Success a) where
  arbitrary =
    success
    <$> arbitrary
    <*> arbitrary

genServerError :: Gen ErrorStatus
genServerError = ServerError <$> choose (-32099, -32000)

genMethodErrorA :: Gen ErrorStatus
genMethodErrorA = MethodError <$> choose (-31999, 0) <*> genText

genMethodErrorB :: Gen ErrorStatus
genMethodErrorB = MethodError <$> choose (-65535, -32769) <*> genText

instance Arbitrary ErrorStatus where
  arbitrary =
    frequency
    [ (1, pure ParseError)
    , (1, pure InvalidRequest)
    , (1, pure MethodNotFound)
    , (1, pure InvalidParams)
    , (1, pure InternalError)
    , (2, genServerError)
    , (2, genMethodErrorA)
    , (2, genMethodErrorB)
    ]

instance Arbitrary e => Arbitrary (Error e) where
  arbitrary =
    makeError
    <$> arbitrary
    <*> arbitrary

instance Arbitrary e => Arbitrary (Failure e) where
  arbitrary =
    failure
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance (Arbitrary e, Arbitrary a) => Arbitrary (Response e a) where
  arbitrary =
    frequency
    [ (2, Response . Right <$> arbitrary)
    , (3, Response . Left  <$> arbitrary)
    ]


data Example =
  Example
  { p :: Int
  , q :: String
  , r :: Maybe Int
  , s :: [Int]
  , t :: Maybe [Int]
  , u :: Maybe String
  } deriving (Eq, Show, Generic)

instance Arbitrary Example where
  arbitrary =
    Example
    <$> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary

instance FromJSON Example where
  parseJSON = genericParseJSONRPC defaultJsonRpcOptions Aeson.defaultOptions

instance ToJSON Example
