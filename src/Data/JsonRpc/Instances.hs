{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.JsonRpc.Instances () where

import GHC.Generics (Generic)
import Control.Applicative ((<$>), pure, (<|>))
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)
import Data.Aeson (FromJSON (..), genericParseJSON, ToJSON (..), genericToJSON, Value(..))
import Data.Aeson.Types (Options (..))
import qualified Data.Aeson.Types as Aeson

import Data.JsonRpc.Integral (fromScientific)
import Data.JsonRpc.Id (Id(..), numberId)
import Data.JsonRpc.Request (Request (..))
import Data.JsonRpc.Success (Success (..))
import Data.JsonRpc.Failure (Failure (..), Error (..), ErrorStatus (..))
import qualified Data.JsonRpc.Failure as Failure
import Data.JsonRpc.Response (Response (..))


instance FromJSON Id where
  parseJSON = d  where
    d (String t)   =  pure $ StringId t
    d (Number n)   =  maybe (parseError "Integer check error") pure $ numberId n
    d (Object {})  =  parseError "object is not allowed"
    d (Array  {})  =  parseError "array is not allowed"
    d (Bool   {})  =  parseError "boolean is not allowed"
    d  Null        =  parseError "null is not allowed"
    parseError = fail . ("JSON RPC id: " ++)

instance ToJSON Id where
  toJSON = d  where
    d (StringId s)  =  String s
    d (NumberId i)  =  Number $ fromIntegral i

deriving instance Generic (Request a)

instance FromJSON a => FromJSON (Request a) where
  parseJSON = genericParseJSON customOptions

instance ToJSON a => ToJSON (Request a) where
  toJSON = genericToJSON customOptions

deriving instance Generic (Success a)

instance FromJSON a => FromJSON (Success a) where
  parseJSON = genericParseJSON customOptions

instance ToJSON a => ToJSON (Success a) where
  toJSON = genericToJSON customOptions

deriving instance Generic (Error e)
deriving instance Generic (Failure e)

instance FromJSON ErrorStatus where
  parseJSON = d  where
    d (String {})  =  parseError "string is not allowed"
    d (Number n)   =  do
      i  <-  fromScientific n  <|>  parseError "not integer number"
      Failure.fromCode i       <|>  parseError "unknown error code range"
    d (Object {})  =  parseError "object is not allowed"
    d (Array  {})  =  parseError "array is not allowed"
    d (Bool   {})  =  parseError "boolean is not allowed"
    d  Null        =  parseError "null is not allowed"
    parseError = fail . ("JSON RPC error code: " ++)

instance ToJSON ErrorStatus where
  toJSON = Number . fromIntegral . Failure.toCode

instance FromJSON e => FromJSON (Error e) where
  parseJSON = genericParseJSON customOptions

instance ToJSON e => ToJSON (Error e) where
  toJSON = genericToJSON customOptions { omitNothingFields  =  True }

instance FromJSON e => FromJSON (Failure e) where
  parseJSON = genericParseJSON customOptions

instance ToJSON e => ToJSON (Failure e) where
  toJSON = genericToJSON customOptions

instance (FromJSON e, FromJSON a) => FromJSON (Response e a) where
  parseJSON v = Response <$> (Right <$> parseJSON v <|>
                              Left  <$> parseJSON v)

instance (ToJSON e, ToJSON a) => ToJSON (Response e a) where
  toJSON (Response r) = case r of
    Right a  -> toJSON a
    Left  e  -> toJSON e

customOptions :: Options
customOptions =
  Aeson.defaultOptions
  { fieldLabelModifier  =  \s -> fromMaybe s $ stripPrefix "_" s }
