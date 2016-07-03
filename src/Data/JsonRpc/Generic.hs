{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.JsonRpc.Generic (
  GFromArrayJSON, genericParseJSONRPC,

  GToArrayJSON, genericToArrayJSON,
  ) where

import GHC.Generics
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson.Types
  (FromJSON (..), ToJSON (..), GFromJSON, genericParseJSON, Parser, Options, Value (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector


class GFromArrayJSON f where
  gFromArrayJSON :: [Value] -> Parser (f a)

instance GFromArrayJSON U1 where
  gFromArrayJSON _  =  return U1

instance (GFromArrayJSON a, GFromArrayJSON b) => GFromArrayJSON (a :*: b) where
  gFromArrayJSON (v:vs)  =  (:*:) <$> gFromArrayJSON [v]    <*> gFromArrayJSON vs
  gFromArrayJSON  []     =  (:*:) <$> gFromArrayJSON [Null] <*> gFromArrayJSON []

instance GFromArrayJSON a => GFromArrayJSON (M1 i c a) where
  gFromArrayJSON vs  =  M1 <$> gFromArrayJSON vs

instance FromJSON a => GFromArrayJSON (K1 i a) where
  gFromArrayJSON (_:_:_) =  empty
  gFromArrayJSON [v]     =  K1 <$> parseJSON v
  gFromArrayJSON []      =  K1 <$> parseJSON Null


genericParseJSONRPC :: (Generic a, GFromJSON (Rep a), GFromArrayJSON (Rep a))
                    => Options -> Value -> Parser a
genericParseJSONRPC opt = d where
  d (Array vs)      =  to <$> gFromArrayJSON (Vector.toList vs)
  d v@(Object _)    =  genericParseJSON opt v
  d _               =  empty


class GToArrayJSON f where
  gToArrayJSON :: f a -> Vector Value

instance GToArrayJSON U1 where
  gToArrayJSON U1 = Vector.empty

instance (GToArrayJSON a, GToArrayJSON b) => GToArrayJSON (a :*: b) where
  gToArrayJSON (x :*: y)  =  gToArrayJSON x Vector.++ gToArrayJSON y

instance GToArrayJSON a => GToArrayJSON (M1 i c a) where
  gToArrayJSON (M1 x)  =  gToArrayJSON x

instance ToJSON a => GToArrayJSON (K1 i a) where
  gToArrayJSON (K1 x)  =  Vector.singleton $ toJSON x

genericToArrayJSON :: (Generic a, GToArrayJSON (Rep a))
                   => a -> Value
genericToArrayJSON = Array . gToArrayJSON . from
