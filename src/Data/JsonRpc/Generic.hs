{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.JsonRpc.Generic (
  GFromArrayJSON, genericParseJSONRPC,

  GToArrayJSON, genericToArrayJSON,
  ) where

import GHC.Generics
import Control.Applicative ((<$>), (<*>), (<*), empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Aeson.Types
  (FromJSON (..), ToJSON (..), GFromJSON, genericParseJSON, Parser, Options, Value (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector


class GFromArrayJSON f where
  gFromArrayJSON :: StateT [Value] Parser (f a)

instance GFromArrayJSON U1 where
  gFromArrayJSON  =  return U1

instance (GFromArrayJSON a, GFromArrayJSON b) => GFromArrayJSON (a :*: b) where
  gFromArrayJSON  =  (:*:) <$> gFromArrayJSON <*> gFromArrayJSON

instance GFromArrayJSON a => GFromArrayJSON (M1 i c a) where
  gFromArrayJSON  =  M1 <$> gFromArrayJSON

instance FromJSON a => GFromArrayJSON (K1 i a) where
  gFromArrayJSON  =  do
    vs'  <-  get
    K1 <$> case vs' of
     v:vs  ->  (lift $ parseJSON v)   <* put vs
     []    ->   lift $ parseJSON Null


genericParseJSONRPC :: (Generic a, GFromJSON (Rep a), GFromArrayJSON (Rep a))
                    => Options -> Value -> Parser a
genericParseJSONRPC opt = d where
  d (Array vs)      =  (to <$>) . evalStateT gFromArrayJSON $ Vector.toList vs
                       -- check state to check too many arguments
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
