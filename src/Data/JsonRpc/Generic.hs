{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.JsonRpc.Generic (
  GFromArrayJSON, genericParseJSONRPC,
  GFieldSetJSON, genericFieldSetParseJSON,

  JsonRpcOptions (..), defaultJsonRpcOptions,

  GToArrayJSON, genericToArrayJSON,
  ) where

import GHC.Generics
import Control.Applicative ((<$>), pure, (<*>), (<*), empty, (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Control.Monad.Trans.State (StateT, runStateT, get, put)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Set ((\\))
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T
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


type FieldName = Text
type FieldsW = Writer (DList FieldName)

class GFieldSetJSON f where
  gFieldSet :: FieldsW (f a)

instance GFieldSetJSON U1 where
  gFieldSet = return U1

instance (GFieldSetJSON a, GFieldSetJSON b) => GFieldSetJSON (a :*: b) where
  gFieldSet  =  do
    x <- gFieldSet
    y <- gFieldSet
    return (x :*: y)

instance GFieldSetJSON a => GFieldSetJSON (D1 c a) where
  gFieldSet  =  do
    x <- gFieldSet
    return $ M1 x

instance GFieldSetJSON a => GFieldSetJSON (C1 c a) where
  gFieldSet  =  do
    x  <- gFieldSet
    return $ M1 x

instance (GFieldSetJSON a, Selector s) => GFieldSetJSON (S1 s a) where
  gFieldSet  =  do
    x <- gFieldSet
    saveQueriedField $ M1 x

saveQueriedField :: (GFieldSetJSON a, Selector s)
                 => S1 s a p
                 -> FieldsW (S1 s a p)
saveQueriedField m1  =  do
  tell (pure . T.pack $ selName m1)
  return m1

instance GFieldSetJSON (K1 i a) where
  gFieldSet  =  return $ K1 undefined

genericFieldSetParseJSON :: (Generic a, GFromJSON (Rep a), GFieldSetJSON (Rep a))
                         => JsonRpcOptions
                         -> Options
                         -> Value
                         -> Parser a
genericFieldSetParseJSON = d  where
  d rpcOpts opts v@(Object m)  =  do
    let (px, fs)  =  runWriter gFieldSet
        inv  =  Set.fromList (HashMap.keys m) \\
                Set.fromList (DList.toList fs)
    guard (allowNonExistField rpcOpts || Set.null inv)
      <|> fail ("object has illegal field: " ++ show (Set.toList inv))
    j  <-  genericParseJSON opts v
    let _ = from j `asTypeOf` px
    return j
  d _       opts v             =
    genericParseJSON opts v


genericParseJSONRPC :: (Generic a, GFromJSON (Rep a), GFromArrayJSON (Rep a), GFieldSetJSON (Rep a))
                    => JsonRpcOptions -> Options -> Value -> Parser a
genericParseJSONRPC rpcOpt opt = d where
  d (Array vs)      =  do (a, s) <- runStateT gFromArrayJSON $ Vector.toList vs
                          guard (allowSpilledArguemnts rpcOpt || null s)
                            <|> fail ("Too many arguments! Spilled arguments: " ++ show s)
                          return $ to a
  d v@(Object _)    =  genericFieldSetParseJSON rpcOpt opt v
  d _               =  empty

data JsonRpcOptions =
  JsonRpcOptions
  { allowSpilledArguemnts  ::  Bool
  , allowNonExistField     ::  Bool
  }

defaultJsonRpcOptions :: JsonRpcOptions
defaultJsonRpcOptions =
  JsonRpcOptions
  { allowSpilledArguemnts  =  False
  , allowNonExistField     =  True
  }


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
