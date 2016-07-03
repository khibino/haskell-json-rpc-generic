{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.JsonRpc.Success (
  Success (..), success,
  ) where

import Data.Text (Text, pack)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Data.JsonRpc.Id (Id)


data Success a =
  Success
  { _jsonrpc :: !Text
  , _id      :: !Id
  , _result  :: !a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

success :: Id -> a -> Success a
success = Success $ pack "2.0"
