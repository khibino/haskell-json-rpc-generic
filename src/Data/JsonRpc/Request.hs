{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.JsonRpc.Request ( Request(..) ) where

import Prelude hiding (id)
import Data.Text (Text)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Data.JsonRpc.Id (Id)


data Request a =
  Request
  { jsonrpc :: !Text
  , method  :: !Text
  , params  :: !(Maybe a)
  , id      :: !(Maybe Id)
  } deriving (Eq, Show, Read, Functor, Foldable, Traversable)
