{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Data.JsonRpc.Response (
  Response (..),
  ) where

import Data.JsonRpc.Success (Success (..))
import Data.JsonRpc.Failure (Failure (..))


newtype Response e a =
  Response (Either (Failure e) (Success a))
  deriving (Eq, Show, Functor)
