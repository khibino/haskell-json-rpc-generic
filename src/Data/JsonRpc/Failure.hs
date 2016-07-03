{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.JsonRpc.Failure (
  Failure (..), Error (..), ErrorStatus (..),

  failure, makeError,
  serverError,
  methodError,
  emptyError,
  ) where

import Prelude hiding (userError)
import Control.Monad (MonadPlus, guard)
import Data.Text (Text)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Data.JsonRpc.Id (Id)


data Failure e =
  Failure
  { _jsonrpc :: !Text
  , _id      :: !(Maybe Id)
  , _error   :: !(Error e)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Error e =
  Error
  { _code    :: !Int
  , _message :: !Text
  , _data    :: !(Maybe e)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data ErrorStatus
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerError !Int
  | MethodError !Int !Text
  deriving (Eq, Show)

failure :: Maybe Id -> ErrorStatus -> Maybe e -> Failure e
failure mayId s =
  Failure "2.0" mayId . makeError s

makeError :: ErrorStatus -> Maybe e -> Error e
makeError = d  where
  d  ParseError         =  Error (-32700) "Parse error"
  d  InvalidRequest     =  Error (-32600) "Invalid Request"
  d  MethodNotFound     =  Error (-32601) "Method not found"
  d  InvalidParams      =  Error (-32602) "Invalid params"
  d  InternalError      =  Error (-32603) "Internal error"
  d (ServerError c)     =  Error       c  "Server error"
  d (MethodError c m)   =  Error       c   m

serverError :: MonadPlus m
            => Int
            -> m ErrorStatus
serverError c = do
  guard $ -32099 <= c && c <= -32000
  return $ ServerError c

methodError :: MonadPlus m
            => Int
            -> Text
            -> m ErrorStatus
methodError c s = do
  guard $ c < -32768 || -32000 < c
  return $ MethodError c s

emptyError :: Maybe ()
emptyError = Nothing
