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
import Data.Maybe (fromMaybe)
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

failure :: Maybe Id -> ErrorStatus -> Maybe Text -> Maybe e -> Failure e
failure mayId s mm =
  Failure "2.0" mayId . makeError s mm

defaultMessage :: ErrorStatus -> Text
defaultMessage = d  where
  d  ParseError         =  "Parse error"
  d  InvalidRequest     =  "Invalid Request"
  d  MethodNotFound     =  "Method not found"
  d  InvalidParams      =  "Invalid params"
  d  InternalError      =  "Internal error"
  d (ServerError _)     =  "Server error"
  d (MethodError _ m)   =  m

makeError :: ErrorStatus -> Maybe Text -> Maybe e -> Error e
makeError e mm = d e $ fromMaybe (defaultMessage e) mm    where
  d  ParseError         =  Error (-32700)
  d  InvalidRequest     =  Error (-32600)
  d  MethodNotFound     =  Error (-32601)
  d  InvalidParams      =  Error (-32602)
  d  InternalError      =  Error (-32603)
  d (ServerError c)     =  Error       c
  d (MethodError c _)   =  Error       c

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
