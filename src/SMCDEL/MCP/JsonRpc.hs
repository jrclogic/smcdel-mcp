{-# LANGUAGE OverloadedStrings #-}
-- | Minimal JSON-RPC 2.0 types, as used by the Model Context Protocol.
--
-- Only what the MCP server needs: incoming requests and notifications,
-- outgoing results and errors. Batches are not supported (MCP dropped
-- them in protocol version 2025-06-18).
module SMCDEL.MCP.JsonRpc
  ( Message(..)
  , RpcError(..)
  , ErrorCode(..)
  , errorCodeInt
  , decodeMessage
  , resultResponse
  , errorResponse
  , rpcError
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

-- | An incoming JSON-RPC message that the server has to react to.
data Message
  = Request      Value T.Text (Maybe Value)  -- ^ id, method, params
  | Notification       T.Text (Maybe Value)  -- ^ method, params (no id, no reply)
  | ClientResponse                           -- ^ a response sent by the client; ignored
  deriving (Eq, Show)

-- | Standard JSON-RPC error codes.
data ErrorCode
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  deriving (Eq, Show)

errorCodeInt :: ErrorCode -> Int
errorCodeInt ParseError     = -32700
errorCodeInt InvalidRequest = -32600
errorCodeInt MethodNotFound = -32601
errorCodeInt InvalidParams  = -32602
errorCodeInt InternalError  = -32603

data RpcError = RpcError
  { rpcErrCode    :: ErrorCode
  , rpcErrMessage :: T.Text
  , rpcErrData    :: Maybe Value
  } deriving (Eq, Show)

rpcError :: ErrorCode -> T.Text -> RpcError
rpcError c m = RpcError c m Nothing

-- | Classify a decoded JSON value as a JSON-RPC message.
-- Returns an error (to be sent with a null id) if the value is not a
-- valid JSON-RPC 2.0 object.
decodeMessage :: Value -> Either RpcError Message
decodeMessage v = case parseEither messageParser v of
  Left err  -> Left (rpcError InvalidRequest (T.pack err))
  Right msg -> Right msg

messageParser :: Value -> Parser Message
messageParser = withObject "JSON-RPC message" $ \o -> do
  ver <- o .:? "jsonrpc" :: Parser (Maybe T.Text)
  case ver of
    Just "2.0" -> return ()
    _ -> fail "jsonrpc field must be \"2.0\""
  let hasMethod = KM.member "method" o
      hasResult = KM.member "result" o || KM.member "error" o
  if not hasMethod && hasResult
    then return ClientResponse
    else do
      method <- o .: "method"
      params <- o .:? "params"
      case KM.lookup "id" o of
        Nothing   -> return (Notification method params)
        Just Null -> return (Notification method params)
        Just i    -> case i of
          String _ -> return (Request i method params)
          Number _ -> return (Request i method params)
          _        -> fail "id must be a string or a number"

-- | Build a successful response for the given request id.
resultResponse :: Value -> Value -> Value
resultResponse i result = object
  [ "jsonrpc" .= ("2.0" :: T.Text)
  , "id"      .= i
  , "result"  .= result
  ]

-- | Build an error response for the given request id (use 'Null' if unknown).
errorResponse :: Value -> RpcError -> Value
errorResponse i (RpcError code msg mdata) = object
  [ "jsonrpc" .= ("2.0" :: T.Text)
  , "id"      .= i
  , "error"   .= object (
      [ "code"    .= errorCodeInt code
      , "message" .= msg
      ] ++ maybe [] (\d -> ["data" .= d]) mdata)
  ]
