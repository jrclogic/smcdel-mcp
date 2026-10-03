{-# LANGUAGE OverloadedStrings #-}
-- | Shared helpers for the test suite.
module Helpers
  ( muddyChildren
  , minimal
  , rpc
  , call
  , callTool
  , (.?)
  , path
  , asBool
  , asText
  , asInt
  , asList
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Vector as V

import SMCDEL.MCP.Protocol

-- | The three muddy children from the SMCDEL examples.
muddyChildren :: T.Text
muddyChildren = T.unlines
  [ "VARS 1,2,3"
  , "LAW Top"
  , "OBS alice: 2,3"
  , "    bob: 1,3"
  , "    carol: 1,2"
  ]

-- | The Minimal.smcdel.txt example.
minimal :: T.Text
minimal = T.unlines
  [ "VARS 1,2"
  , "LAW  OR (1, 2)"
  , "OBS  alice: 1"
  , "     bob:"
  , "     carol: 2"
  ]

-- | A JSON-RPC request with id 1.
rpc :: T.Text -> Value -> Value
rpc method params = object
  [ "jsonrpc" .= ("2.0" :: T.Text), "id" .= (1 :: Int), "method" .= method, "params" .= params ]

-- | Send a request through the dispatcher and return the response.
call :: ServerConfig -> T.Text -> Value -> IO Value
call cfg method params = do
  reply <- handleMessage cfg (rpc method params)
  maybe (fail "expected a response") return reply

-- | Call a tool and return the @result@ object of the response.
callTool :: ServerConfig -> T.Text -> [(Key.Key, Value)] -> IO Value
callTool cfg name args = do
  reply <- call cfg "tools/call" (object [ "name" .= name, "arguments" .= object args ])
  maybe (fail ("no result in " ++ show reply)) return (reply .? "result")

-- | Look up a key in a JSON object.
(.?) :: Value -> Key.Key -> Maybe Value
Object o .? k = KM.lookup k o
_        .? _ = Nothing

-- | Follow a path of keys.
path :: [Key.Key] -> Value -> Maybe Value
path []     v = Just v
path (k:ks) v = v .? k >>= path ks

asBool :: Maybe Value -> Maybe Bool
asBool (Just (Bool b)) = Just b
asBool _ = Nothing

asText :: Maybe Value -> Maybe T.Text
asText (Just (String t)) = Just t
asText _ = Nothing

asInt :: Maybe Value -> Maybe Int
asInt (Just (Number n)) = toBoundedInteger n
asInt _ = Nothing

asList :: Maybe Value -> Maybe [Value]
asList (Just (Array a)) = Just (V.toList a)
asList _ = Nothing
