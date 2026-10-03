{-# LANGUAGE OverloadedStrings #-}
-- | The Model Context Protocol layer: a transport-independent dispatcher
-- for the JSON-RPC methods @initialize@, @ping@, @tools/list@ and
-- @tools/call@. Notifications are accepted and ignored.
--
-- The server is stateless: no session ids are issued and @initialize@ is
-- not required before calling tools.
module SMCDEL.MCP.Protocol
  ( ServerConfig(..)
  , newServerConfig
  , handleMessage
  , handleBytes
  , serverName
  , serverVersion
  , defaultProtocolVersion
  , supportedProtocolVersions
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import qualified Data.Text as T
import Data.Version (showVersion)

import Paths_smcdel_mcp (version)
import SMCDEL.MCP.JsonRpc
import SMCDEL.MCP.Tools

newtype ServerConfig = ServerConfig { cfgTools :: ToolConfig }

-- | Create the server configuration with the given per-call timeout in seconds.
newServerConfig :: Int -> IO ServerConfig
newServerConfig secs = ServerConfig <$> newToolConfig secs

serverName :: T.Text
serverName = "smcdel-mcp"

serverVersion :: T.Text
serverVersion = T.pack (showVersion version)

defaultProtocolVersion :: T.Text
defaultProtocolVersion = "2025-06-18"

supportedProtocolVersions :: [T.Text]
supportedProtocolVersions = [ "2025-06-18", "2025-03-26", "2024-11-05" ]

-- | Handle a raw request body. Invalid JSON yields a parse error response.
handleBytes :: ServerConfig -> BL.ByteString -> IO (Maybe Value)
handleBytes cfg bytes = case eitherDecode bytes of
  Left err -> return . Just $ errorResponse Null (rpcError ParseError (T.pack ("Invalid JSON: " ++ err)))
  Right v  -> handleMessage cfg v

-- | Handle one decoded JSON-RPC message. Returns 'Nothing' when no reply
-- must be sent (notifications and responses from the client).
handleMessage :: ServerConfig -> Value -> IO (Maybe Value)
handleMessage cfg v = case decodeMessage v of
  Left err -> return . Just $ errorResponse Null err
  Right ClientResponse -> return Nothing
  Right (Notification _ _) -> return Nothing
  Right (Request i method params) -> do
    outcome <- handleRequest cfg method params
    return . Just $ either (errorResponse i) (resultResponse i) outcome

handleRequest :: ServerConfig -> T.Text -> Maybe Value -> IO (Either RpcError Value)
handleRequest cfg method params = case method of
  "initialize" -> return . Right $ initializeResult params
  "ping"       -> return . Right $ object []
  "tools/list" -> return . Right $ object
    [ "tools" .= map toolDefinition (tools (cfgTools cfg)) ]
  "tools/call" -> callTool cfg params
  _ -> return . Left $ rpcError MethodNotFound ("Method not found: " <> method)

initializeResult :: Maybe Value -> Value
initializeResult params = object
  [ "protocolVersion" .= negotiated
  , "capabilities" .= object [ "tools" .= object [ "listChanged" .= False ] ]
  , "serverInfo" .= object
      [ "name"    .= serverName
      , "title"   .= ("SMCDEL model checker" :: T.Text)
      , "version" .= serverVersion
      ]
  , "instructions" .= syntaxPrimer
  ]
  where
    requested = params >>= parseMaybe (withObject "params" (.: "protocolVersion"))
    negotiated = case requested of
      Just v | v `elem` supportedProtocolVersions -> v
      _ -> defaultProtocolVersion

callTool :: ServerConfig -> Maybe Value -> IO (Either RpcError Value)
callTool cfg params =
  case params >>= parseMaybe (withObject "params" (\o -> (,) <$> o .: "name" <*> o .:? "arguments")) of
    Nothing -> return . Left $ rpcError InvalidParams "tools/call needs params with a tool 'name'"
    Just (name, mArgs) ->
      case find ((== name) . toolName) (tools (cfgTools cfg)) of
        Nothing -> return . Left $ rpcError InvalidParams ("Unknown tool: " <> name)
        Just tool -> do
          outcome <- toolRun tool (maybe (object []) id mArgs)
          return $ case outcome of
            Left err -> Left (rpcError InvalidParams ("Invalid arguments for " <> name <> ": " <> err))
            Right r  -> Right (toolResultValue r)
