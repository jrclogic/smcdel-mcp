{-# LANGUAGE OverloadedStrings #-}
-- | The Streamable HTTP transport of the Model Context Protocol, served
-- with scotty. One endpoint, @POST /mcp@, takes a single JSON-RPC message.
--
-- * Requests are answered with a @200@ and one JSON-RPC response as @application/json@.
-- * Notifications are answered with @202 Accepted@ and an empty body.
-- * @GET@ and @DELETE@ on the endpoint return @405@: this server never
--   pushes messages and keeps no sessions.
module SMCDEL.MCP.Server
  ( mcpApp
  , mcpScotty
  , runServer
  , originAllowed
  ) where

import Control.Monad (unless)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (accepted202, forbidden403, methodNotAllowed405)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import Web.Scotty

import SMCDEL.MCP.Protocol

-- | Build the WAI application, useful for tests.
mcpApp :: ServerConfig -> IO Application
mcpApp = scottyApp . mcpScotty

-- | Run the server on the given host and port.
runServer :: ServerConfig -> String -> Int -> IO ()
runServer cfg host port =
  scottyOpts (Options 0 (setHost (fromString host) (setPort port defaultSettings))) (mcpScotty cfg)

mcpScotty :: ServerConfig -> ScottyM ()
mcpScotty cfg = do
  get "/" $ text $ TL.fromStrict $ T.unlines
    [ serverName <> " " <> serverVersion <> " -- https://github.com/jrclogic/smcdel-mcp"
    , "MCP endpoint: POST /mcp" ]
  post "/mcp" $ do
    mOrigin <- header "Origin"
    unless (maybe True (originAllowed . TL.toStrict) mOrigin) $ do
      status forbidden403
      text "Forbidden: Origin not allowed"
      finish
    reply <- liftIO . handleBytes cfg =<< body
    case reply of
      Nothing -> status accepted202
      Just v  -> json v
  get "/mcp" notAllowed
  delete "/mcp" notAllowed
  where
    notAllowed = do
      status methodNotAllowed405
      setHeader "Allow" "POST"
      text "Method not allowed: this server only supports POST on /mcp"

-- | Accept only local origins, to guard against DNS rebinding attacks as
-- the MCP specification demands. Requests without an Origin header are
-- allowed (non-browser clients).
originAllowed :: T.Text -> Bool
originAllowed origin = origin `elem` bases || any (`T.isPrefixOf` origin) prefixes
  where
    hosts = [ "localhost", "127.0.0.1", "[::1]" ]
    bases = [ scheme <> h | scheme <- [ "http://", "https://" ], h <- hosts ]
    prefixes = [ b <> suffix | b <- bases, suffix <- [ ":", "/" ] ]
