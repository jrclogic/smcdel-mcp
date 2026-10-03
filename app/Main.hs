module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, hSetBuffering, stderr, stdout, BufferMode(LineBuffering))
import Text.Read (readMaybe)

import SMCDEL.MCP.Protocol
import SMCDEL.MCP.Server
import SMCDEL.MCP.Stdio

main :: IO ()
main = do
  args <- getArgs
  secs <- fromMaybe 30 . (readMaybe =<<) <$> lookupEnv "SMCDEL_TIMEOUT"
  case args of
    []          -> newServerConfig secs >>= runHttp secs
    ["--stdio"] -> newServerConfig secs >>= runStdio
    ["--help"]  -> putStr usage >> exitSuccess
    _           -> hPutStrLn stderr usage >> exitFailure

runHttp :: Int -> ServerConfig -> IO ()
runHttp secs cfg = do
  hSetBuffering stdout LineBuffering
  port <- fromMaybe 3000 . (readMaybe =<<) <$> lookupEnv "PORT"
  host <- fromMaybe "127.0.0.1" <$> lookupEnv "HOST"
  putStrLn $ T.unpack serverName ++ " " ++ T.unpack serverVersion ++ " -- https://github.com/jrclogic/smcdel-mcp"
  putStrLn $ "MCP endpoint: http://" ++ host ++ ":" ++ show port ++ "/mcp"
  putStrLn $ "Timeout per query: " ++ show secs ++ " seconds"
  runServer cfg host port

usage :: String
usage = unlines
  [ "Usage: smcdel-mcp [--stdio]"
  , ""
  , "Without arguments, serve MCP over Streamable HTTP (see PORT and HOST)."
  , "With --stdio, speak MCP over stdin and stdout, for clients that start"
  , "the server themselves."
  ]
