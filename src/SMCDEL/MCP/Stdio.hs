-- | The stdio transport of the Model Context Protocol: the client starts
-- the server as a subprocess and exchanges newline-delimited JSON-RPC
-- messages over its stdin and stdout.
--
-- Every non-empty input line is one message. Requests are answered with
-- one line of JSON, notifications get no reply. Nothing else may be
-- written to the output handle, so logging must go to stderr.
module SMCDEL.MCP.Stdio
  ( runStdio
  , runStdioWith
  ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Aeson (encode)
import System.IO

import SMCDEL.MCP.Protocol

-- | Serve on stdin and stdout until stdin is closed.
runStdio :: ServerConfig -> IO ()
runStdio cfg = runStdioWith cfg stdin stdout

-- | Serve on the given input and output handles until the input ends.
runStdioWith :: ServerConfig -> Handle -> Handle -> IO ()
runStdioWith cfg hIn hOut = do
  hSetBinaryMode hIn True
  hSetBinaryMode hOut True
  hSetBuffering hOut LineBuffering
  let loop = do
        eof <- hIsEOF hIn
        if eof then return () else do
          line <- BC.strip <$> BC.hGetLine hIn
          if BC.null line then loop else do
            reply <- handleBytes cfg (BLC.fromStrict line)
            mapM_ (\v -> BLC.hPutStrLn hOut (encode v) >> hFlush hOut) reply
            loop
  loop
