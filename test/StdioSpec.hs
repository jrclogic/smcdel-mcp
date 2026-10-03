{-# LANGUAGE OverloadedStrings #-}
module StdioSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import System.IO
import System.Process (createPipe)
import Test.Hspec

import Helpers
import SMCDEL.MCP.Protocol
import SMCDEL.MCP.Stdio

spec :: Spec
spec = beforeAll (newServerConfig 30) $
  describe "runStdioWith" $ do
    it "answers requests line by line and ignores notifications and blank lines" $ \cfg -> do
      replies <- exchange cfg
        [ encode (rpc "initialize" (object [ "protocolVersion" .= defaultProtocolVersion ]))
        , encode (object [ "jsonrpc" .= ("2.0" :: T.Text), "method" .= ("notifications/initialized" :: T.Text) ])
        , ""
        , encode (rpc "tools/call" (object
            [ "name" .= ("smcdel_valid" :: T.Text)
            , "arguments" .= object [ "structure" .= minimal, "formula" .= ("[! 1] alice knows that (bob knows that 1)" :: T.Text) ] ]))
        ]
      length replies `shouldBe` 2
      case replies of
        [ini, res] -> do
          (asText . path ["result","protocolVersion"] =<< ini) `shouldBe` Just defaultProtocolVersion
          (asBool . path ["result","structuredContent","holds"] =<< res) `shouldBe` Just True
        _ -> expectationFailure "expected two replies"
    it "answers invalid JSON with a parse error and keeps going" $ \cfg -> do
      replies <- exchange cfg [ "{", encode (rpc "ping" (object [])) ]
      map (\r -> asInt (path ["error","code"] =<< r)) replies `shouldBe` [Just (-32700), Nothing]
      (path ["result"] =<< (replies !! 1)) `shouldBe` Just (object [])

-- | Feed the lines to a stdio server, close its input and collect all
-- output lines, each of which must be valid JSON.
exchange :: ServerConfig -> [BLC.ByteString] -> IO [Maybe Value]
exchange cfg inputs = do
  (serverIn, clientOut) <- createPipe
  (clientIn, serverOut) <- createPipe
  done <- newEmptyMVar
  _ <- forkIO $ runStdioWith cfg serverIn serverOut `finally` (hClose serverOut >> putMVar done ())
  hSetBinaryMode clientOut True
  mapM_ (BLC.hPutStrLn clientOut) inputs
  hClose clientOut
  out <- BC.hGetContents clientIn
  takeMVar done
  return $ map (decodeStrict :: BC.ByteString -> Maybe Value) (BC.lines out)
