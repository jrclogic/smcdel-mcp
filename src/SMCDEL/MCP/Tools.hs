{-# LANGUAGE OverloadedStrings #-}
-- | The MCP tools offered by this server: one for each of the SMCDEL
-- queries TRUE?, VALID? and WHERE?.
module SMCDEL.MCP.Tools
  ( Tool(..)
  , ToolResult(..)
  , ToolConfig(..)
  , newToolConfig
  , tools
  , toolDefinition
  , toolResultValue
  , syntaxPrimer
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.List (intercalate)
import qualified Data.Text as T

import SMCDEL.Language (Form)
import SMCDEL.MCP.Check

-- | Settings shared by all tools.
data ToolConfig = ToolConfig
  { cfgTimeoutSeconds :: Int
  , cfgLock           :: MVar ()  -- ^ serializes all BDD work (HasCacBDD has one global manager)
  }

newToolConfig :: Int -> IO ToolConfig
newToolConfig secs = ToolConfig secs <$> newMVar ()

data ToolResult = ToolResult
  { resultText       :: T.Text
  , resultStructured :: Maybe Value
  , resultIsError    :: Bool
  }

data Tool = Tool
  { toolName         :: T.Text
  , toolTitle        :: T.Text
  , toolDescription  :: T.Text
  , toolInputSchema  :: Value
  , toolOutputSchema :: Value
  , toolRun          :: Value -> IO (Either T.Text ToolResult)
    -- ^ 'Left' means the arguments did not match the schema (a protocol error);
    -- problems with the SMCDEL input are reported inside the 'ToolResult'.
  }

-- | The tool as listed in the reply to @tools/list@.
toolDefinition :: Tool -> Value
toolDefinition t = object
  [ "name"         .= toolName t
  , "title"        .= toolTitle t
  , "description"  .= toolDescription t
  , "inputSchema"  .= toolInputSchema t
  , "outputSchema" .= toolOutputSchema t
  , "annotations"  .= object
      [ "readOnlyHint"    .= True
      , "destructiveHint" .= False
      , "idempotentHint"  .= True
      , "openWorldHint"   .= False
      ]
  ]

-- | The result as returned by @tools/call@.
toolResultValue :: ToolResult -> Value
toolResultValue r = object $
  [ "content" .= [ object [ "type" .= ("text" :: T.Text), "text" .= resultText r ] ]
  , "isError" .= resultIsError r
  ] ++ maybe [] (\s -> [ "structuredContent" .= s ]) (resultStructured r)

tools :: ToolConfig -> [Tool]
tools cfg = [ trueTool cfg, validTool cfg, whereTool cfg ]

-- * Shared pieces

errorResult :: CheckError -> ToolResult
errorResult e = ToolResult (T.pack (renderError e)) Nothing True

okResult :: String -> Value -> ToolResult
okResult txt structured = ToolResult (T.pack txt) (Just structured) False

-- | Parse the arguments, run the check under the lock and timeout, and
-- render the result.
runTool :: NFData a
  => ToolConfig
  -> (Value -> Parser args)
  -> (args -> Either CheckError a)
  -> (args -> a -> ToolResult)
  -> Value -> IO (Either T.Text ToolResult)
runTool cfg parseArgs check render argsValue =
  case parseEither parseArgs argsValue of
    Left err -> return (Left (T.pack err))
    Right args -> do
      outcome <- withMVar (cfgLock cfg) $ \_ ->
        runGuarded (cfgTimeoutSeconds cfg) (check args)
      return . Right $ either errorResult (render args) outcome

structureProperty :: (T.Text, Value)
structureProperty = ("structure", object
  [ "type" .= ("string" :: T.Text)
  , "description" .= ("The knowledge structure in SMCDEL syntax: a VARS line, a LAW line and an OBS block. No queries." :: T.Text)
  ])

formulaProperty :: (T.Text, Value)
formulaProperty = ("formula", object
  [ "type" .= ("string" :: T.Text)
  , "description" .= ("The formula to check, in SMCDEL syntax." :: T.Text)
  ])

schema :: [(T.Text, Value)] -> [T.Text] -> Value
schema props required = object
  [ "type" .= ("object" :: T.Text)
  , "properties" .= object [ (fromText k, v) | (k,v) <- props ]
  , "required" .= required
  ]

boolProp :: T.Text -> (T.Text, Value)
boolProp k = (k, object [ "type" .= ("boolean" :: T.Text) ])

formulaOutputs :: [(T.Text, Value)]
formulaOutputs =
  [ ("formula", object [ "type" .= ("string" :: T.Text), "description" .= ("The formula as given in the request." :: T.Text) ])
  , ("parsed", object [ "type" .= ("string" :: T.Text), "description" .= ("The formula as SMCDEL parsed it, in its own notation (K, Kw, Ck, Dk, [! ])." :: T.Text) ])
  ]

formulaFields :: String -> String -> [Pair]
formulaFields given parsedForm = [ "formula" .= given, "parsed" .= parsedForm ]

intArray :: Value
intArray = object [ "type" .= ("array" :: T.Text), "items" .= object [ "type" .= ("integer" :: T.Text) ] ]

showState :: [Int] -> String
showState s = "{" ++ intercalate "," (map show s) ++ "}"

-- | Arguments as raw strings; parsing of the SMCDEL syntax happens inside
-- the check so that syntax errors become tool errors rather than
-- protocol errors.
data RawArgs = RawArgs
  { rawStructure :: String
  , rawFormula   :: String
  , rawState     :: Maybe [Int]
  , rawMaxStates :: Maybe Int
  }

parseRawArgs :: Value -> Parser RawArgs
parseRawArgs = withObject "arguments" $ \o -> RawArgs
  <$> o .: "structure"
  <*> o .: "formula"
  <*> o .:? "state"
  <*> o .:? "maxStates"

parsed :: RawArgs -> Either CheckError (Structure, Form)
parsed args = (,) <$> parseStructure (rawStructure args) <*> parseFormula (rawFormula args)

-- * TRUE?

trueTool :: ToolConfig -> Tool
trueTool cfg = Tool
  { toolName = "smcdel_true"
  , toolTitle = "SMCDEL TRUE? query"
  , toolDescription = T.unlines
      [ "Check whether a formula is true at a given state of a knowledge structure (the TRUE? command of SMCDEL)."
      , "The state is the set of atomic propositions that are true; it must satisfy the LAW of the structure."
      , "Example: structure \"VARS 1,2,3\\nLAW Top\\nOBS alice: 2,3\\n bob: 1,3\\n carol: 1,2\", state [1,2,3], formula \"alice knows that (2 & 3) & ~ alice knows whether 1\" gives true."
      ]
  , toolInputSchema = schema
      [ structureProperty
      , ("state", object
          [ "type" .= ("array" :: T.Text)
          , "items" .= object [ "type" .= ("integer" :: T.Text) ]
          , "description" .= ("The actual state: the list of atomic propositions (from VARS) that are true." :: T.Text)
          ])
      , formulaProperty
      ] [ "structure", "state", "formula" ]
  , toolOutputSchema = schema
      ([ boolProp "holds", ("state", intArray) ] ++ formulaOutputs) [ "holds", "state", "formula", "parsed" ]
  , toolRun = runTool cfg parseArgs check render
  }
  where
    parseArgs v = do
      args <- parseRawArgs v
      case rawState args of
        Nothing -> fail "missing required argument 'state'"
        Just s  -> return (args, s)
    check (args, s) = do
      (str, f) <- parsed args
      checkTrue str s f
    render (args, _) r = okResult
      ("Is " ++ rawFormula args ++ " true at " ++ showState (trState r) ++ "? " ++ show (trHolds r))
      (object $ [ "holds" .= trHolds r, "state" .= trState r ] ++ formulaFields (rawFormula args) (trFormula r))

-- * VALID?

validTool :: ToolConfig -> Tool
validTool cfg = Tool
  { toolName = "smcdel_valid"
  , toolTitle = "SMCDEL VALID? query"
  , toolDescription = T.unlines
      [ "Check whether a formula is valid on a knowledge structure, i.e. true at all its states (the VALID? command of SMCDEL)."
      , "Example: structure \"VARS 1,2\\nLAW OR (1,2)\\nOBS alice: 1\\n bob:\\n carol: 2\", formula \"[! 1] alice knows that (bob knows that 1)\" gives true."
      ]
  , toolInputSchema = schema [ structureProperty, formulaProperty ] [ "structure", "formula" ]
  , toolOutputSchema = schema (boolProp "holds" : formulaOutputs) [ "holds", "formula", "parsed" ]
  , toolRun = runTool cfg parseRawArgs check render
  }
  where
    check args = do
      (str, f) <- parsed args
      checkValid str f
    render args r = okResult
      ("Is " ++ rawFormula args ++ " valid on the structure? " ++ show (vrHolds r))
      (object $ ("holds" .= vrHolds r) : formulaFields (rawFormula args) (vrFormula r))

-- * WHERE?

defaultMaxStates :: Int
defaultMaxStates = 100

whereTool :: ToolConfig -> Tool
whereTool cfg = Tool
  { toolName = "smcdel_where"
  , toolTitle = "SMCDEL WHERE? query"
  , toolDescription = T.unlines
      [ "List the states of a knowledge structure at which a formula is true (the WHERE? command of SMCDEL)."
      , "Each state is given as the list of true atomic propositions. The exact count is always returned; the list is truncated to maxStates (default " <> T.pack (show defaultMaxStates) <> ")."
      , "Example: with the three muddy children structure, formula \"Top\" lists all 8 states."
      ]
  , toolInputSchema = schema
      [ structureProperty
      , formulaProperty
      , ("maxStates", object
          [ "type" .= ("integer" :: T.Text)
          , "minimum" .= (0 :: Int)
          , "description" .= ("Maximum number of states to list (default " <> T.pack (show defaultMaxStates) <> ")." :: T.Text)
          ])
      ] [ "structure", "formula" ]
  , toolOutputSchema = schema
      ([ ("states", object [ "type" .= ("array" :: T.Text), "items" .= intArray ])
      , ("count", object [ "type" .= ("integer" :: T.Text), "description" .= ("Exact number of states where the formula holds." :: T.Text) ])
      , boolProp "truncated"
      ] ++ formulaOutputs) [ "states", "count", "truncated", "formula", "parsed" ]
  , toolRun = runTool cfg parseRawArgs check render
  }
  where
    check args = do
      (str, f) <- parsed args
      checkWhere (maybe defaultMaxStates (max 0) (rawMaxStates args)) str f
    render args r = okResult
      (unlines $
        [ "At which states is " ++ rawFormula args ++ " true?"
        , "Number of states: " ++ show (wrCount r) ++
            (if wrTruncated r then " (only the first " ++ show (length (wrStates r)) ++ " are listed)" else "")
        ] ++ map showState (wrStates r))
      (object $ [ "states" .= wrStates r, "count" .= wrCount r, "truncated" .= wrTruncated r ] ++ formulaFields (rawFormula args) (wrFormula r))

-- | A short description of the SMCDEL input language, sent to clients in
-- the @instructions@ field of the @initialize@ response.
syntaxPrimer :: T.Text
syntaxPrimer = T.unlines
  [ "SMCDEL is a symbolic model checker for Dynamic Epistemic Logic (S5 knowledge structures)."
  , "Every tool takes a knowledge structure in SMCDEL syntax, for example:"
  , ""
  , "  VARS 1,2,3        -- atomic propositions are positive integers"
  , "  LAW  Top          -- Boolean formula over VARS; the states are its satisfying assignments"
  , "  OBS  alice: 2,3   -- for each agent, the propositions it observes"
  , "       bob:   1,3"
  , "       carol: 1,2"
  , ""
  , "Formula syntax (f, g are formulas; i, j are agent names; n is an atomic proposition):"
  , "  Top, Bot, n, ~f, f & g, f | g, f -> g, f iff g,"
  , "  AND (f, g, ...), OR (f, g, ...), XOR (f, g, ...), ONEOF (f, g, ...),"
  , "  Forall 1,2 f, Exists 1,2 f  (Boolean quantification),"
  , "  i knows that f, i knows whether f  (also written K i f and Kw i f),"
  , "  i,j comknow that f, i,j comknow whether f  (common knowledge),"
  , "  i,j distknow that f, i,j distknow whether f  (distributed knowledge),"
  , "  [! f] g  (after the public announcement of f, g holds),"
  , "  <! f> g  (f is true and after announcing it, g holds),"
  , "  [?! f] g and <?! f> g  (announcing whether f)."
  , "Agent names are alphanumeric and start with a letter. Comments start with --."
  , "Use parentheses generously: & and | do not associate, so write (a & b) & c."
  , "A state is the list of true atomic propositions, e.g. [1,3]; it must satisfy the LAW."
  ]
