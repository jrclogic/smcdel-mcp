{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | Glue between the MCP tools and the SMCDEL library.
--
-- This mirrors what the SMCDEL web interface does in its @check@ endpoint:
-- lex and parse the input, run 'sanityCheck', build a 'KnowStruct' and
-- answer TRUE?, VALID? and WHERE? queries with the symbolic (BDD based)
-- functions from "SMCDEL.Symbolic.S5".
module SMCDEL.MCP.Check
  ( Structure(..)
  , Part(..)
  , CheckError(..)
  , renderError
  , parseStructure
  , parseFormula
  , TrueResult(..)
  , ValidResult(..)
  , WhereResult(..)
  , checkTrue
  , checkValid
  , checkWhere
  , runGuarded
  ) where

import Control.Arrow (second)
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, evaluate, try)
import Data.HasCacBDD (con, evaluateFun, satCountWith)
import Data.List (intercalate, nub, sort)
import GHC.Generics (Generic)
import System.Timeout (timeout)

import SMCDEL.Internal.Lex (alexScanTokensSafe)
import SMCDEL.Internal.Parse
import SMCDEL.Language
import SMCDEL.Symbolic.S5

-- | A knowledge structure as given by the user: the VARS, LAW and OBS block.
data Structure = Structure
  { structVocab :: [Int]
  , structLaw   :: Form
  , structObs   :: [(String,[Int])]
  } deriving (Eq, Show)

-- | Which tool argument an error refers to.
data Part = StructureArg | FormulaArg deriving (Eq, Show, Generic, NFData)

data CheckError
  = LexError Part (Int,Int)
  | ParseError Part (Int,Int)
  | StructureHasQueries
  | SanityError [String]
  | NotAState [Int]
  | RuntimeError String
  | TimedOut Int
  deriving (Eq, Show, Generic, NFData)

partName :: Part -> String
partName StructureArg = "the 'structure' argument"
partName FormulaArg   = "the 'formula' argument"

renderError :: CheckError -> String
renderError (LexError p (l,c)) =
  "Lex error in " ++ partName p ++ " at line " ++ show l ++ ", column " ++ show c ++ ": unexpected character."
renderError (ParseError p (l,c)) =
  "Parse error in " ++ partName p ++ " at line " ++ show l ++ ", column " ++ show c ++ "."
renderError StructureHasQueries =
  "The 'structure' argument must only contain VARS, LAW and OBS. Put the query formula in the 'formula' argument instead of using TRUE?, VALID?, WHERE? or UPDATE!."
renderError (SanityError msgs) =
  "Sanity check failed: " ++ intercalate " " msgs
renderError (NotAState s) =
  "The state " ++ showState s ++ " does not satisfy the LAW, so it is not a state of the given structure."
renderError (RuntimeError msg) =
  "SMCDEL error: " ++ msg
renderError (TimedOut secs) =
  "Model checking did not finish within " ++ show secs ++ " seconds."

showState :: [Int] -> String
showState s = "{" ++ intercalate "," (map show s) ++ "}"

-- | Lex and parse a VARS/LAW/OBS block. Queries are not allowed here.
parseStructure :: String -> Either CheckError Structure
parseStructure input = do
  tokens <- either (Left . LexError StructureArg) Right (alexScanTokensSafe input)
  ci <- either (Left . ParseError StructureArg) Right (parse tokens :: Either (Int,Int) CheckInput)
  case ci of
    CheckInput vocab law obs [] -> Right (Structure vocab law obs)
    CheckInput _ _ _ (_:_)      -> Left StructureHasQueries

-- | Lex and parse a single formula.
parseFormula :: String -> Either CheckError Form
parseFormula input = do
  tokens <- either (Left . LexError FormulaArg) Right (alexScanTokensSafe input)
  either (Left . ParseError FormulaArg) Right (parse tokens :: Either (Int,Int) Form)

-- | Run the SMCDEL sanity checks for a structure together with one job.
sanity :: Structure -> Job -> Either CheckError ()
sanity (Structure vocab law obs) job =
  case sanityCheck (CheckInput vocab law obs [job]) of
    []   -> Right ()
    msgs -> Left (SanityError msgs)

knowStruct :: Structure -> KnowStruct
knowStruct (Structure vocab law obs) =
  KnS (map P vocab) (boolBddOf law) (map (second (map P)) obs)

data TrueResult = TrueResult
  { trState   :: [Int]
  , trFormula :: String
  , trHolds   :: Bool
  } deriving (Eq, Show, Generic, NFData)

data ValidResult = ValidResult
  { vrFormula :: String
  , vrHolds   :: Bool
  } deriving (Eq, Show, Generic, NFData)

data WhereResult = WhereResult
  { wrFormula   :: String
  , wrStates    :: [[Int]]  -- ^ the (possibly truncated) list of states
  , wrCount     :: Int      -- ^ the exact number of states where the formula holds
  , wrTruncated :: Bool
  } deriving (Eq, Show, Generic, NFData)

-- | TRUE? query: is the formula true at the given state of the structure?
-- Unlike the web interface we refuse states that do not satisfy the LAW.
checkTrue :: Structure -> [Int] -> Form -> Either CheckError TrueResult
checkTrue str rawState f = do
  let s = sort (nub rawState)
  sanity str (TrueQ s f)
  let kns@(KnS _ lawbdd _) = knowStruct str
  if evaluateFun lawbdd (`elem` s)
    then Right (TrueResult s (ppForm f) (evalViaBdd (kns, map P s) f))
    else Left (NotAState s)

-- | VALID? query: is the formula true at all states of the structure?
checkValid :: Structure -> Form -> Either CheckError ValidResult
checkValid str f = do
  sanity str (ValidQ f)
  Right (ValidResult (ppForm f) (validViaBdd (knowStruct str) f))

-- | WHERE? query: at which states is the formula true?
-- At most @maxStates@ states are listed; the count is always exact.
checkWhere :: Int -> Structure -> Form -> Either CheckError WhereResult
checkWhere maxStates str f = do
  sanity str (WhereQ f)
  let kns@(KnS props lawbdd _) = knowStruct str
      count = satCountWith (map fromEnum props) (con lawbdd (bddOf kns f))
      states = map (map fromEnum) (take maxStates (whereViaBdd kns f))
  Right (WhereResult (ppForm f) states count (count > length states))

-- | Fully evaluate a check result with a timeout, converting runtime
-- errors thrown by SMCDEL (for example a non-Boolean LAW) into 'CheckError's.
runGuarded :: NFData a => Int -> Either CheckError a -> IO (Either CheckError a)
runGuarded secs computation = do
  outcome <- timeout (secs * 1000000) (try (evaluate (force computation)))
  return $ case outcome of
    Nothing          -> Left (TimedOut secs)
    Just (Left e)    -> Left (RuntimeError (show (e :: SomeException)))
    Just (Right res) -> res
