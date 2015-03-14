-- "Dao/Grammar/Debug.hs"  provides a parser that can be used to
-- visualize the evaluation of a Grammar.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see the URL:
-- <http://www.gnu.org/licenses/agpl.html>.

-- | A parser monad that can be used to debug parsers generated from 'Dao.Grammar.Grammar's. This
-- module is intended to imported qualified, most of the API function names are fairly terse.
module Dao.Grammar.Debug where

import           Dao.Count
import qualified Dao.Computer as Eval
import           Dao.Grammar
import qualified Dao.Interval as Iv
import           Dao.Lens
import           Dao.Predicate
import           Dao.PPrint
import           Dao.Text.Parser

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.State

import qualified Data.Text.Lazy as Lazy
import           Data.Typeable

----------------------------------------------------------------------------------------------------

-- | 'Dao.Grammar.MonadParser' function calls as a data structure.
data VisualParserFunction
  = Parser_look                           (Maybe LazyText)
  | Parser_look1                          (Maybe Char)
  | Parser_get1                           (Maybe Char)
  | Parser_string               LazyText  (Maybe LazyText)
  | Parser_count      Count (Iv.Set Char) (Maybe LazyText)
  | Parser_eof
  | Parser_munch            (Iv.Set Char) (Maybe LazyText)
  | Parser_munch1           (Iv.Set Char) (Maybe LazyText)
  | Parser_noMoreThan Count (Iv.Set Char) (Maybe LazyText)
  | Parser_satisfy          (Iv.Set Char) (Maybe Char)
  | Parser_char                     Char  (Maybe Char)
  | Parser_regex                   Regex  (Maybe LazyText)
  | Parser_pfail              LexerError
  | Parser_setPrecedence             Int
  | Parser_currentPrecedence         Int
  | Parser_resetPrecedence       Int Int
  | Parser_setTextPoint        TextPoint
  | Parser_gotTextPoint        TextPoint
  | Parser_unstring             LazyText
  deriving (Eq, Show, Typeable)

pPrintFuncResult :: (o -> [PPrint]) -> Maybe o -> [PPrint]
pPrintFuncResult prin = ([pShow "-->", pSpace]++) . maybe [pText "..."] prin

pPrintInputText :: Int -> LazyText -> PPrint
pPrintInputText maxlen o = pInline $ case take maxlen $ Lazy.unpack o of
  tx | length tx < 10 -> [pText "(", pText $ show tx, pText ")"]
  tx                  -> [pText "(", pText $ show tx, pSpace, pText "...", pText ")"]

instance PPrintable VisualParserFunction where
  pPrint fu = let result f o = pPrintFuncResult (return . f) o in case fu of
    Parser_look                    o ->
      pText "look"   : pSpace : result (pPrintInputText 10) o
    Parser_look1                   o ->
      pText "look1"  : pSpace : result pShow o
    Parser_get1                    o ->
      pText "get1"   : pSpace : result pShow o
    Parser_string              str o ->
      pText "string" : pSpace : pShow str : result pShow o
    Parser_count            ct set o ->
      [pText "count", pSpace, pShow ct, pSpace] ++ pPrintCharSet set ++ pSpace : result pShow o
    Parser_eof                       ->
      [pText "eof"]
    Parser_munch               set o ->
      [pText "munch" , pSpace] ++ pPrintCharSet set ++ pSpace : result pShow o
    Parser_munch1              set o ->
      [pText "munch1", pSpace] ++ pPrintCharSet set ++ pSpace : result pShow o
    Parser_noMoreThan       ct set o ->
      [pText "noMoreThan", pSpace, pShow ct, pSpace] ++ pPrintCharSet set ++ pSpace : result pShow o
    Parser_satisfy             set o ->
      [pText "satisfy ", pSpace] ++ pPrintCharSet set ++ pSpace : result pShow o
    Parser_char                ch  o ->
      [pText "char", pSpace, pShow ch, pSpace] ++ result pShow o
    Parser_regex               rx  o ->
      [pText "regex"] ++ pPrint rx ++ pSpace : result pShow o
    Parser_pfail               err   ->
      pText "pfail" : pSpace : pPrint err
    Parser_setPrecedence       pre   ->
      [pText "setPrecedence", pSpace, pShow pre]
    Parser_currentPrecedence   pre   ->
      [pText "currentPrecedence ", pSpace, pShow pre]
    Parser_resetPrecedence pr1 pr2   ->
      [pText "resetPrecedence", pSpace, pShow pr1, pSpace, pShow pr2]
    Parser_setTextPoint        pt    ->
      [pText "setTextPoint", pSpace, pShow pt]
    Parser_gotTextPoint        pt    ->
      [pText "gotTextPoint", pSpace, pShow pt]
    Parser_unstring            str   ->
      [pText "unstring", pSpace, pShow str]

data VisualParserLabel = VPTextLabel StrictText | VPTypeRep TypeRep deriving (Eq, Ord, Show)

instance PPrintable VisualParserLabel where
  pPrint o = case o of
    VPTextLabel t -> [pText t]
    VPTypeRep   t -> [pShow t]

data VisualParserState st
  = VisualParserState
    { visualParserState :: ParserState st
    , visualParserLog   :: [PPrint]
    }

-- | A 'Dao.Computer.InstructionT' type that is used to reify a 'Dao.Grammar.MonadParser'
-- computation as a data structure that can be stepped-through, it is wrapped in a 'VisualParser' to
-- create the 'VisualParser' monad.
type VisualParserInstruction st =
  Eval.InstructionT VisualParserLabel VisualParserFunction ParserError () () (VisualParserState st)

-- | This is a monadic parser that instantiates 'Dao.Grammar.MonadParser' such that it can be
-- stepped through as it computes, allowing you can visualise parsing of the input string.
newtype VisualParser st m o = VisualParser { unwrapVisualParser :: VisualParserInstruction st m o }

instance Functor m => Functor (VisualParser st m) where
  fmap f (VisualParser o) = VisualParser (fmap f o)

instance Monad m => Monad (VisualParser st m) where
  (VisualParser o) >>= f = VisualParser $ o >>= unwrapVisualParser . f
  return                 = VisualParser . return
  fail                   = VisualParser . fail

instance (Functor m, Monad m) => Applicative (VisualParser st m) where
  pure = return
  (<*>) = ap

instance Monad m => MonadPlus (VisualParser st m) where
  mzero = VisualParser mzero
  mplus (VisualParser a) (VisualParser b) = VisualParser $ mplus a b

instance (Functor m, Monad m) => Alternative (VisualParser st m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadState (VisualParserState st) (VisualParser st m) where
  state = VisualParser . state

instance Monad m => MonadError ParserError (VisualParser st m) where
  throwError = VisualParser . throwError
  catchError (VisualParser try) catch = VisualParser $ catchError try (unwrapVisualParser . catch)

instance MonadTrans (VisualParser st) where { lift = VisualParser . lift }

instance MonadIO (VisualParser st IO) where { liftIO = VisualParser . liftIO }

-- This function takes a 'Dao.Text.Parser.Parser' from the "Dao.Text.Parser" module and runs it in
-- the 'VisualParser'. This kind of defeats the purpose of using a 'VisualParser', however it is
-- useful for implementing the primitive parsing functions necessary to instantiate 'VisualParser'
-- into the 'Dao.Grammar.MonadParser' class. The instantiation is done such that every call to
-- '_runInner' runs only one 'Dao.Grammar.MonadParser' function to update the
-- 'Dao.Text.Parser.ParserState'. Once the 'Dao.Text.Parser.ParserState' has been updated, it is
-- placed back into the 'VisualParser's 'Dao.Computer.stateTable'.
-- 
-- When 'Dao.Computer.stepT'-ing through a 'VisualParser' computation, the 'Dao.Computer.Eval' and
-- 'Dao.Computer.Eval' symbols will appear to denote the start and end of the inner
-- 'Dao.Text.Parser.Parser' function.
_runInner :: Monad m => Parser st o -> VisualParserInstruction st m o
_runInner par = do
  st <- get
  let (result, parst) = runParser par $ visualParserState st
  put $ st{ visualParserState=parst }
  case result of
    OK      o -> return o
    PFail err -> throwError err
    Backtrack -> mzero

_getsVP :: Monad m => (ParserState st -> o) -> VisualParserInstruction st m o
_getsVP f = gets (f . visualParserState)

_modifyVP :: Monad m => (ParserState st -> ParserState st) -> VisualParserInstruction st m ()
_modifyVP f = modify $ \st -> st{ visualParserState = f $ visualParserState st }

-- This function calls '_runInner' in between a 'Dao.Computer.Eval' and 'Dao.Computer.Eval'
-- constructor evaluation. These constructors are called with the provided 'VisualParserFunction'.
-- When 'Dao.Computer.stepT'-ing through a 'VisualParser' computation, the 'Dao.Computer.Eval'
-- symbols will appear to denote the start and end of the inner 'Dao.Text.Parser.Parser' function.
_logPar :: Monad m => VisualParserFunction -> Parser st o -> VisualParser st m o
_logPar fu par = VisualParser $ Eval.Eval fu $ _runInner par

-- This function calls '_runInner' with a constructor for a 'VisualParserFunction'. The
-- 'Dao.Computer.Eval' constructor is evaluated parser function is evaluated, then the inner
-- 'Dao.Text.Parser.Parser' is evaluated.
_evalPar :: Monad m => (Maybe o -> VisualParserFunction) -> Parser st o -> VisualParser st m o
_evalPar fu par = VisualParser $
  Eval.Eval (fu Nothing) $ _runInner par >>= \o -> Eval.Eval (fu $ Just o) $ return o

-- Tries to evaluate a parser with modified precedence values. The previous precedence value is
-- reset even if parser evaluates to 'Control.Applicative.empty', and the apporpriate verbose log
-- messages are produced.
_precCtx
  :: (Functor m, Monad m)
  => Int -> Int -> VisualParserInstruction st m o -> VisualParserInstruction st m o
_precCtx before after o =
  let done d = Eval.Eval (Parser_resetPrecedence after before) $
        _modifyVP (\st -> st{ precedence=before }) >> d
  in  (o >>= done . return) <|> done mzero

instance Monad m => MonadParser LazyText (VisualParser st m) where
  look           = _evalPar  Parser_look             look
  look1          = _evalPar  Parser_look1            look1
  get1           = _evalPar  Parser_get1             get1
  string       s = _evalPar (Parser_string       s) (string       s)
  count      c s = _evalPar (Parser_count      c s) (count      c s)
  eof            = _logPar   Parser_eof              eof         
  munch        s = _evalPar (Parser_munch        s) (munch        s)
  munch1       s = _evalPar (Parser_munch1       s) (munch1       s)
  noMoreThan c s = _evalPar (Parser_noMoreThan c s) (noMoreThan c s)
  satisfy      s = _evalPar (Parser_satisfy      s) (satisfy      s)
  char         c = _evalPar (Parser_char         c) (char         c)
  regex        r = _evalPar (Parser_regex        r) (regex        r)
  pfail        e = _logPar  (Parser_pfail        e) (pfail        e)

instance (Functor m, Monad m) => MonadPrecedenceParser LazyText (VisualParser st m) where
  prec i o = VisualParser $ Eval.Eval (Parser_setPrecedence i) $ _getsVP precedence >>= \pre ->
    Eval.Eval (Parser_currentPrecedence pre) $ if pre<i then mzero else _precCtx i pre (unwrapVisualParser o)
  step   o = VisualParser $ _getsVP precedence >>= \pre -> let i = pre+1 in
    Eval.Eval (Parser_setPrecedence i) $ _precCtx i pre (unwrapVisualParser o)
  reset  o = VisualParser $ _getsVP precedence >>= \pre ->
    Eval.Eval (Parser_setPrecedence 0) $ _precCtx 0 pre (unwrapVisualParser o)

instance Monad m => MonadSourceCodeParser LazyText (VisualParser st m) where
  getTextPoint    = VisualParser $
    gets (textPoint . visualParserState) >>= \pt -> Eval.Eval (Parser_gotTextPoint pt) $ return pt
  setTextPoint pt = VisualParser $
    Eval.Eval (Parser_setTextPoint pt) $ _modifyVP (\st -> st{ textPoint=pt })

instance Monad m => MonadBacktrackingParser LazyText (VisualParser st m) where
  unstring s = _logPar (Parser_unstring s) (unstring s)

visualParserAppendLog :: Monad m => [PPrint] -> VisualParser st m ()
visualParserAppendLog txt = modify $ \st ->
  st{ visualParserLog = visualParserLog st ++ PNewLine : txt }

visualParserCutLog :: Monad m => VisualParser st m [PPrint]
visualParserCutLog = state $ \st -> (visualParserLog st, st{ visualParserLog=[] })

----------------------------------------------------------------------------------------------------

-- | Pretty-print an 'Dao.Computer.ComputerT' containing a 'VisualParserInstruction' and a having a
-- 'VisualParserState' value stored in the 'Dao.Computer.ComputerT's 'Dao.Computer.stateTable'.
type VisualParserPrinter st =
  Eval.PPrintComputer VisualParserLabel VisualParserFunction ParserError () () (VisualParserState st)

-- | This is the default 'Dao.Computer.PPrintComputer' data type containing the methods used for
-- printing a 'VisualParserInstruction' within a 'VisualParser' monad.
visualParserPPrinter :: PPrintable st => (o -> [PPrint]) -> VisualParserPrinter st o
visualParserPPrinter result =
  Eval.PPrintComputer
  { Eval.pprintLabel            = pPrint
  , Eval.pprintFunction         = pPrint
  , Eval.pprintError            = pPrint
  , Eval.pprintReaderEnv        = const []
  , Eval.pprintWriterStream     = const []
  , Eval.pprintStateTable       = pPrint
  , Eval.pprintData             = \o -> [pText "return", pSpace, pInline $ result o]
  , Eval.pprintInstructionDepth = 2
  }

instance PPrintable st => PPrintable (VisualParserState st) where
  pPrint st = let par = visualParserState st in concat $
    [ [pText "precedence =", pSpace, pShow (precedence par)]
    , [pText "textPoint =", pSpace, pShow (textPoint par)]
    , [pText "charCount =", pSpace, pShow (charCount par)]
    , [pText "inputString =", pSpace] ++ do
        let s = par & inputString
            t = Lazy.take 10 s
        [pShow t] <|> (guard (Lazy.length t < Lazy.length s) >> [pText "..."])
    , let msg = pPrint (par & userState) in  
        if null msg then [] else [pText "userState =", pNewLine, pIndent msg]
    ]

-- | This function takes a 'VisualParserPrinter' and uses it to print a 'VisualParser' monadic
-- computation. You can use 'visualParserPPrinter' as the first parameter to this function.
visualParserMonadPPrinter :: VisualParserPrinter st o -> VisualParser st m o -> [PPrint]
visualParserMonadPPrinter pp (VisualParser instr) = Eval.pprintInstruction pp instr

----------------------------------------------------------------------------------------------------

-- | This debugger data type contains the stateful information necessary to step through a
-- 'VisualParser' monadic computation. Initialize this data type using 'initGrammarDebugger', then
-- evaluate each debug step using 'grammarDebugStep'
data GrammarDebugger typ st m
  = GrammarDebugger
    { grammarDebugPrinter      :: VisualParserPrinter st typ
    , grammarDebugParserState  :: VisualParserState  st
    , grammarDebugVisualParser :: VisualParser st m typ
    }

-- | Prints the internal state of the given 'GrammarDebugger'. This function uses the
-- 'grammarDebugPrinter' stored within the 'GrammarDebugger' to produce the lines of
-- 'Dao.Text.LazyText'.
printGrammarDebugger :: GrammarDebugger typ st m -> [PPrint]
printGrammarDebugger dbg = concat $
  [ visualParserLog $ grammarDebugParserState dbg
  , visualParserMonadPPrinter (grammarDebugPrinter dbg) (grammarDebugVisualParser dbg)
  ]

-- | Initialize a 'GrammarDebugger'. This will allow you to visualize the evaluation of a
-- 'Dao.Grammar.MonadParser' as it parses an input string. Use 'grammarDebugStep' to view each step
-- of the parser evaluation.
initGrammarDebugger
  :: (Functor m, Monad m)
  => VisualParserPrinter st typ -- ^ pretty printer for the 'Dao.Computer.ComputerT'
  -> (GrammarSignature (VisualParser st m) typ -> [PPrint]) -- ^ pretty printer for 'Dao.Grammar.GrammarSignature's
  -> Grammar (VisualParser st m) typ -- ^ the grammar defining the parser you want to test.
  -> st -- ^ the initializing cusomtizable state for the 'Dao.Text.Parser.ParserState'.
  -> LazyText -- ^ the input string to parser.
  -> GrammarDebugger typ st m
initGrammarDebugger prin prinSig gram st instr =
  GrammarDebugger
  { grammarDebugPrinter      = prin
  , grammarDebugParserState  =
      VisualParserState
      { visualParserState = on (parserState st) [inputString <~ instr]
      , visualParserLog   = []
      }
  , grammarDebugVisualParser = grammarToVerboseParser (visualParserAppendLog . prinSig) gram
  }

-- | This function could as well be defined as:
--
-- > grammarDebugStep :: 'GrammarDebugger' typ st m -> m
--
-- (['Dao.Text.LazyText'], 'GrammarDebugger' typ st m) But for convenience it is wrapped in a
-- 'Control.Monad.State.StateT' monad, where you can just use 'Control.Monad.State.runStateT' once
-- you have defined the 'GrammarDebuggger' using 'initGrammarDebugger'.
--
-- The 'initGrammarDebugger' function always uses 'Dao.Grammar.grammarToVerboseParser' to
-- initialized the 'VisualParser' monad (stored in the 'grammarDebugVisualParser' field), and since
-- 'grammarToVerboseParser' prints the 'Dao.Grammar.GrammarSignature' of every node of the
-- 'Dao.Grammar.Grammar' as it is converted to the 'Dao.Grammar.MonadParser', evaluation steps
-- may result in that 'Dao.Grammar.GrammarSignature' being printed and the lines of
-- 'Dao.Text.LazyText' being returned.
grammarDebugStep :: Monad m => StateT (GrammarDebugger typ st m) m [PPrint]
grammarDebugStep = StateT $ \gdbgst -> do -- <- construct a 'StateT' with 'GrammarDebugger' as the state.
  comp <- Eval.stepT $
    Eval.Computer
    { Eval.registers =
        Eval.ComputerState
        { Eval.readerEnv  = (), Eval.writerStream = ()
        , Eval.stateTable = grammarDebugParserState gdbgst
        }
    , Eval.programCounter = unwrapVisualParser $ grammarDebugVisualParser gdbgst
    }
  let st = Eval.stateTable $ Eval.registers comp
  return $ (,) (visualParserLog st) $
    gdbgst
    { grammarDebugParserState  = st{ visualParserLog=[] }
    , grammarDebugVisualParser = VisualParser $ Eval.programCounter comp
    }

