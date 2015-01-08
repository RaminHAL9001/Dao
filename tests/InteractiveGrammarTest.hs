module Main where

import qualified Dao.Computer as Eval
import           Dao.Grammar
import           Dao.Grammar.Debug
import           Dao.Text.Parser
import           Dao.Text.Builder

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State

--import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as Lazy

import           System.Environment

runSimple :: Show o => Grammar (Parser ()) o -> LazyText -> IO ()
runSimple gram str = print $ fst $ runParser (grammarToParser $ gram) $
  (parserState ()){inputString=str}

runVerbose :: forall o . Show o => Grammar (VisualParser () IO) o -> LazyText -> IO ()
runVerbose gram ax = loop debugger where
  printo    :: Show o => o -> [LazyText]
  printo    = return . toLazyText . show
  vpPrinter :: Show o => VisualParserPrinter () o
  vpPrinter = defaultVisualParserPrinter (\ () -> []) printo
  vpGsPrinter :: Show o => VisualParserPrinter () (GrammarSignature (VisualParser () IO) o)
  vpGsPrinter = defaultVisualParserPrinter (\ () -> []) gsPrinter
  gsPrinter :: Show o => GrammarSignature (VisualParser () IO) o -> [LazyText]
  gsPrinter = grammarSignatureShow printo (printVisualParserMonad vpGsPrinter) 2
  debugger  = initGrammarDebugger vpPrinter gsPrinter gram () ax
  loop debugger = do
    (gramSig, debugger) <- runStateT grammarDebugStep debugger
    let txt = gramSig ++ printGrammarDebugger debugger
    if null txt then putStrLn "..." else mapM_ (putStrLn . Lazy.unpack) txt
    getChar
    case unwrapVisualParser $ grammarDebugVisualParser debugger of
      Eval.Return _ -> return ()
      Eval.Fail   _ -> return ()
      Eval.Throw  _ -> return ()
      _             -> loop debugger

main :: IO ()
main = do
  ax <- getArgs
  (verbose, ax) <- return $ case ax of
    "-v"       :ax -> (True, ax)
    "--verbose":ax -> (True , ax)
    ax             -> (False, ax)
  let run :: forall o . Show o
          => (forall m . (Applicative m, Alternative m, MonadPlus m) => Grammar m o)
          -> [String]
          -> IO ()
      run gram = (if verbose then runVerbose gram else runSimple gram) . toLazyText . unwords
  case ax of
    []          -> fail $ unlines $
      [ "", "provide at least two command line arguments:"
      , "* a parser (one of: int, float, str, char)"
      , "* and an input string with which to test the parser"
      ]
    "+int"  :ax -> run (integerLiteral               ) ax
    "int"   :ax -> run (optPlusOrMinus integerLiteral) ax
    "+float":ax -> run (floatLiteral                 ) ax
    "float" :ax -> run (optPlusOrMinus floatLiteral  ) ax
    "str"   :ax -> run (haskellString                ) ax
    "string":ax -> run (haskellString                ) ax
    "char"  :ax -> run (haskellChar                  ) ax
    a           -> fail $ "not a valid parser name: "++show a

