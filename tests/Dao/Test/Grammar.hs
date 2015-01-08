module Dao.Test.Grammar where

import           Dao.Grammar
import           Dao.Grammar.Debug
import           Dao.Predicate
import           Dao.Text.Parser
import           Dao.Text.Builder

import           Control.Monad.State
import           Control.Exception

import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as Lazy

import           System.Environment

import           Test.QuickCheck

text :: String -> Lazy.Text
text = Lazy.pack

testGrammarIO :: String -> Grammar (TextParser ()) o -> IO (Maybe o)
testGrammarIO str g = catch (testParser str $ makeSrcPrecParser g) $ \e ->
  print (e::InvalidGrammar) >> return Nothing

testParserIO :: String -> TextParser () o -> IO (Maybe o)
testParserIO str p = do
  let (o, ()) = runParser p (parserState () (text str))
  case o of
    OK      o -> return (Just o)
    Backtrack -> putStrLn "Backtrack" >> return Nothing
    PFail err -> print err >> return Nothing

testParser :: Lazy.Text -> TextParser () o -> Predicate ParserError o
testParser str p = fst $ runParser p (parserState () str)

sampleString :: String
sampleString = let s = ' ' in -- so you can see how many spaces there are
  "9876543210" ++ s:s:s:s:s:s : '\t' : s:s:s : "HelloWorld"

sampleText :: Lazy.Text
sampleText = text sampleString

_onfail :: Testable prop => String -> prop -> Property
_onfail = counterexample

expectOK :: (Eq o, Show o) => String -> o -> Lazy.Text -> TextParser () o -> Property
expectOK str expect showParser p = once $ _onfail ('(':showParser++")") $ case testParser str p of
  OK      o -> _onfail ("success, but wrong result") (o===expect)
  Backtrack -> _onfail ("parser backtracked, was expecting "++show expect) False
  PFail err -> _onfail ("parser failed: "++show err)

expectBacktrack :: Show o => String -> Lazy.Text -> TextParser () o -> Property
expectBacktrack str showParser p = once $ _onfail ('(':showParser++")") $ case testParser str p of
  OK      o -> _onfail ("expected backtrack, but succeeded with: "++show o) False
  Backtrack -> property True
  PFail err -> _onfail ("expected backtrack, buf failed: "++show err) False

expectFail :: Show o => String -> Lazy.Text -> TextParser () o -> Property
expectFail str showParser p = once $ _onfail ('(':showParser++")") $ case testParser str p of
  OK      o -> _onfail ("expected failure, but succeeded with: "++show o) False
  Backtrack -> _onfail "expected failure, but backtracked" False
  PFail err -> property True

prop_hand_testing_TextParser :: Property
prop_hand_testing_TextParser = once $ conjoin $ do
  let ok a b = expectOK a b sampleString
  let no a = expectBacktrack a sampleString
  let nums = text "9876543210"
  let hello = text "HelloWorld"
  [ ok "look" sampleText $ look
  , ok "(,) <$> look <*> look" (sampleText, sampleText) $ (,) <$> look <*> look
  , ok "look1" '9' $ look1
  , ok "(,) <$> look1 <*> look1" ('9', '9') $ (,) <$> look1 <*> look1
  , ok "get1" '9' $ get1
  , ok "(,) <$> get1 <*> get1" ('9', '8') $ (,) <$> get1 <*> get1
  , ok "string \"9876543210\"" nums $ string nums
  , no "string \"987654321X\"" $ string (text "987654321X")
  , ok "count 9 isDigit" (text "987654321") $ count 5 isDigit
  , no "count 11 isDigit" $ count 11 isDigit
  , no "eof" $ eof
  , ok "munch isDigit" nums $ munch isDigit
  , no "munch isAlpha" $ munch isAlpha
  , ok "munch1 isDigit" nums $ munch1 isDigit
  , no "munch1 isAlpha" $ munch1 isAlpha
  , ok "satisfy isDigit" '9' $ satisfy isDigit
  , no "satisfy isAlpha" $ satisfy isAlpha
  , ok "noMoreThan 9 isDigit" (text "987654321") $ noMoreThan 9 isDigit
  , ok "noMoreThan 9 isAlpha" mempty $ noMoreThan 9 isAlpha
  , ok "noMoreThan 11 isDigit" nums $ noMoreThan 11 isDigit
  , ok "char '9'" '9' $ char '9'
  , no "char 'X'" $ char 'X'
  , ok "string nums >> munch1 isString >> munch isAlpha" hello $
        string nums >> munch1 isString >> munch isAlpha
  , ok "string nums >> munch1 isString >> count 10 isAlpha" hello $
        string nums >> munch1 isString >> count 10 isAlpha
  , ok "munch isDigit >> noMoreThan 10 isSpace >> (:) <$> satisfy isUpper <*> munch isAlpha" hello $
        munch isDigit >> noMoreThan 10 isSpace >> (:) <$> satisfy isUpper <*> munch isAlpha
  , ok "munch1 isDigit >> munch1 isSpace >> mappend <$> munch isUpper <*> munch isLower" (text "Hello") $
        munch1 isDigit >> munch1 isSpace >> mappend <$> munch isUpper <*> munch isLower
  ]

return [] ------------------------------------------------------------------------------------------

runTests :: IO Bool
runTests = $quickCheckAll

