-- "src/Dao/Interpreter/Tokenizer.hs" defines the
-- tokenizer for the Dao programming language.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
-- This file is part of the Dao System.
--
-- The Dao System is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
-- 
-- The Dao System is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

{-# LANGUAGE MultiParamTypeClasses #-}

module Dao.Interpreter.Tokenizer where

import           Dao.String
import           Dao.Interpreter.AST
import           Dao.Parser

import           Control.Monad.Error hiding (Error)

import           Data.Monoid
import           Data.Ix

----------------------------------------------------------------------------------------------------

newtype DaoTT = DaoTT{ unwrapDaoTT :: TT } deriving (Eq, Ord, Ix)
instance TokenType  DaoTT where { unwrapTT = unwrapDaoTT; wrapTT = DaoTT }
instance HasTokenDB DaoTT where { tokenDB  = daoTokenDB }
instance MetaToken DaoTokenLabel DaoTT  where { tokenDBFromMetaValue _ = tokenDB }
instance Show DaoTT where { show = deriveShowFromTokenDB daoTokenDB }

type DaoLexer       = Lexer DaoTT ()

daoTokenDB :: TokenDB DaoTT
daoTokenDB = makeTokenDB daoTokenDef

data DaoTokenLabel
  = SPACE | INLINECOM | ENDLINECOM | COMMA | LABEL | HASHLABEL | STRINGLIT | CHARLIT | DATE | TIME
  | BASE10 | DOTBASE10 | NUMTYPE | EXPONENT | BASE16 | BASE2
  deriving (Eq, Enum, Show, Read)
instance UStrType DaoTokenLabel where { toUStr = derive_ustr; fromUStr = derive_fromUStr; }

daoTokenDef :: LexBuilder DaoTT
daoTokenDef = do
  ------------------------------------------ WHITESPACE -------------------------------------------
  let spaceRX = rxRepeat1(map ch "\t\n\r\f\v ")
  space        <- emptyToken SPACE      $ spaceRX
  
  ------------------------------------------- COMMENTS --------------------------------------------
  closeInliner <- fullToken  INLINECOM  $ rxRepeat1(ch '*') . rx '/'
  inlineCom    <- fullToken  INLINECOM  $ rx "/*" .
    fix (\loop -> closeInliner <> rxRepeat1(invert[ch '*']) . loop)
  endlineCom   <- fullToken  ENDLINECOM $ rx "//" . rxRepeat(invert[ch '\n'])
  
  -------------------------------------------- LABELS ---------------------------------------------
  let alpha = [from 'A' to 'Z', from 'a' to 'z', ch '_']
      labelRX = rxRepeat1 alpha . rxRepeat(from '0' to '9' : alpha)
  label        <- fullToken  LABEL      $ labelRX
  hashlabel    <- fullToken  HASHLABEL  $ rx '#' . labelRX
  
  ---------------------------------------- NUMERICAL TYPES ----------------------------------------
  let from0to9  = from '0' to '9'
      plusMinus = rx[ch '+', ch '-']
      dot       = rx '.'
      number    = rxRepeat1 from0to9
  base10       <- fullToken  BASE10     $ number
  dotBase10    <- fullToken  DOTBASE10  $ dot . base10
  exponent     <- fullToken  EXPONENT   $ rx[ch 'e', ch 'E'] .
    cantFail "expecting exponent value after 'e' or 'E' character" . opt plusMinus . base10
  base2        <- fullToken  BASE2      $ (rx "0b" <> rx "0B") . base10
  base16       <- fullToken  BASE16     $ (rx "0x" <> rx "0X") .
    rxRepeat[from0to9, from 'A' to 'F', from 'a' to 'f']
  numType      <- fullToken  NUMTYPE    $ rx (map ch "UILRFfijs")
  let base10Parser = mconcat $
        [ dotBase10 . opt exponent . opt numType
        , base10 . opt dotBase10 . opt exponent . opt numType
        , base10 . dot . exponent . opt numType
        , base10 . opt exponent . opt numType
        ]
  ---------------------------------------- STRING  LITERAL ----------------------------------------
  let litExpr op =  rx op . (fix $ \loop ->
        rxRepeat(invert [ch op, ch '\\']) . (rx "\\" . rx anyChar . loop <> rx op))
  stringLit    <- fullToken  STRINGLIT $ litExpr '"'
  charLit      <- fullToken  CHARLIT   $ litExpr '\''
  -------------------------------------- KEYWORDS AND GROUPING ------------------------------------
  openers      <- operatorTable $ words "( [ { ${"
  comma        <- emptyToken COMMA (rx ',')
  daoKeywords  <- keywordTable LABEL labelRX $ words $ unwords $
    [ "local const static global"
    , "null false true date time function func rule"
    , "if else for in while with try catch continue break return throw"
    , "BEGIN END EXIT import require"
    , "struct union operator public private" -- other reserved keywords, but they don't do anything yet.
    ]
  let withKeyword key func = do
        tok <- getTokID key :: LexBuilderM DaoTT
        return (rx key . (label <> rxEmptyToken tok . func))
  closers <- operatorTable $ words "} ] )"
  
  ------------------------------------------- OPERATORS -------------------------------------------
  operators    <- operatorTable $ words $ unwords $
    [allUpdateOpStrs, allPrefixOpStrs, allInfixOpStrs, ": ;"]
  
  ------------------------------------ DATE/TIME SPECIAL SYNTAX -----------------------------------
  -- makes use of token types that have already been created above
  let year = rxLimitMinMax 4 5 from0to9
      dd   = rxLimitMinMax 1 2 from0to9
      col  = rx ':'
      hy   = rx '-'
      timeRX = dd . col . dd . col . dd . opt(dot . number)
  timeExpr <- fullToken TIME timeRX
  -- time <- withKeyword "time" $ cantFail "time expression" . space . timeExpr
  time <- withKeyword "time" $ space . timeExpr
  dateExpr <- fullToken DATE $ year . hy . dd . hy . dd
  -- date <- withKeyword "date" $ cantFail "date expression" .
  date <- withKeyword "date" $ space . dateExpr . opt(space . timeExpr) . opt(space . label)
  
  ------------------------------------------- ACTIVATE --------------------------------------------
  -- activate $
  return $ regexToTableLexer $
    [ space, inlineCom, endlineCom, comma
    , stringLit, charLit, base16, base2, base10Parser
    , openers, hashlabel, operators, closers
    , date, time, daoKeywords
    ]

