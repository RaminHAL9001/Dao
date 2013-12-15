-- "src/Dao/Object/Random.hs"  instantiates Objects into the RandO class.
-- 
-- Copyright (C) 2008-2013  Ramin Honary.
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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.Object.Random where

----------------------------------------------------------------------------------------------------

--instance HasRandGen ObjectExpr where
--  randO = randO >>= \o -> case toInterm o of
--    [o] -> return o
--    _   -> error "randO generated AST_Object that failed to be converted to an ObjectExpr"

--instance HasRandGen ParamExpr where { randO = pure ParamExpr <*> randO <*> randO <*> no }
--instance HasRandGen [ParamExpr] where { randO = randList 0 10 }
--instance HasRandGen ParamListExpr where { randO = pure ParamListExpr <*> randO <*> no }

--instance HasRandGen CallableCode where
--  randO = do
--    let msg = "A randomly generated subroutine must not be executed."
--    let sub = 
--          Subroutine
--          { origSourceCode = nullValue
--          , staticVars     = error msg
--          , executable     = error msg
--          }
--    let code = 
--          CallableCode
--          { argsPattern    = nullValue
--          , returnType     = nullValue
--          , codeSubroutine = sub
--          }
--    recurse code $ do
--      pats <- randO
--      scrp <- fmap toInterm randO
--      scrp <- case scrp of
--        []  -> return nullValue
--        o:_ -> return o
--      return $ code{argsPattern=pats, codeSubroutine=sub{origSourceCode=scrp}}

----------------------------------------------------------------------------------------------------

--instance HasRandGen a => HasRandGen (TyChkExpr a) where
--  randO = randChoice [NotTypeChecked <$> randO, pure TypeChecked <*> randO <*> randO <*> no]

