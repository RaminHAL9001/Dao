-- "src/Dao/Prelude.hs"  provides Haskell language functions necessary
-- to construct a knowledge base of pattern->rule relationships that can
-- be executed in the Dao system's runtime.
-- 
-- Copyright (C) 2008-2012  Ramin Honary.
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

module Dao.Prelude
  ( rule, pattern, action
  )
  where

import           Dao.String
import           Dao.Object
import           Dao.Pattern

import           Data.Binary
import qualified Data.ByteString.Lazy      as B

data Event = SETUP | BEGIN | END | TAKEDOWN deriving (Eq, Ord, Read, Show, Enum)

data Builder a
  = Monadic { ruleStateMonad :: State RuleGroup a }
  | AddRule [Pattern] DaoFunc a
  | AddEvent Event DaoFunc a
  | AddFunc DaoFunc a

toMonadic :: RuleSet a -> RuleSet a
toMonadic a = Monadic $ case a of
  Monadic a -> a
  AddRule pat act a -> do
    modify (\st -> st{ ruleSet = T.unionWith (++) p a (ruleSet st) })
    return a
  AddEvent evt action a -> do
    st <- get
    case evt of
      SETUP    -> put (st{ setupEvents    = setupEvents    st ++ [action] })
      BEGIN    -> put (st{ beginEvents    = beginEvents    st ++ [action] })
      END      -> put (st{ endEvents      = endEvents      st ++ [action] })
      TAKEDOWN -> put (st{ takedownEvents = takedownEvents st ++ [action] })
    return a
  AddFunc func a -> do
    modify $ \st ->
      let i = 1 + funcCounter st
      in st{ funcCounter = i, funcMap = I.insert i func (funcMap st) }
    return a

instance Monad Builder where
  return a = Monadic (return a)
  fa >>= mfa =
    Monadic{ ruleStateMonad = ruleStateMonad (toMonadic fa) >>= ruleStateMonad . toMonadic . mfa }

-- | This makes the syntax for adding events simple. For example:
-- @SETUP ... do@
-- @    ref "a.b.c" := obj "hello"@
(...) :: Event -> Action -> Builder ()
(...) evt act = AddEvent evt act ()

class ToPattern pat where
  toPattern :: pat -> [Pattern]

instance ToPattern Pattern where { toPattern pat = [pat] }
instance ToPattern [Pattern] where { toPattern = id }
instance ToPattern String where { toPattern str = [read str] }
instance ToPattern [String] where { toPattern = map read }

rule :: ToPattern pat => pat -> Action ig -> Builder ()
rule pat act = if null pat then return () else AddRule (toPattern pat) (act >> return ())

-- A 'DaoFunc' is defined so that you can basically gather your variables from input arguments, and
-- then define an action for it like so:
-- @function $
-- @  [do  ORef a <- arg "a" -- gets the first argument, calls it "a" in error messages.@
-- @       OInt b <- arg "b" -- gets the second argument, calls it "b" in error messages.@
-- @       begin $ do@
-- @           action :: Action a@
-- @  ]@
data GetArgs a
  = GetArgs
    { daoFuncToIO :: IO (PValue a) -- ^ is in the IO monad to catch pattern matching exceptions.
    }
  | TryAction
    { getArgsAction :: Action a
    } -- ^ If 'TryAction' can be evaluated without an exception, further monadic evaluation will not
      -- continue beyond it,
  | GotException SomeException
      -- ^ Semantically equivalent to 'Control.Monad.mzero'.

function :: [GetArgs ig] -> Builder ()
function funcs = AddFunc (msum funcs >> return ()) ()

-- | Begin the 'Action' part of a 'DaoFunc' after retrieving the arguments. Specify multiple
begin :: Action a -> GetArgs a
begin = TryAction

