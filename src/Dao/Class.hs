-- "Dao/Class.hs"  miscelanous classes.
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

module Dao.Class where

-- | This class was designed specifically for use with the 'Dao.Grammar.Grammar' data type, but it
-- could apply to 'Dao.Rule.StatefulRule' data type, or the 'Dao.Logic.Logic' data type. The idea
-- was for when you want to define a grammar, but don't want to think of a name for it, you just
-- want to use the data type as the name. So, for example:
--
-- @
-- theThingGrammar :: 'Prelude.Monad' m => 'Dao.Grammar.Grammar' m ('Prelude.String', 'Prelude.Int', ['Prelude.String'])
-- theThingGrammar = 'Dao.Grammar.grammar' ...
--
-- main = 'Prelude.print' $ 'Dao.Regex.runParser' (theThingGrammar :: Grammar IO Thing)
-- @
-- 
-- What you can instead do is something like this:
--
-- @
-- newtype Thing = Thing ('Prelude.String', 'Prelude.Int', ['Prelude.String'])
-- instance 'Prelude.Monad' m => 'The' ('Dao.Grammar.Grammar' m Thing) where
--     'the' = 'Dao.Grammar.grammar' ...
--
-- main = 'Dao.Regex.runParser' (the :: Grammar IO Thing)
-- @
--
class The o where { the :: o; }

instance The () where { the = (); }

-- | This function is @'Prelude.flip' 'Prelude.fmap'@,
-- or equivalently, @'Prelude.flip' ('Control.Applicative.<$>')@.
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap

