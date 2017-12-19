-- | This module is very similar to the "Language.Interpreter.Dao" module, except most of the
-- functions have been renamed to an abbreviation so that they are easier to type into a GHCI
-- Read-Eval-Print Loop session. Another major difference is that this module uses
-- 'System.IO.Unsafe.unsafePerformIO' to create a static 'Language.Interpreter.Dao.SessionState',
-- and most of the 'Session' typed functions in the "Language.Interpreter.Dao" module are simply
-- @IO@ typed functions in this module which act on the static
-- 'Language.Interpreter.Dao.SessionState'.
--
-- There are also features for doing simple debugging of query execution, so you can see what
-- production rules are reacting to which queries, and trace program execution so as to see what
-- data is being constructed and returned (or what errors are thrown) in reaction to the query.
--
-- TODO: write all of the code for this module.
module Language.Interpreter.Dao.GHCI
  ( 
  )
  where

import           Language.Interpreter.Dao.Kernel



