-- | This is the main module to import into your project when you want to make use of the Dao
-- database kernel. It exports all of "Language.Interpreter.Dao.Kernel" along with serveral
-- functions for defining databases.
--
-- The database parser is defined in this module, although it mostly just parses the header before
-- launching into a loop that parses as many 'Language.Interpreter.Dao.Kernel.DBEntry's.
module Language.Interpreter.Dao
  ( module Language.Interpreter.Dao.Kernel,
    module Language.Interpreter.Dao.Database,
  )
  where

import           Language.Interpreter.Dao.Kernel
import           Language.Interpreter.Dao.Database

