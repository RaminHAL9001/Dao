-- "tests/pprint.hs"  tests the 'Dao.PPrint' module.
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

module Main where

import           Dao.String
import           Dao.Object
import           Dao.Object.Show
import           Dao.PPrint
import           Dao.Token

el = EndlineComment . ustr
il = InlineComment  . ustr
lu = LocationUnknown
a  = Literal (ORef (LocalRef (ustr "a"))) lu
i0 = Literal (OInt 0) lu
i1 = Literal (OInt 1) lu
i2 = Literal (OInt 2) lu
i3 = Literal (OInt 3) lu
add   = Com ADD
eqeq  = Com EQUL
modu  = Com MOD
mult  = Com MULT
diveq = Com UDIV
eq    = Com UCONST
evalObj expr = EvalObject expr [] lu

ifExpr :: Int -> ScriptExpr
ifExpr i =
  IfThenElse
    []
    (Equation (Equation a modu i2 lu) eqeq i0 lu)
    (Com $
      [ ComBefore [el " if the number is even, divide by two"] $
          evalObj (AssignExpr a diveq i2 lu)
      ])
    (Com $
      [ ComBefore [el "if the number is odd, multiply by three and add one"] $
          evalObj (AssignExpr a eq (Equation (Equation a mult i3 lu) add i1 lu) lu)
      ] ++ if i<=0 then [] else [ComBefore [el "then test it again"] (ifExpr (i-1))]
    )
    LocationUnknown

main = print $ pEvalState $ pPrint (ifExpr 3)

