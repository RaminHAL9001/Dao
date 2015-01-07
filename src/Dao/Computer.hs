-- "Dao/Computer.hs"  combines several monad transformers into a single
-- computation that can be stepped through.
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

-- | This module provides a 'InstructionT', which is a data type that combines several monad
-- transformers into a single data type. However a 'InstructionT' can be stepped through
-- node-by-node, and each instruction can be inspected before it is evaluated. This makes it useful
-- for developing debuggers.
-- 
-- The combination of all of the classical monad transformers constructs a fairly orthagonal
-- instruction set for a Von Neumann architecture computer, so it can be used to implement
-- interpreters for arbitrary programming languages, and can potentially be used as an intermediate
-- code for a compiler.
--
-- Although the 'InstructionT' monad incorporates all functionality, and has a polymorphic type with
-- six type variables, it is trivial to implement a wrapper monad that only provides the
-- functionality necessary (for example just "Control.Monad.State" or just "Control.Monad.Throw")
-- and instance the rest of the polymorphic types as @()@. For example, to implement a wrapper
-- around 'InstructionT' that implements only 'Control.Monad.State.MonadState',
-- 'Control.Monad.IO.Class', and 'Control.Monad.MonadPlus' and is lifted into the @IO@ monad, you
-- could do the following:
--
-- @
-- newtype MyDebugger st a =
--         MyDebugger { unwrapMyDebugger :: 'InstructionT' () () () () () st IO a }
-- @
-- 
-- @
-- startMyDebugger :: st -> MyDebugger st a
-- startMyDebugger st = MyDebugger $ 'startComputer' ('ComputerState' () () st)
-- @
-- 
-- @
-- stepMyDebugger :: MyDebugger st a -> MyDebugger st a
-- stepMyDebugger (MyDebugger comp) = MyDebugger $ 'stepT' comp
-- @
-- 
-- @
-- runMyDebugger :: MyDebugger st a -> st -> IO (a, st)
-- runMyDebugger (MyDebugger comp) st = 'Data.Functor.fmap' ('Data.Functor.fmap' 'computationStateTable') $
--     'runComputerT' comp ('ComputerState' () () st)
-- @
-- 
-- @
-- instance 'Data.Functor.Functor' (MyDebugger st) where
--     'Data.Functor.fmap' f (MyDebugger comp) = MyDebugger $ 'Data.Functor.fmap' f comp
-- @
-- 
-- @
-- instance 'Control.Monad.Monad' (MyDebugger st) where
--     'Control.Monad.return' = MyDebugger . 'Control.Monad.return'
--     ('Control.Monad.>>=') (MyDebugger comp) f = MyDebugger $ comp >>= unwrapMyDebugger . f
-- @
-- 
-- @
-- instance 'Control.Applicative' (MyDebugger st) where
--     'Control.Applicative.pure' = 'Control.Monad.return'
--     ('Control.Monad.<*>') = 'Control.Monad.ap'
-- @
-- 
-- @
-- instance 'Control.Monad.MonadPlus' (MyDebugger st) where
--     'Control.Monad.mzero' = MyDebugger 'Control.Monad.mzero'
--     'Control.Monad.mplus' (MyDebugger compA) (MyDebugger compB) = MyDebugger $ 'Control.Monad.mplus' compA compB
-- @
-- 
-- @
-- instance 'Control.Monad.Alternative' (MyDebugger st) where
--     'Control.Applicative.empty' = 'Control.Monad.mzero'
--     ('Control.Monad.<|>') = 'Control.Monad.mplus'
-- @
-- 
-- @
-- instance 'Control.Monad.IO.Class.MonadIO' (MyDebugger st) where
--     'Control.Monad.IO.Class.liftIO' = MyDebugger . 'Control.Monad.IO.Class.liftIO'
-- @
-- 
-- @
-- instance 'Control.Monad.State.MonadState' st (MyDebugger st) where
--      'Control.Monad.State.state' = MyDebugger . 'Control.Monad.State.state'
-- @
-- 
-- If MyDebugger were to instantiate 'Control.Monad.Throw.MonadError' with a polymorphic type
-- @err@ instead of @()@, the instance would look like this:
--
-- @
-- newtype MyDebugger err o = MyDebugger { unwrapMyDebugger :: 'InstructionT' () () err () () () IO o }
-- 
-- instance 'Control.Monad.Throw.MonadError' err (MyDebugger err) where
--     'Control.Monad.Except.throwError' = MyDebugger . 'Control.Monad.Except.throwError'
--     'Control.Monad.Except.catchError' (MyDebugger try) catch =
--          MyDebugger $ 'Control.Monad.Except.catchError' try (unwrapMyDebugger . catch)
-- @
-- 
-- If MyDebugger were to instantiate 'Control.Monad.Reader.MonadReader' with a polymorphic type @rd@
-- instead of @()@ the instance would look like this:
--
-- @
-- newtype MyDebugger rd o = MyDebugger { unwrapMyDebugger :: 'InstructionT' () () () rd () () IO o }
-- 
-- instance 'Control.Monad.Reader.MonadReader' rd (MyDebugger rd) where
--     'Control.Monad.Reader.local' f (MyDebugger o) = MyDebugger $ 'Control.Monad.Reader.local' f o
--     'Control.Monad.Reader.ask' = MyDebugger 'Control.Monad.Reader.ask'
-- @
-- 
-- If MyDebugger were to instantiate 'Control.Monad.Writer.MonadWriter' with a polymorphic type @wr@
-- instead of @()@ the instance would look like this:
--
-- @
-- newtype MyDebugger wr o = MyDebugger { unwrapMyDebugger :: 'InstructionT' () () () () wr () IO o }
-- 
-- instance 'Control.Monad.Writer.MonadWriter' wr (MyDebugger wr)where
--      'Control.Monad.Writer.writer' = MyDebugger . 'Control.Monad.Writer.writer'
--      'Control.Monad.Writer.listen' = MyDebugger . 'Control.Monad.Writer.listen' . unwrapMyDebugger
--      'Control.Monad.Writer.pass'   = MyDebugger . 'Control.Monad.Writer.pass'   . unwrapMyDebugger
-- @
--
-- This module is intended to be imported qualified, because most of the API names are pretty terse.
module Dao.Computer where

import           Dao.Text
import           Dao.PPrint

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Control.Monad.State
import           Control.Monad.Writer.Class

import           Data.Functor.Identity
import           Data.List (partition)
import           Data.Monoid
import           Data.Typeable

import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as Lazy

----------------------------------------------------------------------------------------------------

type Instruction lbl fu err rd wr st o = InstructionT lbl fu err rd wr st Identity o

-- | This data type is a monadic data type combining capabilities of several fundamental monad
-- transformers: 'Control.Monad.Except.MonadError', 'Control.Monad.Reader.MonadReader',
-- 'Control.Monad.Writer.MonadWriter', 'Control.Monad.State.MonadState',
-- 'Control.Monad.Trans.MonadTrans', 'Control.Monad.IO.Class.MonadIO', and
-- 'Control.Monad.MonadPlus'.
-- 
-- There are six type parameters (seven if you count the return type):
-- @fu@  -- see the API documentation for 'Eval' for more information
-- @err@ -- the 'Control.Monad.Except.MonadError'-like exception type used
-- @rd@  -- the 'Control.Monad.Reader.MonadReader'-like reader environment used
-- @wr@  -- the 'Control.Monad.Writer.MonadWriter'-like writer stream used
-- @st@  -- the 'Control.Monad.State.MonadState'-like state table used
-- @m@   -- the 'Control.Monad.Trans.lift'-ed monad, usually @IO@, or 'Data.Functor.Identity.Identity' for pure functions.
data InstructionT lbl fu err rd wr st m o
  = Empty
    -- ^ monadic 'Control.Monad.mzero', a.k.a. applicative 'Control.Applicative.empty'.
  | Return                                                o
    -- ^ monadic return
  | Lift             (m   (InstructionT lbl fu err rd wr st m o))
    -- ^ lifts a function of the containing monad into the 'Instruction' monad. This is similar to a
    -- call to an interrupt vector in an assembler language.
  | Eval             fu   (InstructionT lbl fu err rd wr st m o)
    -- ^ similar to immediate instructions in an assembler language, the input parameter for the
    -- instruction is stored with the instruction in the program text. 'Eval' steps model evaluation
    -- of pure functions like arithmetic and logic. The 'Eval' step reduces the @fu@ data type to
    -- another @fu@ data type.
  | Choice (InstructionT lbl fu err rd wr st m o)
                          (InstructionT lbl fu err rd wr st m o)
    -- ^ instantiates 'Control.Monad.mplus' and @('Control.Applicative.<>')@, when the first branch
    -- evaluates to 'Empty' try the second branch.
  | Put              st   (InstructionT lbl fu err rd wr st m o)
    -- ^ instantiates 'Control.Monad.State.put': rewrites the current state table.
  | Get             (st -> InstructionT lbl fu err rd wr st m o)
    -- ^ instantiates 'Control.Monad.State.get': retrieves the current 'stateTable'.
  | Got              st   (InstructionT lbl fu err rd wr st m o)
    -- ^ logs that a 'Get' action was preformed.
  | Ask             (rd -> InstructionT lbl fu err rd wr st m o)
    -- ^ instantiates 'Control.Monad.Reader.ask', it reads the 'readerEnv'.
  | Answered         rd   (InstructionT lbl fu err rd wr st m o)
    -- ^ logs that an 'Ask' function was performed.
  | Local     (rd -> rd)  (InstructionT lbl fu err rd wr st m o)
    -- ^ instantiates 'Control.Monad.Reader.local', transforms the 'readerEnv' but keeps the old
    -- 'readerEnv' on the stack. When the next 'InstructionT' complets, the stack is popped and the
    -- previous 'readerEnv' is restored, thus the environment is visible locally only to the next
    -- 'InstructionT'.
  | Env              rd   (InstructionT lbl fu err rd wr st m o)
    -- ^ when 'Local' has been used, 'Env' keeps the previous 'readerEnv'-ironment on the stack.
    -- This also does the job of logging that a 'Local' 'InstructionT' has been performed.
  | Write            wr   (InstructionT lbl fu err rd wr st m o)
    -- ^ instantiates 'Control.Monad.Writer.writer': appends data to the 'writerStream'
  | Listen          (wr -> InstructionT lbl fu err rd wr st m o)
    -- ^ instantiates 'Control.Monad.Writer.listen': reads the 'writerStream'.
  | Heard            wr   (InstructionT lbl fu err rd wr st m o)
    -- ^ logs that a 'Listen' even occurred.
  | Pass                  (InstructionT lbl fu err rd wr st m (o, wr -> wr))
    -- ^ instnatiates 'Control.Monad.Writer.pass': takes a parameter 'Instruction' that returns a
    -- value and a function to transform the 'writerStream' (to pass over the whole stream). When
    -- this 'InstructionT' completes, the parameter instruction, the transformation function is
    -- applied to the writerStream, and then the return value is returned again.
  | Passed           wr   (InstructionT lbl fu err rd wr st m o)
    -- ^ loggs the result of the 'Pass' instruction
  | Catch  (InstructionT lbl fu err rd wr st m o)
                   (err -> InstructionT lbl fu err rd wr st m o)
    -- ^ catch an error
  | Caught    err         (InstructionT lbl fu err rd wr st m o)
    -- ^ logs that an error was caught
  | NoError               (InstructionT lbl fu err rd wr st m o)
    -- ^ indicates to the 'Catch' function that the try was successful, and the catch need not
    -- evaluate and can be removed from the stack.
  | Throw     err
    -- ^ throws an error
  | Label     lbl         (InstructionT lbl fu err rd wr st m o)
    -- ^ pushes a label onto the stack, good for marking recursion.
  | Address   lbl         (InstructionT lbl fu err rd wr st m o)
    -- ^ logs a label without pushingit onto the stack.
  | Fail      StrictText
  deriving Typeable

instance (Show lbl, Show fu, Show err, Show rd, Show wr, Show st, Show o) =>
  Show (InstructionT lbl fu err rd wr st m o) where
    show =
      let print :: Show t => t -> [PPrint]
          print = return . PText . Strict.pack . show
          printer =
            PPrintComputer
            { pprintLabel        = print
            , pprintFunction     = print
            , pprintError        = print
            , pprintReaderEnv    = print
            , pprintWriterStream = print
            , pprintStateTable   = print
            , pprintData         = print
            , pprintInstructionDepth = 1
            }
      in Lazy.unpack . runTextPPrinter 2 80 . pprintInstruction printer

instance Functor m => Functor (InstructionT lbl fu err rd wr st m) where
  fmap f o = case o of
    Empty            -> Empty
    Return        o  -> Return              $            f  o
    Lift          o  -> Lift                $ fmap (fmap f) o
    Eval       fu o  -> Eval            fu  $ fmap f o
    Choice     oA oB -> Choice (fmap f  oA) $ fmap f oB
    Put        st o  -> Put             st  $ fmap f o
    Get           o  -> Get                 $ fmap (fmap f) o
    Got        st o  -> Got             st  $ fmap f o
    Ask           o  -> Ask                 $ fmap (fmap f) o
    Answered   rd o  -> Answered        rd  $ fmap f o
    Local      rd o  -> Local           rd  $ fmap f o
    Env        rd o  -> Env             rd  $ fmap f o
    Write      wr o  -> Write           wr  $ fmap f o
    Listen        o  -> Listen              $ fmap (fmap f) o
    Heard      wr o  -> Heard           wr  $ fmap f o
    Pass          o  -> Pass   $ fmap (\ (o, wr) -> (f o, wr)) o
    Passed     wr o  -> Passed          wr  $ fmap f o
    Catch    try  o  -> Catch  (fmap f try) $ fmap (fmap f) o
    Caught   err  o  -> Caught         err  $ fmap f o
    NoError       o  -> NoError             $ fmap f o
    Throw    err     -> Throw          err
    Label    lbl  o  -> Label          lbl  $ fmap f o
    Address  lbl  o  -> Address        lbl  $ fmap f o
    Fail     msg     -> Fail           msg

instance (Monoid wr, Monad m) => Monad (InstructionT lbl fu err rd wr st m) where
  return   = Return
  o >>= f  = case o of
    Empty          -> Empty
    Return      o  -> f o
    Lift        o  -> Lift $ o >>= return . (>>= f)
    Eval     fu o  -> Eval     fu $ o >>= f
    Choice   oA oB -> Choice (oA >>= f) (oB >>= f)
    Put      st o  -> Put      st $ o >>= f
    Get         o  -> Get         $ fmap (>>= f) o
    Got      st o  -> Got      st $ o >>= f
    Ask         o  -> Ask         $ fmap (>>= f) o
    Answered rd o  -> Answered rd $ o >>= f
    Local    rd o  -> Local    rd $ o >>= f
    Env      rd o  -> Env      rd $ o >>= f
    Write    wr o  -> Write    wr $ o >>= f
    Listen      o  -> Listen      $ fmap (>>= f) o
    Heard    wr o  -> Heard    wr $ o >>= f
    Pass        o  -> o >>= \ (o, wr) -> f o >>= \o -> Pass $ Return (o, wr)
    Passed   wr o  -> Passed   wr $ o >>= f
    Catch   try o  -> Catch  (try >>= f) (fmap (>>= f) o)
    Caught  err o  -> Caught  err $ o >>= f
    NoError     o  -> NoError     $ o >>= f
    Throw   err    -> Throw   err
    Label   lbl o  -> Label   lbl $ o >>= f
    Address lbl o  -> Address lbl $ o >>= f
    Fail    err    -> Fail    err
  fail     = Fail . Strict.pack

instance (Monoid wr, Functor m, Monad m) => Applicative (InstructionT lbl fu err rd wr st m) where
  pure  = return
  (<*>) = ap

instance (Monoid wr, Monad m) => MonadPlus (InstructionT lbl fu err rd wr st m) where
  mzero     = Empty
  mplus a b = case a of
    Choice a b' -> Choice a $ mplus b b'
    a           -> Choice a b

instance (Monoid wr, Functor m, Monad m) => Alternative (InstructionT lbl fu err rd wr st m) where
  empty = mzero
  (<|>) = mplus

instance (Monoid wr, Monad m) => MonadReader rd (InstructionT lbl fu err rd wr st m) where
  ask   = Ask $ \rd  -> Answered rd $ Return rd
  local = Local

instance (Monoid wr, Monad m) => MonadState st (InstructionT lbl fu err rd wr st m) where
  state f = Get $ (\ (a, st) -> Put st $ Return a) . f
  put st  = Put st $ return ()
  get     = Get $ \st -> Put st $ Return st

instance (Monoid wr, Monad m) => MonadError err (InstructionT lbl fu err rd wr st m) where
  throwError           = Throw
  catchError try catch = case try of
    Throw err -> Caught err $ catch err
    o         -> NoError o

instance (Monoid wr, Monad m) => MonadWriter wr (InstructionT lbl fu err rd wr st m) where
  writer (o, w) = Write w $ Return o
  tell       w  = Write w $ Return ()
  listen     o  = Listen $ \w -> o >>= \o -> Heard w $ Return (o, w)
  pass          = Pass

instance MonadTrans (InstructionT lbl fu err rd wr st) where { lift m = Lift $ m >>= return . Return; }

instance (Monoid wr, MonadIO m) => MonadIO (InstructionT lbl fu err rd wr st m) where
  liftIO o = Lift $ liftIO o >>= return . Return

----------------------------------------------------------------------------------------------------

-- | The computation state contains the "Control.Monad.Reader" environment, the
-- "Control.Monad.Writer" stream, and the "Control.Monad.State" table. The 'InstructionT' data type
-- instantiates these classes so the appropriate item in this object is read or updated accordingly.
data ComputerState rd wr st
  = ComputerState
    { readerEnv    :: rd
    , writerStream :: wr
    , stateTable   :: st
    }

type Computer lbl fu err rd wr st o = ComputerT lbl fu err rd wr st Identity o

data ComputerT lbl fu err rd wr st m o
  = Computer
    { registers      :: ComputerState rd wr st
    , programCounter :: InstructionT lbl fu err rd wr st m o
    }
    -- ^ This constructor contains the current state of the computation (the 'registers') and the
    -- next instruction on that will be executed (the 'programCounter').

instance Functor m => Functor (ComputerT lbl fu err rd wr st m) where
  fmap f o = case o of
    Computer st pc -> Computer st $ fmap f pc

-- | The pure version of 'resetT'.
reset :: ComputerState rd wr st -> Instruction lbl fu err rd wr st o -> Computer lbl fu err rd wr st o
reset st = runIdentity . resetT st

-- | Force all of the 'registers' in the 'ComputerT' to the value of the given 'ComputerState'
-- and 'programCounter'.
resetT :: Monad m => ComputerState rd wr st -> InstructionT lbl fu err rd wr st m o -> m (ComputerT lbl fu err rd wr st m o)
resetT st = return . Computer st

-- | The pure version of 'stepT'.
step :: Monoid wr => Computer lbl fu err rd wr st o -> Computer lbl fu err rd wr st o
step = runIdentity . stepT

-- | This function steps the 'Computer', evaluating the next 'InstructionT' in the pipeline. This
-- function could be called @step@ but that name interfears with the 'Dao.Grammar.step' function. It
-- is called 'stepT' because it is like a single step of the 'Computer's clock.
stepT :: (Monoid wr, Monad m) => ComputerT lbl fu err rd wr st m o -> m (ComputerT lbl fu err rd wr st m o)
stepT (Computer run o) = case o of
  Empty            -> next run   Empty
  Return        o  -> next run $ Return o
  Lift          o  -> o >>= return . Computer run
  Eval       _  o  -> next run o
  Choice     oA oB -> case oA of
    Empty     -> next run oB
    Return  o -> next run $ Return o
    Throw err -> next run $ Throw err
    Fail  err -> next run $ Fail err
    oA        -> loop run oA $ \run oA -> next run $ Choice oA oB
  Put        st o  -> next (run{stateTable=st}) o
  Get           o  -> next run $ let st = stateTable run in Got st $ o st
  Got        st o  -> next (run{stateTable=st}) o
  Ask           o  -> next run $ let rd = readerEnv run in Answered rd $ o rd
  Answered   rd o  -> next (run{readerEnv=rd}) o
  Local      rd o  -> let { rd0 = readerEnv run; rd1 = rd rd0; } in
    next (run{readerEnv=rd1}) $ Env rd0 o
  Env        rd o  -> case o of
    Empty     -> next run $ Empty
    Return  o -> next run $ Return o
    Throw err -> next run $ Throw err
    o         -> loop run o $ \run -> next run . Env rd
  Write      wr o  -> next (run{writerStream=writerStream run <> wr}) o
  Listen        o  -> next run $ let wr = writerStream run in Heard wr (o wr)
  Heard      wr o  -> next (run{writerStream=wr}) o
  Pass          o  -> stepT (Computer run o) >>= \ (Computer run owr) -> do
    return $ Computer run $ owr >>= \ (o, wr) -> Passed (wr $ writerStream run) $ Return o
  Passed     wr o  -> next (run{writerStream=wr}) o
  Catch     try o  -> case try of
    Throw   err -> next run $ Caught err $ o err
    Catch try o -> next run $ Catch try o
    NoError   o -> next run o
    try         -> loop run try $ \run -> next run . flip Catch o
  Caught   _    o  -> stepT $ Computer run o
  NoError       o  -> stepT $ Computer run o
  Throw    err     -> next run $ Throw err
  Label    lbl  o  -> case o of
    Empty     -> next run $ Empty
    Return o  -> next run $ Return o
    Throw err -> next run $ Throw err
    o         -> loop run o $ \run -> next run . Label lbl
  Address  _    o  -> next run o
  Fail     msg     -> next run $ Fail msg
  where
    next run = return . Computer run
    loop run o f = stepT (Computer run o) >>= \ (Computer run o) -> f run o

-- | Check if this 'ComputerT' is on a halting 'InstructionT'. An 'InstructionT' is halting if
-- the 'stepT' function cannot perform any transformation on the 'ComputerT' and behaves is
-- 'Prelude.id' returning the 'ComputerT' value that was passed. If you are looping on 'stepT',
-- you should check if the result of 'stepT' is 'halted' after every 'stepT'.
halted :: ComputerT lbl fu err rd wr st m o -> Bool
halted (Computer _ o) = case o of
  Empty    -> True
  Return _ -> True
  Throw  _ -> True
  Fail   _ -> True
  o        -> loop o
  where
    loop o = case o of
      Eval     _ o -> loop o
      Put      _ o -> loop o
      Got      _ o -> loop o
      Answered _ o -> loop o
      Local    _ o -> loop o
      Env      _ o -> loop o
      Write    _ o -> loop o
      Heard    _ o -> loop o
      Passed   _ o -> loop o
      Caught   _ o -> loop o
      NoError    o -> loop o
      Label    _ o -> loop o
      Address  _ o -> loop o
      Fail     _   -> True
      _            -> False

-- | The pure version of 'runUntilT'; evaluates without a containing monad.
runUntil
  :: Monoid wr
  => Computer lbl fu err rd wr st o
  -> (Computer lbl fu err rd wr st o -> Bool)
  -> Computer lbl fu err rd wr st o
runUntil o until = if until o then o else runUntil (step o) until

-- | Repeatedly evaluate a 'ComputerT' with 'stepT' until the 'ComputerT' satisfies the given
-- pausing predicate function, at which point the looping will stop and the 'ComputerT' at that
-- point will be returned. Remember, the pausing predicate function should return 'Prelude.True' to
-- halt the evaluation loop, and 'Prelude.False' to indicate that the evaluation loop should
-- continue.
runUntilT
  :: (Monoid wr, Monad m)
  => ComputerT lbl fu err rd wr st m o
  -> BreakpointT lbl fu err rd wr st m o
  -> m (ComputerT lbl fu err rd wr st m o)
runUntilT o until = until o >>= \stop -> if stop then return o else stepT o >>= flip runUntilT until

-- | The pure version of 'runT'; evaluates without a containing monad.
run :: Monoid wr => Computer lbl fu err rd wr st o -> Computer lbl fu err rd wr st o
run = flip runUntil halted

-- | This function repeatedly evaluates a 'ComputerT' until the 'ComputerT' satisfies the
-- 'halted' condition. This function is simply defined as:
--
-- > 'Prelude.flip' 'runUntilT' (return . 'halted')
--
runT :: (Monoid wr, Monad m) => ComputerT lbl fu err rd wr st m o -> m (ComputerT lbl fu err rd wr st m o)
runT = flip runUntilT (return . halted)

----------------------------------------------------------------------------------------------------

type BreakpointName = StrictText
type BreakpointT lbl fu err rd wr st m o = ComputerT lbl fu err rd wr st m o -> m Bool
type Breakpoint lbl fu err rd wr st o = BreakpointT lbl fu err rd wr st Identity o

-- | This data type provides a 'ComputerT' with a history of every computational step performed up
-- to a given limit of steps.
data DebugStateT lbl fu err rd wr st m o
  = DebugState
    { debugTarget   :: ComputerT lbl fu err rd wr st m o
    , historyLength :: Int
    , debugHistory  :: [ComputerT lbl fu err rd wr st m o]
    , breakpoints   :: [(BreakpointName, BreakpointT lbl fu err rd wr st m o)]
    }

type DebugState lbl fu err rd wr st = DebugStateT lbl fu err rd wr st Identity

-- | This is a debugging monad good for making read-eval-print loops. The 'ComputerT' it is
-- debugging must run in the same parent monad as the 'DebuggerT'.
--
-- 'DebuggerT' is a monadic 'Control.Monad.State.StateT' transformer. There are a lot of parameters
-- but it is easily understood. Let us define a newtype for:
--
-- > 'Control.Monad.State.StateT' state parentMonad returnType
--
-- where:
--
-- @
--   state       = 'DebugState' lbl fu err rd wr st m o
--   parentMonad = m -- the exact same type as the m used in the 'ComputerT' type
--   returnType  = d -- the DebuggerT's monadic variable
-- @
--
-- so substituting @state@, @parentMonad@, and @returnType@ into the definition for
-- 'Control.Monad.State.StateT', we get the definition of 'DebuggerT' which is:
--
-- @  'Control.Monad.State.StateT' ('DebugState' lbl fu err rd wr st m o) m d@
--
-- Therefore all those other variables, @lbl@, @fu@, @err@, @rd@, @wr@, @st@, @m@, and @o@ are just
-- the variables for the 'DebuggerT's state. 
--
-- Both @o@ and @d@ are monadic variables (the return types), but @o@ (meaning "object") is the
-- return type for the 'ComputerT', and @d@ is the return type for the 'DebuggerT'. A good way to
-- remember the difference is between @o@ and @d@ is to think of @o@ as the fundamental unit of
-- computation for the 'ComputerT'. So if you are emulating a 64-bit CPU, the type @o@ will always
-- be 'Data.Int.Int64'. If you are emulating the interpreter of a higher-level language like Java,
-- @o@ would be a Haskell wrapper around a "java.lang.Object" type. The @d@ type, on the other hand,
-- will be what you see when you use the 'DebuggerT' in GHCi, so it will depend on what function you
-- call: 'Control.Monad.State.get' will result in @d@ being a 'DebugState' specific to your
-- 'Computer', whereas calling 'debugStep' will result in @d@ being the @()@ type. Whatever the type
-- of @d@ may be, the type of @o@ does not change as it is part of the state type.
--
-- The parent monad for the 'ComputerT' that you are debugging *must* be identical to the parent
-- monad for the 'DebuggerT'. For example, if you use 'Computer' instead of 'ComputerT' the monad of
-- 'ComputerT' is 'Data.Functor.Identity.Identity', therefore the 'DebuggerT' must also run in
-- 'Data.Functor.Identity.Identity'. So it is usually a good idea to keep your 'DebuggerT'
-- polymorphic over the monadic type, and let the monadic type of the 'ComputerT' dictate what it
-- becomes when you actually use it.
--
-- *NOTE:* the 'DebuggerT' type variables flip the order of @m@ and @o@ relative to how they are
-- defined for the 'InstructionT', 'ComputerT', and 'DebugState' data types. This is probably
-- very confusing but it is necessary for instantiating 'DebuggerT' into 'Data.Functor.Functor',
-- 'Control.Monad.Monad', 'Control.Monad.State.MonadState'.
newtype DebuggerT lbl fu err rd wr st o m d =
  Debugger { unwrapDebugger :: StateT (DebugStateT lbl fu err rd wr st m o) m d }

type Debugger lbl fu err rd wr st o = DebuggerT lbl fu err rd wr st o Identity

instance Functor m => Functor (DebuggerT lbl fu err rd wr st o m) where
  fmap f (Debugger o) = Debugger $ fmap f o

instance Monad m => Monad (DebuggerT lbl fu err rd wr st o m) where
  return = Debugger . return
  (Debugger o) >>= f = Debugger $ o >>= unwrapDebugger . f

instance (Monad m, MonadPlus m) => MonadPlus (DebuggerT lbl fu err rd wr st o m) where
  mzero = Debugger mzero
  mplus (Debugger a) (Debugger b) = Debugger $ mplus a b

instance (Functor m, Monad m, MonadPlus m) => Alternative (DebuggerT lbl fu err rd wr st o m) where
  empty = mzero
  (<|>) = mplus

instance (Functor m, Monad m) => Applicative (DebuggerT lbl fu err rd wr st o m) where
  pure  = return
  (<*>) = ap

instance Monad m =>
  MonadState (DebugStateT lbl fu err rd wr st m o) (DebuggerT lbl fu err rd wr st o m) where
    state = Debugger . state

instance MonadTrans (DebuggerT lbl fu err rd wr st o) where
  lift = Debugger . lift

instance MonadIO m => MonadIO (DebuggerT lbl fu err rd wr st o m) where
  liftIO = Debugger . liftIO

-- | Runs 'stepT' on the 'ComputerT'.
debugStep :: (Monoid wr, Monad m) => DebuggerT lbl fu err rd wr st o m ()
debugStep = do
  computer <- gets debugTarget >>= lift . stepT
  modify $ \st ->
    st{ debugTarget  = computer
      , debugHistory = take (max 0 $ historyLength st) $ computer : debugHistory st
      }

-- | Evaluate all of the 'breakpoints' against the current state of the 'debugTarget', return the
-- labels of every breakpoint that evaluates 'Prelude.True'.
debugCheckBreaks :: Monad m => DebuggerT lbl fu err rd wr st o m [BreakpointName]
debugCheckBreaks = do
  debugger <- get
  let target = debugTarget debugger
  let loop o bps = case bps of
        []     -> return o
        bp:bps -> lift (snd bp target) >>= \match -> loop (if match then o++[fst bp] else o) bps
  loop [] $ breakpoints debugger

-- | Runs 'runUntilT' until a breakpoint is hit, retursn the list of breakpoint labels that matched
-- when the debugger was halted.
debugRun :: (Monoid wr, Monad m) => DebuggerT lbl fu err rd wr st o m [BreakpointName]
debugRun = debugStep >> debugCheckBreaks >>= \bps -> if null bps then debugRun else return bps

-- | Sets a breakpoint associated with a name. Removes any breakpoints by the same name if any by
-- that name have already been set.
debugSetBreakpoint
  :: Monad m
  => BreakpointName
  -> (ComputerT lbl fu err rd wr st m o -> m Bool)
  -> DebuggerT lbl fu err rd wr st o m ()
debugSetBreakpoint name bp = modify $ \st ->
  st{ breakpoints = filter ((/= name) . fst) (breakpoints st) ++ [(name, bp)] }

-- | Remove the breakpoint associated with the given name, return it (if it existed).
debugDeleteBreakpoint
  :: Monad m
  => BreakpointName
  -> DebuggerT lbl fu err rd wr st o m [(BreakpointName, BreakpointT lbl fu err rd wr st m o)]
debugDeleteBreakpoint name = state $ \st ->
  let (remove, keep) = partition ((== name) . fst) (breakpoints st)
  in  (remove, st{ breakpoints=keep })

-- | Show the current state of the computer.
debugPrint
  :: (Functor m, Monad m)
  => PPrintComputer lbl fu err rd wr st o
  -> DebuggerT lbl fu err rd wr st o m [PPrint]
debugPrint pp = pprintComputer pp <$> gets debugTarget

----------------------------------------------------------------------------------------------------

-- | A data type containing callback functions for pretty printing instructions.
data PPrintComputer lbl fu err rd wr st o
  = PPrintComputer
    { pprintLabel        :: lbl -> [PPrint]
    , pprintFunction     :: fu  -> [PPrint]
    , pprintError        :: err -> [PPrint]
    , pprintReaderEnv    :: rd  -> [PPrint]
    , pprintWriterStream :: wr  -> [PPrint]
    , pprintStateTable   :: st  -> [PPrint]
    , pprintData         :: o   -> [PPrint]
    , pprintInstructionDepth :: Int
    }

-- | Show an 'InstructionT' data type with up to @n@ steps of instruction look-ahead.
pprintInstruction
  :: PPrintComputer lbl fu err rd wr st o
  -> InstructionT lbl fu err rd wr st m o
  -> [PPrint]
pprintInstruction pp o = case o of
  Empty         -> [pText "empty"]
  Return      o -> [pText "return", pIndent (pprintData pp o)]
  Lift        _ -> [pText "lift"]
  Eval     fu o -> loop o [pText "eval ", pIndent $ pprintFunction pp fu]
  Choice   a  b -> [pText "choice", pIndent (loop a []), pText "branch", pIndent $ loop b []]
  Put      st o -> loop o [pText "put", pIndent $ pprintStateTable pp st]
  Get         _ -> [pText "get"]
  Got      st o -> loop o [pText "got", pIndent $ pprintStateTable pp st]
  Ask         _ -> [pText "ask"]
  Answered rd o -> loop o [pText "answered", pIndent $ pprintReaderEnv pp rd]
  Local    _  o -> loop o [pText "local"]
  Env      rd o -> envLoop rd o
  Write    wr o -> loop o [pText "write", pIndent $ pprintWriterStream pp wr]
  Listen      _ -> [pText "listen"]
  Heard    wr o -> loop o [pText "heard", pIndent $ pprintWriterStream pp wr]
  Pass        _ -> [pText "pass"]
  Passed   wr o -> loop o [pText "passed", pIndent $ pprintWriterStream pp wr]
  Catch   try _ -> tryLoop try
  Caught  err o -> loop o [pText "caught", pIndent $ pprintError pp err]
  NoError     o -> loop o [pText "success"]
  Throw   err   -> [pText "throw", pIndent $ pprintError pp err]
  Label   lbl o -> loop o [pText "label", pIndent $ pprintLabel pp lbl]
  Address lbl o -> loop o [pText "address", pIndent $ pprintLabel pp lbl]
  Fail    err   -> [pText "fail:", pSpace, pIndent [pText err]]
  where
    depth = pprintInstructionDepth pp
    loop o msg = msg ++ deep depth o
    deep depth o =
      if depth<=0 then [] else pprintInstruction (pp{pprintInstructionDepth=depth-1}) o
    envLoop rd env = envDeep [pText "env"] rd env
    envDeep t rd env = case env of
      Env rd env -> envDeep (t ++ [pText ".env"]) rd env
      env        -> t ++ [pIndent $ pprintInstruction pp env ++ [pIndent $ pprintReaderEnv pp rd]]
    tryLoop try = tryDeep [pText "try"] try
    tryDeep t try = case try of
      Catch try _ -> tryDeep (t ++ [pText ".try"]) try
      try         -> t ++ pprintInstruction pp try

-- | Prints the current instruction using 'pprintInstruction', then prints the current
-- 'ComputerState' after it.
pprintComputer
  :: forall lbl fu err rd wr st m o . PPrintComputer lbl fu err rd wr st o
  -> ComputerT lbl fu err rd wr st m o
  -> [PPrint]
pprintComputer pp o = let reg = registers o in concat $
  [ pprintInstruction pp (programCounter o)
  , mprint "environment" (pprintReaderEnv    pp) (readerEnv    reg)
  , mprint "stream"      (pprintWriterStream pp) (writerStream reg)
  , mprint "registers"   (pprintStateTable   pp) (stateTable   reg)
  ] where
      mprint :: String -> (t -> [PPrint]) -> t -> [PPrint]
      mprint lbl prin o = let txt = prin o in
        if null txt then [] else [pText lbl]++txt

