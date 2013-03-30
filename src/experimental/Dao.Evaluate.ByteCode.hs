module Dao.Evaluate.ByteCode where

type Addr  = Int
type Rgstr = Name

-- | State transition resulting from evaluation of an object expression. This includes assignment to
-- variables and calls to any functions which would also assign variables.
data OpCode
  = LoadAccum       Object
  | OpDirect        ArithOp       Object -- Operate with an infix operator, the accumulator being the right hand, this object being the left.
  | DerefUpdate     UpdateOp -- Execute an update using the reference in the stack, and the value in the accumulator
  | PopEvaluate     ArithOp
  | PushStack                -- Transfer the value in the accumulator register onto the stack.
  | PopStack                 -- Pop the stack to the accumulator.
  | SubScriptOp              -- treats the accumulator as an index, retrieves an item from within the stack object.
  | InitNewObject   Object   -- Create a new 'Data.Map.Map', 'Data.IntMap.IntMap' or 'Dao.Tree.Tree' object to be constructed.
  | ReadyCallStack           -- creates a new stack in the stack of stacks known as the "call stack"
  | PushCallStack            -- pushes the accumulator onto the call stack
  | MakeNewList              -- uses the call stack to build a new list
  | MakeNewArray             -- uses the call stack to build a new array
  | MakeNewDict              -- uses the call stack to build a new dict
  | MakeNewSet               -- uses the call stack to build a new set
  | MakeNewIntMap            -- uses the call stack to build a new int map
  | MakeNewStruct            -- uses the call stack to build a new struct ('Data.Object.OTree')
  | CallBuiltin     ArithOp  -- calls a built-in arithmetic operator with the items on the call stack
  | CallLocal       Name     -- calls a function in the current module
  | CallGlobal               -- looks-up a function with the reference in the accumulator, calls that function with the call stack
  | SetLocation     Location
  | Jump            Addr
  | JumpIfNull      Addr
  | JumpIfNotNull   Addr
  | SetCatch        Rgstr         Addr
    -- ^ sets the local variable, and the address to jump to if an exception occurs.
  | PushLoopStack   -- load a new list of item into the loop stack
  | ShiftLoopStack  Rgstr         Addr -- give a register to load, and an address to jump to if the stack is empty
  | ClearLoopStack  Addr               -- give an address to jump to after the stack is cleared.
  | PushWithRef     -- push the current "with" reference
  | PopWithRef                         -- pop the current "with" reference
  | ThrowErr        
  | Return

-- | Used to represent a block of instructions in an program in memory. Each 'FuncExpr' and
-- 'Rule' translates into a 'CodeBlock'.
newtype CodeBlock = CodeBlock { codeBlockToArray :: Maybe (Array Int CtrlExpr) }

-- | State for translating a 'Dao.Object.CtrlExpr' to an intermediate representation
-- 'Dao.Object.CtrlBlock'.
data IntermState
  = IntermState
    { transInstructs :: [(Addr, OpCode)] -- 'OpCodes' emitted are stored into this list with their address.
    , transErrors    :: [(Location, UStr)] -- errors that occur but do not interrupt translation are recored here
    , stackFrame     :: M.Map Name Rgstr  -- maps names of local variables to register addresses
    , anonFuncs      :: IM.IntMap CodeBlock -- blocks of code created by lambda expressions
    , instCount      :: Addr -- counts the number of 'OpCodes' emitted, used to determine the address for jump instructions
    , regCount       :: Rgstr -- counts the number of register used
    }

initIntermState :: IntermState
initIntermState =
  IntermState
  { transInstructs = []
  , transErrors    = []
  , anonFuncs      = mempty
  , instCount      = 0
  , stackFrame     = []
  , regCount       = 0
  }

type IntermTrans a = State IntermState a

-- | Create an array of executable instructions from a 'IntermTrans' function.
makeInstrArray :: IntermTrans () -> Array Addr CtrlInstr
makeInstrArray it = let st = execState it in array (0, instCount st) (transInstructs st)

-- Emit an instruction
pushInstruction :: OpCode -> IntermTrans ()
pushInstruction u = modify $ \st ->
  st{transInstructs = transInstructs st ++ [(instCount st, u)], instCount = instCount st + 1}

pushStackFrame :: IntermState a -> IntermState a
pushStackFrame fn = do
  a <-modify (\st -> st{stackframe = M.empty : stackFrame st}) >> fn
  sf <- gets stackFrame
  modify (\st -> st{stackFrame = tail sf, regCount = regCount st - M.size (head sf)})
  return a

takeInstructions :: IntermState [OpCode]
takeInstructions = do
  inst <- gets transInstructs
  modify (\st -> st{transInstructs = [], instCount = instCount st + 1})
  return inst

placeInstructions :: ([OpCode] -> [OpCode]) -> IntermState ()
placeInstructions sb = modify (\st -> st{transInstructs = sb (transInstructs st)})

translatorError :: Location -> String -> IntermState ()
translatorError lc msg = modify (\st -> st{transErrors = transErrors st ++ [(lc, ustr msg)]})

-- | Translates a 'Dao.Object.ScriptExpr' into to a 'Dao.Object.OpCode'. Use 'execState' to retrieve
-- the resulting byte code from the state evaluated by this 'IntermState' monad.
transScriptExpr :: ScriptExpr -> IntermState ()
transScriptExpr expr = case expr of
  EvalObject lc   objExpr _       -> transObjExpr lc objExpr
  IfThenElse lc _ objExpr thn els -> do
    expr <- transObjExpr lc expr
    inst <- takeInstructions
    mapM_ transScriptExpr (map unComment (unComment thn))
    addr <- gets instCount
    placeInstructions (\sb -> inst ++ JumpIfNull expr addr : sb)
    mapM_ transScriptExpr (map unComment (unComment els))
    putResult (Branch objExpr thn els)
  TryCatch   lc  try  nm  ctch    -> do
    inst <- takeInstructions
    mapM_ transScriptExpr (map unComment (unComment try))
    pushStackFrame $ do
      ctch  <- gets instCount
      mapM_ transScriptExpr (map unComment (unComment ctch))
      noErr <- gets instCount
      placeInstructions (\sb -> SetCatch (unComment nm) ctch : inst ++ Jump noErr : sb)
  ForLoop    lc nm  objExpr loop  -> do
    obj   <- transObjExpr lc (unComment objExpr)
    pushInstruction (PushLoopStack obj)
    first <- takeInstructions
    pushStackFrame $ do
      top <- gets instCount
      --     Because of "break" statements, it is possible here to have jump instructions that can
      -- jump forward in the procedure. However, we don't know where to jump to until the remainder
      -- of the block is translated. With "if" blocks, we can evaluate a condition and then jump, so
      -- all we need is the number of instructions to jump over. But with a "for" block, we don't
      -- know where in the block a condition evluation and "break" statement will occur, there may
      -- even be many "break" statements, all jumping to the same place, so the solution for "if"
      -- blocks doesn't work here.
      --     The not-so-elegant solution for "for" blocks is to use an alternative "transScriptExpr"
      -- function (labeled here as "inLoop") to emit not 'Dao.Object.OpCode's but functions of the
      -- type @Int -> 'Dao.Object.OpCode'@, where every function simply ignores it's 'Int' parameter
      -- (a function constructed with 'Prelude.const' returning the 'Dao.Object.OpCode' unchanged).
      -- unless the emitted op codes is one that jumps forward (those generated by a "break" statement).
      --      The "inLoop" function basically checks for a "break" statement
      -- 'Dao.Object.ContinueExpr', and emits an appropriate 'Dao.Object.OpCode'. Anything other
      -- than 'Dao.Object.ContinueExpr' is translated by "transCtrlInst", and the emitted
      -- 'Dao.Object.OpCodes' are mapped against 'Prelude.const'.
      --     Once translation of the code block is complete, and we know the address to which we
      -- must jump, we pass this address to every function using @map ($address)@ constructing a
      -- list of 'Dao.Object.OpCode's that have all the "jump" instructions that were generated by
      -- "break" statements pointing to that correct address.
      let inLoop i = case i of
            ContinueExpr lc back _ objExpr -> do
              transObjExpr objExpr
              return $ case objExpr of
                Literal _ OTrue -> if back then [const (ShiftLoopStack (unComment nm) top)] else [ClearLoopStack]
                objExpr         -> if back then [const (JumpIfNotNull top)] else [JumpIfNotNull]
            _                              -> transScriptExpr i >> fmap (map const) takeInstructions
      instx <- fmap concat (mapM (inLoop . unComment) loop)
      last  <- pushInstruction (ShiftLoopStack (unComment nm top) >> takeInstructions
      end   <- gets instCount
      placeInstructions (\last -> first ++ ShiftLoopStack nm : map ($end) instx ++ last)
  ContinueExpr lc  _  _  _        -> translatorError lc $
    (if back then "continue" else "break")++" expression not within \"for\" loop"
  ReturnExpr   lc  objExpr        -> transObjExpr lc objExpr >>= pushInstruction .  Return
  WithDoc      lc  objExpr subsc  -> do
    ref <- transObjExpr lc objExpr
    pushInstruction (PushWithRef ref)
    mapM_ transScriptExpr (map unComment (unComment ctch))
    pushInstruction (PopWithRef  ref)

-- Translates a 'Dao.Object.ObjectExpr' to 'Dao.Object.OpCode's.
transObjExpr :: ObjectExpr -> IntermState ()
transObjExpr expr = case expr of
  Literal      lc obj           -> pi (LoadAccum obj)
  AssignExpr   lc targ op valu  -> do
    transObjExpr (unComment targ)
    pi PushStack
    transObjExpr (unComment valu)
    pi PopStack
    pi (DerefUpdate op)
  Equation     lc left op right -> do
    transObjExpr (unComment left)
    pi PushStack
    transObjExpr (unComment right)
    pi (PopEvaluate (unComment op))
  ArraySubExpr lc left _  right -> do
    transObjExpr (unComment left)
    pi PushStack
    transObjExpr (unComment right)
    pi SubScriptOpt
  FuncCall     lc name _ args   -> do
    pushArgs args
    pi $ case M.lookup builtinFuncs name of
      Nothing -> CallLocal name
      Just op -> CallBuiltIn op
  DictExpr     lc  n   _ args   -> makeObj op init args (return ()) where
    (op, init) =
      case n of
        n | n == ustr "dict"   -> (MakeNewDict  , ODict   mempty)
        n | n == ustr "intmap" -> (MakeNewIntMap, OIntMap mempty)
        n | n == ustr "list"   -> (MakeNewList  , OList   mempty)
        n | n == ustr "set"    -> (MakeNewList  , OSet    mempty)
        _ -> error ("parser created DictExpr with an invalid constructor: "++show name)
  ArrayExpr    lc bnds   args   -> makeObj MakeNewArray (OList mempty) args $ do
    pi ReadyCallStack
    pushArgs bnds
  LambdaCall   lc  ref   args   -> do
    evalObjExpr (unComment ref)
    pi PushStack
    pushArgs args
    pi PopStack
    pi CallGlobal
  StructExpr   lc  init  args   -> makebj MakeNewStruct T.Void args $ do
    transObjExpr (unComment init)
    pi PushStack 
  LambdaExpr   lc argv lamExpr  -> do
    let sub = execState initIntermState transObjExpr 
    modify $ \st ->
      let next = instCount st + 1
          errs = transErrors sub
          fref = IM.size (anonFuncs st)
      in  st{ transInstructs = transInstructs ++ [(next, LoadAccum (ORef (IntRef (fromIntegral fref)))]
            , anonFuncs = 
                if null errs
                  then  flip IM.union (stackFrame sub) $
                          IM.insert fref $
                            if instCount sub == 0
                              then  Nothing
                              else  Just (array (0, instCount sub - 1) (transInstructs sub))
                  else  anonFuncs st
            , instCount = next
            , transErrors = transErrors st ++ errs
            }
  ParenExpr    _  _    objExpr  -> transObjExpr (unComment objExpr)
  where
    pi = pushInstruction
    pushArgs args = do
      pi ReadyCallStack
      mapM_ (transObjExpr . unComment >=> \_ -> pi PushCallStack) (unComment args)
    makeObj op init args preConstruct = do
      let op = case M.lookup builtinFuncs name of
                  Nothing -> error ("unknown object builder function "++show name)
                  Just op -> op
      seq op $ pushArgs args
      preConstruct
      pi (InitNewObject init)
      pi op

data NewExecUnit
  = NewExecUnit
    { progCounter     :: Addr
    , accumulator     :: Object
    , currentLocation :: Location
    , lastUpdate      :: Object -- the result of the last global state update operation
    , registers       :: M.Map Name Object -- registers holding intermediate results
    , loopStack       :: [[Object]]
    , withRefStack    :: [Reference]
    , catchPoint      :: [(Int, Int)]
    }

initNewExecUnit :: NewExecUnit
initNewExecUnit =
  ExecUnit
  { progCounter     = 0
  , currentLocation = mempty
  , lastUpdate      = ONull
  , registers       = mepmty
  , progStack       = []
  , loopStack       = []
  , withRefStack    = []
  , catchPoint      = []
  }

type ExecStateTransIO = StateT IO NewExecUnit a
type ExecStateTrans   = State     NewExecUnit a

-- | Execute the instructions in the given array of 'Dao.Object.OpCode's, which was translated from
-- the Dao abstract syntax tree 'Dao.Object.ScriptExpr'. I went out of my way to make this execution
-- as lazy as possible, lifting the lazy evaluation into the IO monad only when it is absolutely
-- necessary, which is when a global variable needs to be read or updated. My hope is that lazy
-- evaluation will be easier for the runtime system to evaluate efficiently.
execInstrArray :: Array Int OpCode -> [Object] -> ExecStateTransIO (Bool, Object)
execInstrArray arr stack = do
  put $
    initNewExecUnit
    { registers =
        fold (\ (i, o) m -> IM.insert i o m) (zip (iterate (+1) 0) stack)
    }
  loop arr
  where
    loop arr = do
      st0 <- get
      -- first execute as many instructions as possible outside of the IO monad.
      let (err, st) = runState (scan arr) st0
      if err -- if an error occurred
        then
          case catchPoint st of
            []         -> put st >> return (Just object) -- no, exception cannot be caught
            (nm, pc):_ -> do -- yes, exception can be caught
              put (st{progCounter = pc, registers = IM.insert nm report (registers st)})
              loop arr
        else do -- execute a stateful instruction and loop
          put st
          case arr ! progCounter st of
            DerefUpdate  op             ->
            OpDirect     op    (ORef r) -> 
            CallFunc     ref    oxpx    -> 
            PopEvaluate  DEREF          ->  
            _ -> error "the stateless instruction scan should have picked up this instruction"
    scan arr = gets progCounter >>= \pc -> case arr!pc of
      LoadAccum     obj      -> modify (\st -> st{accumulator = obj}) >> scan ar
      OpDirect      op   obj -> modify (\st -> (infixOps!op) (accumulator st) obj) -- TODO: check the order of application, is it (accumulator + operand) or (operand + accumulator)?
      DerefUpdate   op       -> gets registers >>= \reg -> 
      Jump          pc       -> modify (\st -> st{progCounter = pc}) >> scan arr
      JumpIfNotNull pc       -> jumpIfNull not oxp pc
      JumpIfNull    pc       -> jumpIfNull id  oxp pc
      SetCatch      nm   pc  -> modify (\st -> st{catchPoint = Just (nm, pc)}) >> pcPlus1 >> scan arr
      IntermExpr    oxp      -> execObjExpr oxp >> pcPlus1 >> scan arr
      PushLoopStack oxp      ->
        execObjExpr oxp >>= \objx -> modify (\st -> st{loopStack = objx}) >> pcPlus1 >> scan arr
      ShiftLoopStack nm_ end -> do
        let nm = unComment nm_
        lstk <- gets loopStack
        if null lstk
          then modify (\st -> st{progCounter = end}) >> scan arr
          else do
            modify (\st -> st{loopStack = tail lstk, registers = IM.insert nm (head lstk) (registers st)})
            pcPlus1 >> scan arr
      ClearLoopStack        -> modify (\st -> st{loopStack = []}) >> scan arr
      PushWithRef   oxp     -> execObjExpr oxp >>= \ref -> case ref of
        ORef ref -> modify (\st -> st{withRefStack = ref : withRefStack st}) >> scan arr
        _        -> error "with statement must push a reference"
      PopWithRef            -> modify (\st -> st{withRefStack = tail (withRefStack st)}) >> scan arr
      ThrowErr      oxp     -> execObjExpr oxp >> return False
      Return        oxp     -> execObjExpr oxp >> return True
      _                     -> return True -- let "loop" handle all other instructions.
    pcPlus1 = modify (\st -> st{progCounter = progCounter st + 1})
    jumpIfNull is oxp pc = do
      notNull <- fmap objToBool (evalObjExpr oxp) -- TODO: evalObjExpr needs to be defined
      modify (\st -> st{progCounter = if is notNull then progCounter st + 1 else pc})
      scan arr


