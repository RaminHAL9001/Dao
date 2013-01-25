-- "src/Dao/Resource.hs"  prevents many threads from updating a portion
-- of a data structure within DMVar a state variable.
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

module Dao.Resource where

import           Dao.Debug.OFF
import           Dao.String
import           Dao.Object
import qualified Dao.Tree as T

import qualified Data.Map as M

import           Control.Exception
import           Control.Monad.Reader

newDMVarsForResource
  :: Bugged r
  => String
  -> String
  -> stor Object
  -> stor (DQSem, Maybe Object)
  -> ReaderT r IO (Resource stor ref)
newDMVarsForResource dbg objname unlocked locked = do
  content <- dNewMVar xloc (dbg++'(':objname++".resource)") (unlocked, locked)
  return $
    Resource
    { resource        = content
    , updateUnlocked  = error "Resource.updateUnlocked is not defined"
    , lookupUnlocked  = error "Resource.lookupUnlocked is not defined"
    , updateLocked    = error "Resource.updateLocked is not defined"
    , lookupLocked    = error "Resource.lookupLocked is not defined"
    }

-- | Returns the unlocked portion of the resource. Locked resources are still being modified, so
-- their not-yet-updated values are returned.
getUnlockedResource :: Bugged r => Resource stor ref -> ReaderT r IO (stor Object)
getUnlockedResource r = fmap fst (dReadMVar xloc (resource r))

newStackResource :: Bugged r => String -> [T.Tree Name Object] -> ReaderT r IO StackResource
newStackResource dbg initStack = do
  resource <- newDMVarsForResource dbg "StackResource" (Stack initStack) (Stack [])
  return $
    resource
      { updateUnlocked = stackDefine
      , lookupUnlocked = stackLookup
      , updateLocked   = stackDefine
      , lookupLocked   = stackLookup
      }

newTreeResource :: Bugged r => String -> T.Tree Name Object -> ReaderT r IO TreeResource
newTreeResource dbg initTree = do
  resource <- newDMVarsForResource dbg "TreeResource" initTree T.Void
  let updater ref obj t = T.update ref (const obj) t
  return $
    resource
    { updateUnlocked = updater
    , lookupUnlocked = T.lookup
    , updateLocked   = updater
    , lookupLocked   = T.lookup
    }

newMapResource :: Bugged r => String -> M.Map Name Object -> ReaderT r IO MapResource
newMapResource dbg initMap = do
  resource <- newDMVarsForResource dbg "MapResource" initMap M.empty
  let updater ref obj m = M.update (const obj) ref m
  return $
    resource
    { updateUnlocked = updater
    , lookupUnlocked = M.lookup
    , updateLocked   = updater
    , lookupLocked   = M.lookup
    }

newDocResource :: Bugged r => String -> T_tree -> ReaderT r IO DocResource
newDocResource dbg docdata = do
  resource <- newDMVarsForResource dbg "DocResource" (initDoc docdata) (NotStored T.Void)
  let lookup ref d = T.lookup ref (docRootObject d)
      updater ref obj d = d{ docRootObject = T.update ref (const obj) (docRootObject d) }
  return $
    resource
    { updateUnlocked = \ref obj d -> (updater ref obj d){ docModified = 1 + docModified d }
    , updateLocked   = updater
    , lookupUnlocked = lookup
    , lookupLocked   = lookup
    }

-- | Not intended for general use, this function modifies the "locked" and "unlocked" DMVars in
-- rapid succession, lets you modify the contents of both at the same time. This function is used by
-- both updateResource and readResource.
modifyResource
  :: Bugged r
  => Resource stor ref
  -> (stor Object -> stor (DQSem, Maybe Object) -> ReaderT r IO (stor Object, stor (DQSem, Maybe Object), a))
  -> ReaderT r IO a
modifyResource rsrc fn = dModifyMVar xloc (resource rsrc) $ \ (unlocked, locked) ->
  fn unlocked locked >>= \ (unlocked, locked, a) -> return ((unlocked, locked), a)

-- | Modify the contents of a 'Dao.Object.Resource' /without/ locking it. Usually, it is better to
-- use 'Dao.Object.updateResource' or 'Dao.Object.updateResource_', but there are situations where
-- atomic updates are not necessary and you can skip the overhead necessary to lock a reference, or
-- you simply need to dump a lot of values directly into the unlocked store in a single, atomic
-- mutex operation.  Using 'Dao.Object.dModifyUnlocked' will not effect any of the currently locked
-- items, and once the items have been unlocked, they may overwrite the values that were set by the
-- evaluation of this function.
modifyUnlocked
  :: Bugged r
  => Resource stor ref
  -> (stor Object -> ReaderT r IO (stor Object, a))
  -> ReaderT r IO a
modifyUnlocked rsrc runUpdate = modifyResource rsrc $ \unlocked locked ->
  runUpdate unlocked >>= \ (unlocked, a) -> return (unlocked, locked, a)

-- | Is to 'Dao.Object.modifyUnlocked', what to 'Dao.Debug.dModifyMVar_' is to
-- 'Dao.Debug.dModifyMVar'.
modifyUnlocked_
  :: Bugged r
  => Resource stor ref
  -> (stor Object -> ReaderT r IO (stor Object))
  -> ReaderT r IO ()
modifyUnlocked_ rsrc runUpdate = modifyResource rsrc $ \unlocked locked ->
  runUpdate unlocked >>= \unlocked -> return (unlocked, locked, ())

inEvalDoModifyUnlocked :: Resource stor ref -> (stor Object -> ExecScript (stor Object, a)) -> ExecScript a
inEvalDoModifyUnlocked rsrc runUpdate = do
  xunit <- ask
  ce <- execScriptRun $ modifyUnlocked rsrc $ \stor -> do
    ce <- runExecScript (runUpdate stor) xunit
    case ce of
      CENext (stor, a) -> return (stor, CENext   a  )
      CEReturn obj     -> return (stor, CEReturn obj)
      CEError  obj     -> return (stor, CEError  obj)
  returnContErr ce

inEvalDoModifyUnlocked_ :: Resource stor ref -> (stor Object -> ExecScript (stor Object)) -> ExecScript ()
inEvalDoModifyUnlocked_ rsrc runUpdate =
  inEvalDoModifyUnlocked rsrc $ \stor -> runUpdate stor >>= \a -> return (a, ())

updateResource_
  :: Bugged r
  => Resource stor ref -- ^ the resource to access
  -> ref -- ^ the address ('Dao.Object.Reference') of the 'Dao.Object.Object' to update
  -> (m Object -> Maybe Object) -- ^ checks the value returned by the above function, if it returns true, the update is executed.
  -> (Maybe Object -> m Object) -- ^ converts a value returned by the 'lookupItem' function to the type returned by this function.
  -> (m Object -> ReaderT r IO (m Object)) -- ^ a function for updating the 'Dao.Object.Object'
  -> ReaderT r IO (m Object)
updateResource_ rsrc ref toMaybe fromMaybe runUpdate = do
  let modify fn = modifyResource rsrc fn
      release sem = do -- remove the item from the "locked" store, signal the semaphore
        modify (\unlocked locked -> return (unlocked, updateLocked rsrc ref Nothing locked, ()))
        dSignalQSem xloc sem -- even if no threads are waiting, the semaphore is signaled.
      errHandler sem (SomeException e) = release sem >> dThrow xloc e
      updateAndRelease sem item = dHandle xloc (errHandler sem) $ do
        item <- runUpdate item
        modify $ \unlocked locked -> return $
          (updateUnlocked rsrc ref (toMaybe item) unlocked, updateLocked rsrc ref Nothing locked, ())
        dSignalQSem xloc sem
        return item
      waitTryAgain sem = dWaitQSem xloc sem >> updateResource_ rsrc ref toMaybe fromMaybe runUpdate
  join $ modify $ \unlocked locked -> case lookupLocked rsrc ref locked of
    Just (sem, _) -> return (unlocked, locked, waitTryAgain sem)
    Nothing       -> do
      sem <- dNewQSem xloc "updateResource" 0
      let item = lookupUnlocked rsrc ref unlocked
      return (unlocked, updateLocked rsrc ref (Just (sem, item)) locked, updateAndRelease sem (fromMaybe item))

-- | This function inserts, modifies, or deletes some 'Dao.Object.Object' stored at a given
-- 'Dao.Object.Reference' within this 'Resource'. Evaluating this function will locks the
-- 'Resource', evaluate the updating function, then unlocks the 'Resource', and it will take care of
-- unlocking if an exception occurs. When the 'Resource' is locked, any other thread that needs to
-- use 'updateResource' will be made to wait on a 'Dao.Debug.DQSem' until the thread that is
-- currently evaluating 'updateResource' completes.
updateResource
  :: Bugged r
  => Resource stor ref -- ^ the resource to access
  -> ref -- ^ the address ('Dao.Object.Reference') of the 'Dao.Object.Object' to update
  -> (Maybe Object -> ReaderT r IO (Maybe Object)) -- ^ a function for updating the 'Dao.Object.Object'
  -> ReaderT r IO (Maybe Object)
updateResource rsrc ref runUpdate = updateResource_ rsrc ref id id runUpdate

newtype ContErrMaybe a = ContErrMaybe { contErrMaybe :: ContErr (Maybe a) }

-- | Same function as 'updateResource', but is of the 'ExecScript' monad type.
inEvalDoUpdateResource
  :: Resource stor ref -- ^ the resource to access
  -> ref -- ^ the address ('Dao.Object.Reference') of the 'Dao.Object.Object' to update
  -> (Maybe Object -> ExecScript (Maybe Object)) -- ^ a function for updating the 'Dao.Object.Object'
  -> ExecScript (Maybe Object)
inEvalDoUpdateResource rsrc ref runUpdate = do
  xunit <- ask
  let toMaybe ce = case contErrMaybe ce of
        CENext Nothing  -> Nothing
        CENext (Just a) -> Just a
        CEReturn a      -> Just a
        CEError  _      -> Nothing
      fromMaybe item = ContErrMaybe{contErrMaybe = CENext item}
  execScriptRun >=> returnContErr $
    fmap contErrMaybe $ updateResource_ rsrc ref toMaybe fromMaybe $ \item ->
      fmap ContErrMaybe (runExecScript (runUpdate (toMaybe item)) xunit)

-- | Same function as 'readResource', but is of the 'ExecScript' monad type. Really, this is simply
-- @\resource reference -> 'Dao.Object.execRun' ('Dao.Object.readResource' resource reference)@
-- but it is included for the sake of completion -- to have a read-only counterpart to
-- 'Dao.Object.inEvalDoUpdateResource'.
inEvalDoReadResource :: Resource stor ref -> ref -> ExecScript (Maybe Object)
inEvalDoReadResource rsrc ref = execRun (readResource rsrc ref)

-- | This function will return an 'Dao.Object.Object' at a given address ('Dao.Object.Reference')
-- without blocking, and will return values even if they are locked by another thread with the
-- 'updateResource' function. If a value that is locked is accessed, the value returned is the value
-- that was set before it was locked. NOTE: this means it is possible that two subsequent reads of
-- the same 'Dao.Object.Reference' will return two different values if another thread completes
-- updating that value in between each evaluation of this function. The thread calling
-- 'readResource' is charged with the responsibility of determining whether or not this will cause
-- an inconsistent result, and caching of values looked-up by 'readResource' should be done to
-- guarantee consistency where multiple reads need to return the same value.
readResource :: Bugged r => Resource stor ref -> ref -> ReaderT r IO (Maybe Object)
readResource rsrc ref = modifyResource rsrc $ \unlocked locked ->
  (\maybeObj -> return (unlocked, locked, maybeObj)) $ case lookupLocked rsrc ref locked of
    Nothing       -> lookupUnlocked rsrc ref unlocked
    Just (_, obj) -> obj

-- | Operating on a 'StackResource', push an item onto the stack.
pushStackResource :: Bugged r => StackResource -> ReaderT r IO ()
pushStackResource rsrc = modifyResource rsrc $ \unlocked locked ->
  return (stackPush T.Void unlocked, stackPush T.Void locked, ())

-- | Operating on a 'StackResource', push an item onto the stack.
popStackResource :: Bugged r => StackResource -> Stack Name Object -> ReaderT r IO ()
popStackResource rsrc stor = modifyResource rsrc $ \unlocked locked ->
  return (stackPop unlocked, stackPop locked, ())

