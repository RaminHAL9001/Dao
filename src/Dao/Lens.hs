-- "Dao/Lens.hs"  An ultra-lightweight lens library.
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

-- | This module defines a very simple 'Lens' data type inspired by Job Varnish's "lenses" package.
--
-- In the hopes of trying to be somewhat consistent with work that has come before mine, I borrowed
-- the terminology for some of these API functions from the "lenses" library, in particular the
-- 'fetch', 'update', and 'alter' functions. However what is called @fromGetSet@ in Varnish's Lens
-- library, in this module this function is simply called 'newLens', and has a the monadic version
-- 'newLensM'.
module Dao.Lens where

import           Prelude hiding ((.), id)
import           Control.Applicative
import           Control.Category
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State.Lazy

import           Data.Array.IArray
import           Data.Array.IO
import           Data.Array.Unboxed
import qualified Data.IntMap as I
import           Data.IORef
import qualified Data.Map    as M
import           Data.Monoid

-- | This is defined as @('Prelude.flip' 'pureFetch')@ a left-associative infix operator of
-- precedence 1. On the right of this infix operator is the data from which you want to fetch, on
-- the right is a 'Lens' used to retrieve the data from within it. For example, say you have a
-- large, complicated data type @complicated@ which contains a @Foo@ accessibly by the 'Lens' @foo@,
-- and @Foo@ contains a @Bar@ accessible by the 'Lens' @bar@, and all you want to do is get the
-- value @Bar@ from @complicated@. To do that you simply write:
--
-- @
-- complicated 'Dao.Lens.&' foo 'Dao.Lens.&' bar
-- @
--
-- This function requires a 'PureLens', but of course any 'Lens' polymorphic over the monadic type
-- @m@ can be used.
(&) :: c -> PureLens c e -> e
(&) = flip pureFetch
infixl 1 &

-- | Apply a sequence of 'Data.Monoid.Endo'functors (updating functions) to a container @c@. The
-- 'Data.Monoid.Endo'functors are applied in the order they appear in the list from left-to-right.
--
-- In category theory jargon, the 'Data.Monoid.mconcat'enation of the 'Data.Monoid.Dual' of each
-- 'Data.Monoid.Endo'functor in the list @[c -> c]@ is applied to the container @c@.
--
-- This function is useful for applying a series of updates on a container @c@, where each update is
-- constructed as pure 'Lens' function with the '($=)' or '($$)' operators.
--
-- @
-- data Item = Item Int Int deriving Show
--
-- foo :: 'Control.Monad.Monad' m => 'Lens' m Item Int
-- foo = 'newLens' (\\ (Item foo _) -> foo) (\\foo (Item _ bar) -> Item foo bar)
--
-- bar :: 'Control.Monad.Monad' m => 'Lens' m Item Int
-- bar = 'newLens' (\\ (Item _ bar) -> bar) (\\bar (Item foo _) -> Item foo bar)
--
-- theItem :: Item
-- theItem = Item 0 0
--
-- -- Now lets try this 'on' function with theItem...
-- main :: IO ()
-- main = 'System.IO.print' $
--     'on' theItem [foo 'Dao.Lens.$=' 25, bar 'Dao.Lens.$=' 200, foo 'Dao.Lens.$$' (* 4)]
-- @
--
-- The output of the above program will be:
--
-- > Item 100 200
on :: c -> [c -> c] -> c
on c fx = appEndo (getDual $ mconcat $ Dual . Endo <$> fx) c

-- | This is the 'on' function with the parameters 'Prelude.flip'ped.
by :: [c -> c] -> c -> c
by = flip on

-- | This is a function intended to be used with the 'on' function. It is used for constructing a
-- simple updating 'Data.Monoid.Endo'functor (updating function) that simply stores the element @e@
-- into a container @c@ using 'pureUpdate'. You would use this operator when building a list of
-- updates to pass to the 'on' function.
--
-- This function requires a 'PureLens', but of course any 'Lens' polymorphic over the monadic type
-- @m@ can be used.
--
-- This operator is visually similar to the bind operator used in Haskell's "do" notation @(<-)@.
-- Visually, it looks like you are writing a value into a lens, like in a procedural programming
-- language where the field you want to modify is on the left of the assignment operator, and the
-- value you want to write is on the right.
--
-- @
-- 'on' myData [fieldInData 'Dao.Lens.<~' 0]
-- @
(<~) :: PureLens c e -> e -> c -> c
(<~) = pureUpdate
infixr 0 <~

-- | This is a function intended to be used with the 'on' function. It is used for constructing a
-- simple updating 'Data.Monoid.Endo'functor (updating function) that updates element @e@ inside of
-- a container @c@ using 'pureAlter'. You would use this operator when building a list of updates to
-- pass to the 'on' function.
--
-- This function requires a 'PureLens', but of course any 'Lens' polymorphic over the monadic type
-- @m@ can be used.
--
-- This operator is visually similar to updating operators in popular C/C++ family of programming
-- languages. In this languages, to do an in-place update on a variable "x", for example to
-- increment an integer "x" by 5, you would write:
--
-- > x += 5;
--
-- Likewise this operator does an "in-place update." However you must provide a function on the
-- right-hand side of this operator that will perform the update:
--
-- @
-- 'on' myData [x 'Dao.Lens.$=' (+ 5)]
-- @
($=) :: PureLens c e -> (e -> e) -> c -> c
($=) lens f c = snd (pureAlter lens f c)
infixr 0 $=

----------------------------------------------------------------------------------------------------

-- | A 'Lens' is a 'Control.Monad.State.StateT' monadic function that 'Control.Monad.Trans.lift's a
-- monad @m@, and can perform an update on an element @e@ stored within a container @c@. The
-- container @c@ is the state of the 'Control.Monad.State.StateT' function.
--
-- The monadic function inside of the 'Lens' @newtype@ takes an optional updating function as an
-- argument. If the updating function is not provided then the 'Lens' must perform a 'fetch' from
-- the container @c@; if the updating function is provided then the 'Lens' must perform an 'alter'
-- on the container @c@. The function is but wrapped in a @newtype@ 'Lens' and instantiates
-- 'Control.Category.Category' to make for what I believe is a cleaner function composition
-- semantics, especially in that you can use 'Control.Category.>>>' for 'Lens' composition.
newtype Lens m c e =
  Lens
  { lensStateT :: Maybe (e -> m e) -> StateT c m e 
    -- ^ This function takes an element @e@ wrapped in a 'Prelude.Maybe'. If this function is passed
    -- an element @e@ contained in a 'Prelude.Just' constructor, the container should be updated
    -- with this element @e@. If this function is passed 'Prelude.Nothing', a value of type element
    -- @e@ should be retrieved from the container @c@ and returned.
    --
    -- As a law, passing a non-'Prelude.Nothing' argument should not modify container @c@ at all,
    -- although it is conceivable that, if your container includes something like a "number of times
    -- accessed" counter value, the container @c@ could be updated under these circumstances. But be
    -- warned that this is very bad programming practice.
  }

-- | This is a 'Lens' where the monad must be 'Control.Monad.Identity'. The advantage of using this
-- over 'Lens' (apart from the fact that it is easier to write it's type in your code) is that it
-- guarantees access to your container must be pure, with no IO.
type PureLens c e = Lens Identity c e

instance Monad m => Category (Lens m) where
  id = newLens id (\o -> const o)
  (Lens bc) . (Lens ab) = Lens $ \c -> StateT $ \a -> case c of
    Nothing -> evalStateT (ab Nothing) a >>= evalStateT (bc Nothing) >>= \c -> return (c, a)
    Just  c -> do
      (b, a) <- runStateT (ab  Nothing) a -- fetch b from a
      (c, b) <- runStateT (bc $ Just c) b -- update c into b, updating b
      (_, a) <- runStateT (ab $ Just (return . const b)) a -- update updated b into a
      return (c, a)

-- | This class lets you define a relation between containers and elements so that you can constrain
-- container types in the contexts of functions. For example, lets say you want to make your
-- stateful monadic function polymorphic over the state type, but you want to constrain this state
-- type so that it must at least have certain lenses defined to access or modify the state.
-- @
-- newtype Comment = Comment 'Prelude.String'
-- @
--
-- @
-- instance 'Data.Monoid.Monoid' Comment where
--   'Data.Monoid.mempty' = Comment "";
--   'Data.Monoid.mappend' (Comment a) (Comment b) = Comment $ a++b
-- @
--
-- @
-- data Expr = Const Comment 'Prelude.Int' | Expr Comment Expr 'Prelude.Char' Expr
-- @
--
-- @
-- instance 'Contains' Comment Expr where
--   lens = 'newLens' (\\e   -> case e of { Const c _ -> c; Expr c _ _ _ -> c; })
--                  (\\e c -> case e of { Const _ x -> Const x c; Expr _ x y z -> Expr c x y z; })
-- @
--
-- @
-- data ExprError = ExprError 'Prelude.String' Expr
-- @
--
-- @
-- instance 'Contains' Comment ExprError where
--   lens = 'newLens' (\\ (ExprError _ c)   -> 'pureFetch' 'lens' c)
--                  (\\ (ExprError x c) e -> 'pureAlter' 'lens' c e)
-- @
--
-- @
-- appendComment :: 'Contains' Comment st => Comment -> 'Control.Monad.State.StateT' st m ()
-- appendComment com = 'Control.Monad.State.modify' $ 'pureUpdate' 'lens' ('Data.Monoid.<>' com)
-- @
--
-- In the above example, the 'Contains' type class allowed us defined a stateful function
-- 'appendComment' that can operate on any state, so long as the state 'Contains' a Comment.
class Contains e c where { lens :: Monad m => Lens m c e; }

-- | This class lets you define a way to focus a 'Lens', which means to create a lens that operates
-- on an element at a specific index of the container @c@. 'FocusesWith' data types of course must be
-- containers with indexable elements, like arrays or maps.
class Monad m => FocusesWith index m c e where
  focus :: index -> Lens m c e

-- | This function allows you to construct a new 'Lens' without using a
-- 'Control.Monad.State.Lazy.StateT' transformer. Instead provide two functions, a function that
-- 'fetch's an element @e@ from the container @c@, and a function that 'update's an element @e@ into
-- the container.
newLensM :: Monad m => (c -> m e) -> (e -> c -> m c) -> Lens m c e
newLensM fetch update = Lens $ maybe (get >>= lift . fetch) $ \upd ->
  get >>= \st -> lift (fetch st >>= upd) >>= \o -> lift (update o st) >>= put >> return o

-- | This function is similar to 'newLensM', but the two parameter functions are pure functions.
-- *NOTE:* that the 'Lens' constructed by this function can be used as both a normal 'Lens' or a
-- 'PureLens', only the 'fetch' and 'update' parameters are pure.
newLens :: Monad m => (c -> e) -> (e -> c -> c) -> Lens m c e
newLens fetch update = newLensM (return . fetch) (\o -> return . update o)

-- | It will convert a 'Lens' to a 'Control.Monad.State.Lazy.StateT' monad transformer that returns
-- the element @e@ of the container @c@.
getWithLens :: Monad m => Lens m c e -> StateT c m e
getWithLens (Lens lens) = lens Nothing

-- | It will convert a 'Lens' to a 'Control.Monad.State.Lazy.StateT' monad transformer that inserts
-- an element @e@ into the container @c@.
putWithLens :: Monad m => Lens m c e -> (e -> StateT c m e)
putWithLens (Lens lens) = lens . Just . const . return

-- | It will convert a 'Lens' to a 'Control.Monad.State.Lazy.StateT' monad transformer that modifies
-- the element @e@ within the container.
modifyWithLens :: Monad m => Lens m c e -> ((e -> m e) -> StateT c m e)
modifyWithLens (Lens lens) = lens . Just

-- | Use a 'Lens' to read an element @e@ within a container @c@.
fetch :: Monad m => Lens m c e -> c -> m e
fetch (Lens lens) = evalStateT (lens Nothing)

-- | Similar to 'fetch', but performs the lookup purely, without Monadic side-effects.
pureFetch :: PureLens c e -> c -> e
pureFetch lens = runIdentity . fetch lens

-- | Use a 'Lens' to write an element @e@ within a container @c@.
--
-- Notice that the type signature of this function is defined such that multiple 'alter' functions
-- can be composed using the 'Control.Monad.>>=' operator, for example:
--
-- @
--     'update' lastName  "Curry"    newPersonRecord
-- >>= 'update' firstName "Haskell"
-- >>= 'update' born      1900
-- >>= 'update' died      1981
-- @
update :: Monad m => Lens m c e -> e -> c -> m c
update (Lens lens) o = execStateT (lens $ Just $ const $ return o)

-- | Similar to 'fetch', but performs the update purely, without Monadic side-effects.
pureUpdate :: PureLens c e -> e -> c -> c
pureUpdate lens o = runIdentity . update lens o

-- | Uses 'fetch' to take an element @e@ from the container @c@, modifies the element @e@ using the
-- given function, then puts the element @e@ back into the container @c@ using 'update'.
alter :: Monad m => Lens m c e -> (e -> m e) -> c -> m (e, c)
alter (Lens lens) f = runStateT (lens $ Just f)

-- | Similar to 'alter' but requires a 'PureLens' and performs an update with a pure function.
pureAlter :: PureLens c e -> (e -> e) -> c -> (e, c)
pureAlter lens f = runIdentity . alter lens (return . f)

----------------------------------------------------------------------------------------------------

-- | Use 'Control.Monad.Trans.Class.lift' to lift the operating monad of the 'Lens'. This is useful
-- when you want to modify the behavior of an existing 'Lens' by changing the monad in which 'update'
-- and 'fetch' operate. One example of how this could be used is if you want 'update' or 'fetch' to
-- be able to throw an error message, you could lift the monad into an
-- 'Control.Monad.Except.ExceptT' monad.
--
-- @
-- data Container = Container ('Prelude.Maybe' 'Prelude.Int') ('Prelude.Maybe' 'Prelude.String')
-- 
-- maybeInt :: 'Prelude.Monad' m => 'Lens' m Container ('Prelude.Maybe' 'Prelude.Int')
-- maybeInt = 'newLens' (\ (Container i _) -> i) (\i (Container _ s) -> Container i s)
-- 
-- maybeString :: 'Prelude.Monad' m => 'Lens' m Container ('Prelude.Maybe' 'Prelude.String')
-- maybeString = 'newLens' (\ (Container _ s) -> s) (\s (Container i _) -> Container i s)
-- 
-- required :: 'Prelude.Monad' m => 'Prelude.String' -> 'Lens' m Container (Maybe element) -> 'Lens' ('Control.Monad.Except.ExceptT' 'Prelude.String' m) Container element
-- required fieldName lens = 'liftLens' lens 'Control.Category.>>>'
--     'maybeLens' 'Prelude.True' ('Control.Monad.Except.throwError' $ fieldName++" is undefined") 'Prelude.return'
-- 
-- requireInt :: 'Prelude.Monad' m => 'Lens' ('Control.Monad.Except.ExceptT' String m) Container 'Prelude.Int'
-- requireInt = required "int" maybeInt
-- 
-- requireString :: 'Prelude.Monad' m => 'Lens' ('Control.Monad.Except.ExceptT' String m) Container 'Prelude.String'
-- requireString = required "string" maybeString
-- @
liftLens :: (Monad m, MonadTrans t, Monad (t m), MonadFix (t m)) => Lens m c e -> Lens (t m) c e
liftLens (Lens lens) = Lens $ \element -> case element of
  Nothing      -> StateT $ lift . runStateT (lens Nothing)
  Just element -> StateT $ \st -> mfix $ \ (o, _) -> do
    o <- element o
    lift $ runStateT (lens $ Just $ const $ return o) st
    -- TODO: testing, I am not sure exactly if MonadFix will work as I think it will here.

----------------------------------------------------------------------------------------------------

-- | Creates a lens that operates on a type stored in a 'Prelude.Maybe' data type. When 'fetch'ing a
-- data type, the first boolean parameter is ignored, the second function should return a default
-- value to be used when 'fetch'ing from a 'Prelude.Nothing' data type:
--
-- @
-- 'fetch' ('maybeLens' 'Prelude.False' $ return "") 'Prelude.Nothing'      -- ""
-- 'fetch' ('maybeLens' 'Prelude.False' $ return "") ('Prelude.Just' "red") -- "red"
-- 'fetch' ('maybeLens' 'Prelude.True'  $ return "") 'Prelude.Nothing'      -- ""
-- 'fetch' ('maybeLens' 'Prelude.True'  $ return "") ('Prelude.Just' "red") -- "red"
-- @
--
-- The boolean parameter determines the behavior of the lens when 'update'ing a value: if
-- 'Prelude.True', it will force the container to become 'Prelude.Just' regardless what the previous
-- value was, but if 'Prelude.False' it will only set the 'update'ed value if the container is
-- 'Prelude.Just', otherwise it stays 'Nothing' and the default 'update'ed value is returned.
--
-- @
-- 'update' ('maybeLens' 'Prelude.False' $ return "") 'Prelude.Nothing'      "blue" -- Nothing
-- 'update' ('maybeLens' 'Prelude.False' $ return "") ('Prelude.Just' "red") "blue" -- Just "blue"
-- 'update' ('maybeLens' 'Prelude.True'  $ return "") 'Prelude.Nothing'      "blue" -- Just "blue"
-- 'update' ('maybeLens' 'Prelude.True'  $ return "") ('Prelude.Just' "red") "blue" -- Just "blue"
-- @
--
maybeLens :: Monad m => Bool -> m e -> Lens m (Maybe e) e
maybeLens force deflt = newLensM (maybe deflt return) $ \o ->
  return . if force then const (Just o) else fmap (const o)

-- | To create a 'Lens' that focuses on an element of a dictionary, provide a lookup function (e.g.
-- 'Data.Map.lookup') and an alter function (e.g. 'Data.Map.alter'). Or just use the 'mapLens' or
-- 'intMapLens' functions predefined for the 'Data.Map.Map' and 'Data.IntMap.IntMap' data types,
-- respectively.
dictionaryLens
  :: (Monad m, Eq key, Ord key)
  => (key -> map o -> Maybe o)
  -> ((Maybe o -> Maybe o) -> key -> map o -> map o)
  -> key -> Lens m (map o) (Maybe o)
dictionaryLens lookup alter key = newLens (lookup key) $ \o -> alter (const o) key

-- | A 'Lens' that focuses on an element of an 'Data.Map.Map' with the key to that element.
mapLens :: (Monad m, Eq key, Ord key) => key -> Lens m (M.Map key o) (Maybe o)
mapLens = dictionaryLens M.lookup M.alter

-- | A 'Lens' that focuses on an element of an 'Data.IntMap.IntMap' with the key to that element.
intMapLens :: Monad m => Int -> Lens m (I.IntMap o) (Maybe o)
intMapLens = dictionaryLens I.lookup I.alter

instance (Monad m, Eq key, Ord key) =>
  FocusesWith key m (M.Map key o) (Maybe o) where { focus=mapLens; }

instance Monad m => FocusesWith Int m (I.IntMap o) (Maybe o) where { focus=intMapLens; }

----------------------------------------------------------------------------------------------------

-- | Create a lens that accesses an element at the given index in an array. Evaluates to
-- 'Prelude.undefined' if the index is out of bounds.
arrayLens :: (Monad m, Ix i, IArray arr o) => i -> Lens m (arr i o) o
arrayLens i = newLens (! i) (\o -> (// [(i, o)]))

instance (Monad m, Ix i, IArray Array o) => FocusesWith i m (Array i o) o where { focus=arrayLens; }

instance (Monad m, Ix i, IArray UArray o) => FocusesWith i m (UArray i o) o where { focus=arrayLens; }

-- | Create a lens that accesses an element at the given index in an array. If the index is out of
-- bounds, calling 'fetch' on the 'Lens' will evaluate to 'Prelude.Nothing', and calling 'fetch'
-- will do nothing at all.
maybeArrayLens :: (Monad m, Ix i, IArray arr o) => i -> Lens m (arr i o) (Maybe o)
maybeArrayLens i = let lens = arrayLens i in Lens $ \o -> get >>= \arr ->
  if inRange (bounds arr) i
  then (case o of
          Nothing -> getWithLens lens
          Just  o -> modifyWithLens lens (o . Just >=> maybe (return $ arr!i) return)
       ) >>= return . Just
  else return Nothing

instance (Monad m, Ix i, IArray Array o) =>
  FocusesWith i m (Array i o) (Maybe o) where { focus=maybeArrayLens; }

instance (Monad m, Ix i, IArray UArray o) =>
  FocusesWith i m (UArray i o) (Maybe o) where { focus=maybeArrayLens; }

ioArrayLens :: (Monad m, MonadIO m, Ix i, MArray arr o IO) => i -> Lens m (arr i o) o
ioArrayLens i =
  newLensM (liftIO . flip readArray i) (\o arr -> liftIO $ writeArray arr i o >> return arr)

instance (Monad m, Applicative m, MonadIO m, Ix i, MArray IOArray o IO) =>
  FocusesWith i m (IOArray i o) o where { focus=ioArrayLens; }

instance (Monad m, Applicative m, MonadIO m, Ix i, MArray IOUArray o IO) =>
  FocusesWith i m (IOUArray i o) o where { focus=ioArrayLens; }

-- | Checks if the index is within the bounds of the array, does no lookup or update if the index is
-- out of bounds.
ioMaybeArrayLens :: (Monad m, MonadIO m, Ix i, MArray arr o IO) => i -> Lens m (arr i o) (Maybe o)
ioMaybeArrayLens i = let lens = ioArrayLens i in Lens $ \o -> do
  arr      <- get
  inBounds <- liftIO (getBounds arr) >>= return . flip inRange i
  if inBounds
  then (case o of
          Nothing -> getWithLens lens
          Just  o -> modifyWithLens lens (o . Just >=> maybe (liftIO $ readArray arr i) return)
       ) >>= return . Just
  else return Nothing

instance (Monad m, MonadIO m, Ix i, MArray IOArray o IO) =>
  FocusesWith i m (IOArray i o) (Maybe o) where { focus=ioMaybeArrayLens; }

instance (Monad m, MonadIO m, Ix i, MArray IOUArray o IO) =>
  FocusesWith i m (IOUArray i o) (Maybe o) where { focus=ioMaybeArrayLens; }

----------------------------------------------------------------------------------------------------

ioRefLens :: (Monad m, MonadIO m) => Lens m (IORef o) o
ioRefLens = newLensM (liftIO . readIORef) (\o ref -> liftIO $ writeIORef ref o >> return ref)

mVarLens :: (Monad m, MonadIO m) => Lens m (MVar o) o
mVarLens = newLensM (liftIO . readMVar) $ \o mvar ->
  liftIO $ modifyMVar_ mvar (const $ return o) >> return mvar

