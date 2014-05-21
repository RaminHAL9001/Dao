-- "src/Dao/Lib/ListEditor.hs"  built-in object for Dao programs that can
-- functions like a line editor, but for arbitrary types, not just strings.
-- 
-- Copyright (C) 2008-2014  Ramin Honary.
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

-- | This is a line-editor object, but it works with arbitrary lists of objects, but this will work
-- for editing arbitrary lists. You could use it to create an ordinary line editor by representing a
-- file as be a list of strings representing a file. each string could further be converted to a
-- StepList containing characters to edit the line. 
module Dao.Lib.ListEditor where

import           Dao.String
import           Dao.StepList
import           Dao.Predicate
import           Dao.PPrint
import qualified Dao.Binary as B
import           Dao.Interpreter
import           Dao.Interpreter.AST

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State

import           Data.Monoid
import qualified Data.IntMap as I
import           Data.Typeable

----------------------------------------------------------------------------------------------------

newtype ListEditor = ListEditor { listEditor :: StepList Object } deriving (Eq, Ord, Show, Typeable)

loadLibrary_ListEditor :: DaoSetup
loadLibrary_ListEditor = do
  daoClass (haskellType::ListEditor)

instance Monoid ListEditor where
  mempty = ListEditor mempty
  mappend (ListEditor a) (ListEditor b) = ListEditor (a<>b)

instance HasNullValue ListEditor where
  nullValue = mempty
  testNull (ListEditor sl) = slNull sl

instance ToDaoStructClass ListEditor where
  toDaoStruct = void $ do
    renameConstructor "ListEditor"
    "left"  .=@ slLeftOfCursor  . listEditor
    "right" .=@ slRightOfCursor . listEditor

instance FromDaoStructClass ListEditor where
  fromDaoStruct = do
    constructor "ListEditor"
    left  <- optList "left"
    right <- optList "right"
    return $ ListEditor $ slFromLeftRight left right

instance PPrintable ListEditor where { pPrint = pPrintStructForm }

instance B.Binary ListEditor MethodTable where
  put (ListEditor sl) = B.put (slCursor sl) >> B.put (slLeftOfCursor sl ++ slRightOfCursor sl)
  get = B.get >>= \cur ->
    ListEditor . slCursorTo cur . uncurry slFromLeftRight . splitAt cur <$> B.get

instance ReadIterable ListEditor Object where
  readForLoop (ListEditor sl) = forM_ (slToList sl)

instance UpdateIterable ListEditor (Maybe Object) where
  updateForLoop (ListEditor sl) = fmap (ListEditor . slFromList (slCursor sl) .
    concatMap (maybe [] return)) . forM (fmap Just $ slToList sl)

instance ObjectClass ListEditor where { obj=new; fromObj=objFromHata; }

_getIndex :: [Object] -> Predicate err Int
_getIndex ix = case ix of
  [i] -> xmaybe (fromObj i)
  _   -> fail "must index ListEditor with a 1-dimentional integer value"

_withRange :: String -> (Int -> Int -> Exec a) -> [Object] -> Exec a
_withRange func f ox = case ox of
  []  -> execThrow $ obj $ [obj func, obj "expects one or two integer paramters for range selection"]
  [a] -> do
    a <- xmaybe (fromObj a) <|>
      execThrow (obj $ [obj func, obj "index parameter received is not an integer value"])
    f (min 0 a) (max 0 a)
  [a, b] -> do
    let param msg a = xmaybe (fromObj a) <|>
          (execThrow $ obj $
            [obj func, obj $ msg++" range parameter received is not of an integer type"])
    param "first" a >>= \a -> param "second" b >>= \b -> f (min a b) (max a b)
  _ -> execThrow $
    obj [obj func, obj "received parameters which are neither an integer index nor a range"]

instance ObjectLens ListEditor Int where
  updateIndex i f = do
    sl <- slCursorTo i . listEditor <$> get
    (o, right) <- pure $ case slRightOfCursor sl of
      []   -> (Nothing, [])
      o:ox -> (Just  o, ox)
    (result, o) <- withInnerLens o f
    put $ ListEditor $ case o of
      Nothing -> sl{ slRightOfCursor =   right, slLength = slLength sl - 1 }
      Just  o -> sl{ slRightOfCursor = o:right }
    return result
  lookupIndex i = (slIndex i . listEditor <$> get) >>= xmaybe

instance ObjectFunctor ListEditor Int where
  objectFMap f = do
    (ListEditor sl) <- get
    (>>=put) $ fmap (ListEditor . slCursorTo (slCursor sl) . slFromIntMap . I.fromList . concat) $
      mapM (fmap snd . withInnerLens [] . uncurry f) $ concat $
        [ reverse $
            if slCursor sl > 0
            then zip (map negate [1..slCursor sl]) (slLeftOfCursor sl)
            else []
        , zip [0..] (slRightOfCursor sl)
        ]

instance ObjectFunctor ListEditor [Object] where { objectFMap f = objectFMap (\i -> f [obj i]) }

instance Sizeable ListEditor where { getSizeOf = return . obj . slLength . listEditor }

instance HataClass ListEditor where
  haskellDataInterface = interface "ListEditor" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest
    autoDefPPrinter >> autoDefToStruct >> autoDefFromStruct
    autoDefSizeable >> return ()
    autoDefReadIterable >> autoDefUpdateIterable >> autoDefTraverse
    defIndexer $ \ (ListEditor sl) -> fmap (flip slIndex sl) . predicate . _getIndex >=> xmaybe
    defIndexUpdater (\ix f -> predicate (_getIndex ix) >>= flip updateIndex f)
    defInitializer
      (\ox -> 
        if null ox
        then return mempty
        else predicate $ (\i -> ListEditor $ mempty{ slCursor=i }) <$> _getIndex ox
      )
      (\ (ListEditor sl) ox -> do
          let loop im ox = case ox of
                [] -> return $ ListEditor $ slCursorTo (slCursor sl) $ slFromIntMap im
                (i, o):ox -> case o of
                  InitSingle        o -> loop (I.insert i o im) ox
                  InitAssign ref op o -> do
                    i <- (fromObj <$> derefObject ref >>= xmaybe) <|>
                      execThrow
                        (obj [ obj "ListEditor constructor assigns value to non-integer type"
                             , obj (typeOfObj ref)
                             ])
                    o <- evalUpdateOp (Just $ RefObject ref NullRef) op o (I.lookup i im)
                    loop (I.alter (const o) i im) ox
          loop mempty (zip [(slCursor sl)..] ox)
      )
    let deref sl = case slRightOfCursor sl of { [] -> Nothing; o:_ -> Just o; }
    defDeref (return . deref . listEditor)
    defInfixOp SHL $ \ _ (ListEditor sl) o -> case o of
      OList ox -> return $ obj $ ListEditor (ox<++sl)
      o        -> return $ obj $ ListEditor (o <| sl)
    defInfixOp SHR $ \ _ (ListEditor sl) o -> case o of
      OList ox -> return $ obj $ ListEditor (ox++>sl)
      o        -> return $ obj $ ListEditor (o  |>sl)
    defInfixOp ADD $ \ _ (ListEditor a) o -> do
      (ListEditor b) <- xmaybe (fromObj o) <|> fail "added ListEditor object to non-ListEditor object"
      return $ obj $ ListEditor (a<>b)
    defMethod "insertLeft" $
      daoFunc
      { daoForeignFunc = \ (ListEditor sl) ox -> pure (snd (objConcat ox) <++ sl) >>= \sl -> return $
          (Just $ obj $ ListEditor sl, ListEditor sl)
      }
    defMethod "insertRight" $
      daoFunc
      { daoForeignFunc = \ (ListEditor sl) ox -> pure (snd (objConcat ox) ++> sl) >>= \sl -> return $
          (Just $ obj $ ListEditor sl, ListEditor sl)
      }
    defMethod "cursorTo" $
      daoFunc
      { daoForeignFunc = \ (ListEditor sl) ox -> predicate (_getIndex ox) >>= \i ->
          pure (slCursorTo i sl) >>= \sl -> return (deref sl, ListEditor sl)
      }
    defMethod "shift" $
      daoFunc
      { daoForeignFunc = \ (ListEditor sl) ox -> predicate (_getIndex ox) >>= \i ->
          pure (slCursorShift i sl) >>= \sl -> return (deref sl, ListEditor sl)
      }
    defMethod "copy" $
      daoFunc
      { daoForeignFunc = \ (ListEditor sl) -> _withRange "copy" $ \a b -> return $
          (Just $ obj $ ListEditor $ slCopyRelRange (a, b) sl, ListEditor sl)
      }
    defMethod "cut" $
      daoFunc
      { daoForeignFunc = \ (ListEditor sl) -> _withRange "cut" $ \a b -> return $
          (Just $ obj $ ListEditor $ slCopyRelRange (a, b) sl, ListEditor $ slDeleteRelRange (a, b) sl)
      }
    defMethod "copyRange" $
      daoFunc
      { daoForeignFunc = \ (ListEditor sl) -> _withRange "copyRange" $ \a b -> return $
          (Just $ obj $ ListEditor $ slCopyAbsRange (a, b) sl, ListEditor sl)
      }
    defMethod "cutRange" $
      daoFunc
      { daoForeignFunc = \ (ListEditor sl) -> _withRange "cut" $ \a b -> return $
          (Just $ obj $ ListEditor $ slCopyAbsRange (a, b) sl, ListEditor $ slDeleteAbsRange (a, b) sl)
      }
    defInfixOp ADD $ \ _ (ListEditor sl) ->
      xmaybe . fromObj >=> \ (ListEditor o) -> return (obj $ ListEditor $ sl <> o)

