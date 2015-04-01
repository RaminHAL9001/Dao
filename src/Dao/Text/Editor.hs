-- "Dao/Text/Editor.hs"  functions for pretty-printing with "Data.Text.Lazy"
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

-- | This module provides a basic text editor, mostly used for pretty printing. Although there is
-- nothing stopping you from binding the various monadic functions to key events, and defining a
-- method for displaying the contents of a "window" full of text in the editor state, and inventing
-- your own interactive text editor.
--
-- This 'EditorT' monadic function type provides a facility for associating metadata with each line
-- of text. This is useful for storing parser states so that after a line has been edited, the
-- parser can pick-up it's state where it left off and begin parsing again starting from the edited
-- line.
module Dao.Text.Editor where

import           Prelude hiding (id, (.))

import           Dao.Lens
import           Dao.TestNull
import           Dao.Text

import           Control.Applicative
import           Control.Category
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Writer

import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as Lazy

----------------------------------------------------------------------------------------------------

newtype PPrintState = PPrintState (Int, Int, StrictText, [Int], Int, Int, Int, LazyText)

instance TestNull PPrintState where
  testNull  = Lazy.null . (~> outputText)
  nullValue = PPrintState (80, 4, Strict.singleton '\n', [], 0, 0, 1, mempty)

instance Monoid PPrintState where
  mempty = nullValue
  mappend a b = with b
    [ outputText  $= mappend (a~>outputText)
    , currentLine $= ((a~>currentLine) +)
    , currentColumn <~ a~>currentColumn
    ]

pPrintStateLens :: Monad m => Lens m PPrintState (Int, Int, StrictText, [Int], Int, Int, Int, LazyText)
pPrintStateLens = newLens (\ (PPrintState o) -> o) (\o _ -> PPrintState o)

-- | Set the current line wrap width.
wrapWidth :: Monad m => Lens m PPrintState Int
wrapWidth = pPrintStateLens >>> tuple0

-- | Sets how many space characters a tab character will be replaced with on 'indent'.
tabWidth :: Monad m => Lens m PPrintState Int
tabWidth = pPrintStateLens >>> tuple1

-- | Whenever a 'nextLine' operation is executed, this string is copied into the 'outputText' and
-- the 'currentLine' is incremented.
newlineString :: Monad m => Lens m PPrintState StrictText
newlineString = pPrintStateLens >>> tuple2

-- | You can set a list of tab stops so every 'tab' operation steps to the next stop rather than the
-- default behavior of stepping forward to a column that is some multiple 'tabWidth'.
tabStops :: Monad m => Lens m PPrintState [Int]
tabStops = pPrintStateLens >>> tuple3

-- | This is the current indentation count. This counts the number of 'tab' opreations that have
-- been performed on the current line.
currentIndent :: Monad m => Lens m PPrintState Int
currentIndent = pPrintStateLens >>> tuple4

-- | The column counter.
currentColumn :: Monad m => Lens m PPrintState Int
currentColumn = pPrintStateLens >>> tuple5

-- | The line counter.
currentLine :: Monad m => Lens m PPrintState Int
currentLine = pPrintStateLens >>> tuple6

-- | The chunk of lazy 'Data.Text.Lazy.Text' produced so far.
outputText :: Monad m => Lens m PPrintState LazyText
outputText = pPrintStateLens >>> tuple7

----------------------------------------------------------------------------------------------------

newtype EditorT m o = EditorT{ runEditorT :: StateT PPrintState m o }

type Editor o = EditorT Identity o

runEditor :: Editor o -> State PPrintState o
runEditor (EditorT f) = f

instance Functor m => Functor (EditorT m) where { fmap f (EditorT o) = EditorT $ fmap f o; }

instance Monad m => Monad (EditorT m) where
  return = EditorT . return
  (EditorT o) >>= f = EditorT $ o >>= runEditorT . f

instance (Functor m, Monad m) => Applicative (EditorT m) where { (<*>) = ap; pure = return; }

instance Monad m => MonadState PPrintState (EditorT m) where { state = EditorT . state; }

instance Monad m => MonadWriter Lazy.Text (EditorT m) where
  writer (o, t) = state $ \st -> (,) o $ with st
    [ outputText    $= flip mappend t
    , currentColumn $= (+ (fromIntegral $ Lazy.length t))
    ]
  listen (EditorT f) = do
    (o, stA) <- lift $ runStateT f nullValue
    modify (<> stA) >> return (o, stA~>outputText)
  pass f = f >>= \ (o, f) -> state $ \st -> (o, with st [outputText $= f])

instance MonadTrans EditorT where { lift = EditorT . lift; }

instance MonadIO m => MonadIO (EditorT m) where { liftIO = EditorT . liftIO; }

----------------------------------------------------------------------------------------------------

lineBreak :: Monad m => Int -> EditorT m ()
lineBreak i =
  modify $ \st -> with st
    [ currentLine   $= (+ i)
    , currentColumn <~ 0
    , outputText    $= flip mappend $
        Lazy.replicate (fromIntegral i) $ Lazy.fromStrict $ st~>newlineString
    ]


