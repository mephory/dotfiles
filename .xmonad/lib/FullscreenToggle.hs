{-# LANGUAGE DeriveDataTypeable #-}
module FullscreenToggle (
    toggleFullscreen
  ) where

import System.FilePath.Posix (dropExtension, (</>))
import System.Environment (getEnv)
import System.Directory
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM)
import Data.List (stripPrefix, sort, isInfixOf)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Graphics.X11.Types
import qualified Data.Map as M

import XMonad.Core
import XMonad.Prompt
import XMonad.Operations
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- Wenn Fenster kein Fullscreen:
--   → Position speichern und Fullscreenen
-- Wenn Fenster Fullscreen:
--   → Gemerkte Position wiederherstellen oder nichts

type WindowPosition = (W.RationalRect, Bool)

data PositionStorage = PositionStorage (M.Map Window WindowPosition)
    deriving (Typeable,Read,Show)
instance ExtensionClass PositionStorage where
    initialValue = PositionStorage $ M.fromList []
    extensionType = PersistentExtension

fullscreenLocation :: W.RationalRect
fullscreenLocation = W.RationalRect 0 0 1 1

toggleFullscreen :: Window -> X ()
toggleFullscreen w = do
    full <- isFullscreen w
    if full
    then restoreWindow w
    else fullscreenWindow w

fullscreenWindow :: Window -> X ()
fullscreenWindow w = do
    pos <- currentWindowPosition w
    storePosition w pos
    windows $ W.float w fullscreenLocation

restoreWindow :: Window -> X ()
restoreWindow w = do
    pos <- retrievePosition w
    case pos of
        Just p -> setWindowPosition w p
        Nothing -> mempty

setWindowPosition :: Window -> WindowPosition -> X ()
setWindowPosition w (loc,isFloating) = do
    if isFloating
    then windows $ W.float w loc
    else windows $ W.sink w

storePosition :: Window -> WindowPosition -> X ()
storePosition w pos = do
    XS.modify $ alterPositionStorage (\_ -> Just pos) w

retrievePosition :: Window -> X (Maybe WindowPosition)
retrievePosition w = do
    (PositionStorage m) <- XS.get
    return $ M.lookup w m

alterPositionStorage :: (Maybe WindowPosition -> Maybe WindowPosition) -> Window -> PositionStorage -> PositionStorage
alterPositionStorage f k (PositionStorage m) = PositionStorage $ M.alter f k m

currentWindowPosition :: Window -> X WindowPosition
currentWindowPosition w = do
    loc <- currentLocation w
    float <- isFloating w
    return $ (loc, float)

currentLocation :: Window -> X W.RationalRect
currentLocation w = do
    (_, pos) <- floatLocation w
    return pos

isFloating :: Window -> X Bool
isFloating w = withWindowSet $ \s ->
    return $ M.member w (W.floating s)

isFullscreen :: Window -> X Bool
isFullscreen w = currentLocation w >>= (return . (==fullscreenLocation))
