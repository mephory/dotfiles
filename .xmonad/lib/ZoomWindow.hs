-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.ZoomWindow
-- Copyright   :  (c) Robin Oberschweiber <mephory@mephory.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Robin Obercshweiber <mephory@mephory.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Toggle fullscreen for windows
--
-----------------------------------------------------------------------------

module ZoomWindow (
    -- * Usage
    -- $usage
    toggleZoom,
    zoomWindow,
    restoreWindow
  ) where

import Graphics.X11.Types
import XMonad.Core
import XMonad.Operations
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- $usage
-- Allows you to bind a key to "zoom" in or out of a window. This makes the
-- window fullscreen, or reverts it back to its original position, if it's
-- already fullscreen.
--
-- You can bind a "zoom" key like so:
--
-- import XMonad.Actions.ZoomWindow
--
-- , ((modm, xK_o), withFocused toggleZoom)


type WindowPosition = (W.RationalRect, Bool)

-- | Stores original positions of zoomed windows
data PositionStorage = PositionStorage (M.Map Window WindowPosition)
    deriving (Typeable,Read,Show)

instance ExtensionClass PositionStorage where
    initialValue = PositionStorage $ M.fromList []
    extensionType = PersistentExtension

fullscreenLocation :: W.RationalRect
fullscreenLocation = W.RationalRect 0 0 1 1

-- | Zoom the given window, or restore its original position if it's already
-- | zoomed.
toggleZoom :: Window -> X ()
toggleZoom w = do
    pos <- getWindowPosition w
    if isFullscreen pos
    then restoreWindow w
    else zoomWindow w

-- | Zoom the given window, making it fullscreen.
zoomWindow :: Window -> X ()
zoomWindow w = storePosition w >> (windows $ W.float w fullscreenLocation)

-- | Restore the original position of a window if it's zoomed.
restoreWindow :: Window -> X ()
restoreWindow w = do
    pos <- retrievePosition w
    case pos of
        Just p -> setWindowPosition w p
        Nothing -> mempty

-- | Move the given window to the given WindowPosition.
setWindowPosition :: Window -> WindowPosition -> X ()
setWindowPosition w (loc,isFloating) = do
    if isFloating
    then windows $ W.float w loc
    else windows $ W.sink w

-- | Store the current position of the given window.
storePosition :: Window -> X ()
storePosition w = do
    pos <- getWindowPosition w
    XS.modify (\(PositionStorage m) -> PositionStorage $ M.alter (\_ -> Just pos) w m)

-- | Get the stored position of the given window.
retrievePosition :: Window -> X (Maybe WindowPosition)
retrievePosition w = do
    (PositionStorage m) <- XS.get
    return $ M.lookup w m

-- | Get the current position of the given window.
getWindowPosition :: Window -> X WindowPosition
getWindowPosition w = do
    (_, loc) <- floatLocation w
    float <- withWindowSet $ return . M.member w . W.floating
    return $ (loc, float)

isFullscreen :: WindowPosition -> Bool
isFullscreen (loc,isFloating) = isFloating && loc == fullscreenLocation
