module FloatCenterWindow (
        floatCenterWindow
    ) where

import Graphics.X11.Types

import XMonad.Core
import XMonad.Operations
import qualified XMonad.StackSet as W

floatCenterWindow :: Window -> X ()
floatCenterWindow win = do
    (_, W.RationalRect x y w h) <- floatLocation win
    windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
    return ()
