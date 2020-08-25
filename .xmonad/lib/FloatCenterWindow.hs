module FloatCenterWindow (
        centerFloatingWindow,
        makeFloatingCenterWindow
    ) where

import Graphics.X11.Types

import XMonad.Core
import XMonad.Operations
import qualified XMonad.StackSet as W

centerFloatingWindow :: Window -> X ()
centerFloatingWindow win = do
    (_, W.RationalRect x y w h) <- floatLocation win
    windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)

makeFloatingCenterWindow :: Window -> X ()
makeFloatingCenterWindow win = do
    (_, W.RationalRect x y w h) <- floatLocation win
    case (x, y, w, h) of
        (0.2, 0.2, 0.6, 0.6) -> windows $ W.float win (centeredRect 0.5 0.5)
        _                    -> windows $ W.float win (centeredRect 0.6 0.6)

centeredRect :: Rational -> Rational -> W.RationalRect
centeredRect w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h
