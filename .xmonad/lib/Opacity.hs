
module Opacity (
        getOpacity,
        setOpacity,
        changeOpacity
    ) where

import Foreign.C.Types
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types
import qualified Data.Map as M

import XMonad
import XMonad.Core
import XMonad.Operations
import Control.Monad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

clamp :: Rational -> Rational -> Rational -> Rational
clamp min max n = minimum [maximum [min, n], max]

getOpacity :: Window -> X Rational
getOpacity w = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_WINDOW_OPACITY"
    mo <- io $ getWindowProperty32 dpy a w
    case mo of
        Just o -> return (opacityToRational (Foreign.C.Types.CUInt . fromIntegral . head $ o))
        Nothing -> return (1/1)

setOpacity :: Window -> Rational -> X ()
setOpacity w t = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_WINDOW_OPACITY"
    c <- getAtom "CARDINAL"
    io $ changeProperty32 dpy w a c propModeReplace [rationalToOpacity t]

changeOpacity :: Window -> Rational -> X ()
changeOpacity w step = do
    o <- getOpacity w
    setOpacity w (clamp 0.05 1.0 (o + step))

rationalToOpacity :: Integral a => Rational -> a
rationalToOpacity perc = round $ perc * 0xffffffff

opacityToRational :: Integral a => a -> Rational
opacityToRational opa = (fromIntegral opa)  / (fromIntegral 0xffffffff)
