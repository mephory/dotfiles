module PictureInPicture (
    togglePip,
    enable,
    disable,
    pipEventHook,
    pipPP
) where

import Graphics.X11.Xlib.Atom (getAtomName, getAtomNames)
import Control.Monad.IO.Class (liftIO)

import Control.Monad
import Data.Monoid
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras (Event(CrossingEvent, ClientMessageEvent, ev_event_type, ev_window, ev_message_type, ev_atom))
import XMonad
import XMonad.Core
import XMonad.Operations
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import UnicodeUtils (appendFileUtf8)

data PipStorage = PipStorage (Maybe (Window, Bool))
    deriving (Typeable, Read, Show)

instance ExtensionClass PipStorage where
    initialValue = PipStorage Nothing
    extensionType = PersistentExtension

-- position should
-- * MUST be 16:9
-- * gravitate towards corners
-- * 20% of screen size
-- pos1 = W.RationalRect 0.60 0.55 0.35 0.35
-- pos2 = W.RationalRect 0.60 0.05 0.35 0.35

getPosition False = W.RationalRect 0.05 0.55 0.35 0.35
getPosition True = W.RationalRect 0.05 0.05 0.35 0.35

togglePip :: X ()
togglePip = do
    PipStorage win <- XS.get
    case win of
        Just _ -> disable
        Nothing -> withFocused enable

enable :: Window -> X ()
enable w = do
    XS.modify $ \_ -> PipStorage (Just (w, False))
    windows $ W.float w (getPosition False)
    unmanage w
    reveal w

disable :: X ()
disable = do
    PipStorage win <- XS.get
    case win of
        Just (w, _) -> manage w >> (XS.modify $ \_ -> PipStorage Nothing) >> windows (W.focusWindow w)
        Nothing          -> mempty

pipEventHook e@(CrossingEvent {ev_event_type=t, ev_window=win}) = do
  (PipStorage ps) <- XS.get
  case ps of
    Just (w, p) -> do
      if w == (ev_window e) then selectPosition (not p) else mempty
      withDisplay $ \d -> io $ raiseWindow d w
      refreshWindow
    Nothing -> mempty
  return $ All True

pipEventHook e = do
  (PipStorage ps) <- XS.get
  case ps of
    Just (w, p) -> do
      withDisplay $ \d -> io (raiseWindow d w) >> selectPosition (not p)
    Nothing -> mempty
  return $ All True

isPip :: Window -> X Bool
isPip w = do
    (PipStorage win) <- XS.get
    case win of
        Just (x, _) -> return $ x == w
        Nothing        -> return False

selectPosition :: Bool -> X ()
selectPosition p = do
  (PipStorage ps) <- XS.get
  case ps of
    Just (w, _) -> XS.modify $ \_ -> PipStorage . Just $ (w, p)
    Nothing -> mempty

refreshWindow :: X ()
refreshWindow = do
  (PipStorage ps) <- XS.get
  case ps of
    Just (win, p) -> setWindowPosition' win (getPosition p)
    Nothing -> mempty

setWindowPosition' :: Window -> W.RationalRect -> X ()
setWindowPosition' w (W.RationalRect x y ww wh) = do
    sr <- gets windowset >>= return . screenRect . W.screenDetail . W.current
    let scrX = floor $ x * (fromIntegral (rect_width sr)) + (fromIntegral (rect_x sr))
        scrY = floor $ y * (fromIntegral (rect_height sr)) + (fromIntegral (rect_y sr))
        scrW = floor $ ww * (fromIntegral (rect_width sr))
        scrH = floor $ wh * (fromIntegral (rect_height sr))
      in withDisplay $ \d -> io $ moveResizeWindow d w scrX scrY scrW scrH

pipPP :: (String -> String) -> X (Maybe String)
pipPP f = do
    (PipStorage pip) <- XS.get
    case pip of
        Just _ -> return . Just . f $ "PiP"
        Nothing -> return Nothing
