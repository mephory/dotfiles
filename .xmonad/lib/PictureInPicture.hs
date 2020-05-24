module PictureInPicture (
    toggle,
    enable,
    disable,
    view,
    greedyView,
    pipEventHook,
    pipPP
) where

import Control.Monad
import Data.Monoid
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras (Event(CrossingEvent, ClientMessageEvent, ev_event_type, ev_window, ev_message_type))
import XMonad.Core
import XMonad.Operations
import XMonad.Hooks.ToggleHook (runLogHook)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

type WindowPosition = (W.RationalRect, Bool)

data PipStorage = PipStorage (Maybe (Window, WindowPosition, Bool))
    deriving (Typeable, Read, Show)
instance ExtensionClass PipStorage where
    initialValue = PipStorage Nothing
    extensionType = PersistentExtension

toggle :: Window -> X ()
toggle w = do
    PipStorage win <- XS.get
    case win of
        Just _ -> disable
        Nothing -> enable w

enable :: Window -> X ()
enable w = do
    pos <- currentWindowPosition w
    XS.modify $ \_ -> PipStorage (Just (w, pos, False))
    windows $ W.focusDown . W.float w pos1
    runLogHook

disable :: X ()
disable = do
    PipStorage win <- XS.get
    case win of
        Just (w, pos, _) -> setWindowPosition w pos
        Nothing          -> mempty
    XS.modify $ \_ -> PipStorage Nothing
    runLogHook

view :: WorkspaceId -> X ()
view i = do
    (PipStorage win) <- XS.get
    case win of
        Just (w, _, _) -> windows $ W.focusDown . W.view i . W.shiftWin i w
        Nothing        -> windows $ W.view i


greedyView :: WorkspaceId -> X ()
greedyView i = do
    (PipStorage win) <- XS.get
    case win of
        Just (w, _, _) -> windows $ W.focusDown . W.greedyView i . W.shiftWin i w
        Nothing        -> windows $ W.greedyView i

pipEventHook :: Event -> X All
pipEventHook e@(CrossingEvent {ev_event_type=t, ev_window=win})
    | t == enterNotify = do
        (PipStorage ps) <- XS.get
        case ps of
            Just (w, pos, p) -> if w == win then switchPosition win pos p  else mempty
            Nothing -> mempty
        return $ All True
    | otherwise = return $ All True

pipEventHook e@(ClientMessageEvent {ev_event_type=t, ev_window=win, ev_message_type=mt}) = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    when (mt == a_aw) $ do
        (PipStorage ps) <- XS.get
        case ps of
            Just (w, _, _) -> if w == win then windows W.focusDown else mempty
            Nothing -> mempty
    return $ All True
pipEventHook _ = return $ All True

isPip :: Window -> X Bool
isPip w = do
    (PipStorage win) <- XS.get
    case win of
        Just (x, _, _) -> return $ x == w
        Nothing        -> return False

switchPosition :: Window -> WindowPosition -> Bool -> X ()
switchPosition w pos p = do
    if p
    then windows $ W.float w pos1
    else windows $ W.float w pos2
    XS.modify $ \_ -> PipStorage (Just (w, pos, not p))
    return ()

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

setWindowPosition :: Window -> WindowPosition -> X ()
setWindowPosition w (loc, isFloating) = do
    windows $ if isFloating
        then W.focusWindow w . W.float w loc
        else W.focusWindow w . W.sink w

pos1 = W.RationalRect 0.60 0.55 0.35 0.35
pos2 = W.RationalRect 0.60 0.05 0.35 0.35

pipPP :: (String -> String) -> X (Maybe String)
pipPP f = do
    (PipStorage pip) <- XS.get
    case pip of
        Just _ -> return . Just . f $ "PiP"
        Nothing -> return Nothing
