module EvasiveWindow (
    toggleEvasive,
    setEvasive,
    clearEvasive,
    evasiveWindowEventHook
) where

import Data.Monoid
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras (Event(CrossingEvent, ev_event_type, ev_window))
import XMonad.Core
import XMonad.Operations
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

data EvasiveStorage = EvasiveStorage (Maybe Window)
    deriving (Typeable, Read, Show)
instance ExtensionClass EvasiveStorage where
    initialValue = EvasiveStorage Nothing
    extensionType = PersistentExtension

toggleEvasive :: Window -> X ()
toggleEvasive w = do
    (EvasiveStorage win) <- XS.get
    case win of
        Just _  -> clearEvasive
        Nothing -> setEvasive w

setEvasive :: Window -> X ()
setEvasive w = XS.modify $ \_ -> EvasiveStorage (Just w)

clearEvasive :: X ()
clearEvasive = XS.modify $ \_ -> EvasiveStorage Nothing

evasiveWindowEventHook :: Event -> X All
evasiveWindowEventHook e@(CrossingEvent {ev_event_type=t, ev_window=win})
    | t == enterNotify = do
        b <- isEvasive win
        case b of
            True -> switchPosition win
            False -> mempty
        return $ All True
    | otherwise = return $ All True

isEvasive :: Window -> X Bool
isEvasive w = do
    (EvasiveStorage win) <- XS.get
    case win of
        Just x -> return $ x == w
        Nothing -> return False

switchPosition :: Window -> X ()
switchPosition w = do
    (_, pos) <- floatLocation w
    if pos == pos1
    then windows $ W.float w pos2
    else windows $ W.float w pos1
    return ()

pos1 = W.RationalRect 0.55 0.05 0.4 0.4
pos2 = W.RationalRect 0.55 0.55 0.4 0.4
