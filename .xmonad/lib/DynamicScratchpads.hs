module DynamicScratchpads (
        makeDynamicSP,
        spawnDynamicSP
    ) where

import Graphics.X11.Types
import qualified Data.Map as M

import XMonad.Core
import XMonad.Operations
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

data SPStorage = SPStorage (M.Map String Window)
    deriving (Typeable,Read,Show)
instance ExtensionClass SPStorage where
    initialValue = SPStorage $ M.fromList []
    extensionType = PersistentExtension

makeDynamicSP :: String -> Window -> X ()
makeDynamicSP s w = do
    (SPStorage m) <- XS.get
    case M.lookup s m of
        Nothing -> addDynamicSP s w
        Just ow  -> if w == ow
                    then removeDynamicSP s
                    else (showWindow ow >> addDynamicSP s w)

addDynamicSP :: String -> Window -> X ()
addDynamicSP s w = XS.modify $ alterSPStorage (\_ -> Just w) s

removeDynamicSP :: String -> X ()
removeDynamicSP s = XS.modify $ alterSPStorage (\_ -> Nothing) s

spawnDynamicSP :: String -> X ()
spawnDynamicSP s = do
    (SPStorage m) <- XS.get
    case M.lookup s m of
        Nothing -> mempty
        Just w  -> spawnDynamicSP' w

spawnDynamicSP' :: Window -> X ()
spawnDynamicSP' w = withWindowSet $ \s -> do
    let matchingWindows = filter (== w) ((maybe [] W.integrate . W.stack . W.workspace . W.current) s)
    case matchingWindows of
        [] -> showWindow w
        _  -> hideWindow w

hideWindow :: Window -> X ()
hideWindow = windows . W.shiftWin "NSP"

showWindow :: Window -> X ()
showWindow w = windows $ \ws ->
    (W.focusWindow w) . (W.shiftWin (W.currentTag ws) w) $ ws


alterSPStorage :: (Maybe Window -> Maybe Window) -> String -> SPStorage -> SPStorage
alterSPStorage f k (SPStorage m) = SPStorage $ M.alter f k m
