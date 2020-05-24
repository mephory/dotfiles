module LowerDocks (
        addDock,
        delDock
    ) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.WindowProperties (getProp32s)

import Data.List
import Data.Monoid

data DockT  = DockA Window
            | DesktopA Window
  deriving (Show, Eq)

checkDesktop :: Query Bool
checkDesktop = ask >>= \w -> liftX $ do
    desk <- getAtom "_NET_WM_WINDOW_TYPE_DESKTOP"
    mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
    case mbr of
        Just rs -> return $ any (== desk) (map fromIntegral rs)
        _       -> return False

newtype DockList    = DockList [DockT]
  deriving (Show)

findDock :: Window -> [DockT] -> Maybe DockT
findDock w    = find go
  where
    go :: DockT -> Bool
    go (DockA x)    = w == x
    go (DesktopA x) = w == x

getPanels :: DockList -> [DockT]
getPanels (DockList ds) = filter go ds
  where
    go (DockA _)    = True
    go _            = False

getDesktops :: DockList -> [DockT]
getDesktops (DockList ds) = filter go ds
  where
    go (DesktopA _) = True
    go _            = False

unDock :: DockT -> Window
unDock (DockA x)    = x
unDock (DesktopA x) = x

instance ExtensionClass DockList where
    initialValue = DockList []


addDesktop :: MaybeManageHook
addDesktop    = checkDesktop  -?> do
    w <- ask
    liftX $ XS.modify (\(DockList zs) -> DockList (DesktopA w : zs))
    return idHook

addPanel :: MaybeManageHook
addPanel    = checkDock  -?> do
    w <- ask
    liftX $ XS.modify (\(DockList zs) -> DockList (DockA w : zs))
    return idHook

addDock :: ManageHook
addDock   = checkDock --> do
    composeOne [addDesktop, addPanel]
    liftX $ do
      dl <- XS.get
      let ds = getDesktops dl
          ps = getPanels dl
          rs = map unDock (ps ++ ds)
      d <- asks display
      liftIO $ do
        lowerWindow d (head rs)
        restackWindows d rs
      return mempty

delDock :: Event -> X All
delDock (DestroyWindowEvent {ev_window = w}) = do
    DockList ds <- XS.get
    let md = w `findDock` ds
    case md of
      Just d    -> XS.put (DockList (d `delete` ds)) >> return mempty
      Nothing   -> return mempty
delDock _ = return mempty
