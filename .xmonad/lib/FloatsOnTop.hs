module FloatsOnTop (
    floatsOnTop
) where

import XMonad (ManageHook, MonadReader(ask))
import qualified XMonad.StackSet as W
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Monoid (Endo(Endo))
import Data.Map (keys)

floatsOnTop :: ManageHook
floatsOnTop = Endo . g <$> ask
    where
        g w ws = case isFloating w ws of
            True -> viewingWs w (W.focusWindow w . ins w . W.delete' w) ws
            False -> ws
        ins w = (\f ws -> fromMaybe id (W.focusWindow <$> W.peek ws) $ f ws) $
            W.insertUp w
        isFloating w ws = elem w $ keys (W.floating ws)

viewingWs :: (Eq a, Eq s, Eq i, Show i) =>a-> (W.StackSet i l a s sd -> W.StackSet i l a s sd)-> W.StackSet i l a s sd-> W.StackSet i l a s sd
viewingWs w f = do
    i <- W.tag . W.workspace . W.current
    ws <- find (elem w . W.integrate' . W.stack) . W.workspaces
    maybe id (fmap (W.view i . f) . W.view . W.tag) ws
