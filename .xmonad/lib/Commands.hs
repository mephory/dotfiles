{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Commands (
        commandHandler
    ) where

import Control.Monad
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import XMonad
import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace, removeEmptyWorkspaceAfter, renameWorkspaceByName)
import XMonad.Actions.Navigation2D (Direction2D (..), screenSwap, windowGo, windowSwap)
import XMonad.Layout.Maximize (maximizeRestore)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (namedScratchpadAction)
import XMonad.Layout.LayoutCombinators (JumpToLayout (..))
import Network.Simple.TCP
import qualified Data.ByteString.Char8 as B

commandHandler (port:xs) = do
        connect "127.0.0.1" port \(sock, remoteAddr) -> do
                commandHandler' (\x -> send sock x) xs >> return ()
commandHandler _ = return ()

commandHandler' :: (B.ByteString -> IO ()) -> [String] -> X ()
-- Core
commandHandler' respond ["exit-xmonad"] = io exitSuccess
-- Directional actions
commandHandler' respond ["focus-left"] = windowGo L False
commandHandler' respond ["focus-down"] = windowGo D False
commandHandler' respond ["focus-up"] = windowGo U False
commandHandler' respond ["focus-right"] = windowGo R False
commandHandler' respond ["swap-left"] = windowSwap L False
commandHandler' respond ["swap-down"] = windowSwap D False
commandHandler' respond ["swap-up"] = windowSwap U False
commandHandler' respond ["swap-right"] = windowSwap R False
-- Stack actions
commandHandler' respond ["focus-master"] = windows W.focusMaster
commandHandler' respond ["swap-master"] = windows W.swapMaster
commandHandler' respond ["focus-prev"] = windows W.focusUp
commandHandler' respond ["focus-next"] = windows W.focusDown
commandHandler' respond ["swap-prev"] = windows W.swapUp
commandHandler' respond ["swap-next"] = windows W.swapDown

commandHandler' respond ["close-window"] = kill1
commandHandler' respond ["close-program"] = kill

commandHandler' respond ["test"] = io $ respond "Was geht?"

-- Layouts
commandHandler' respond ["cycle-layouts"] = sendMessage NextLayout
commandHandler' respond ["set-layout", new_layout] = sendMessage $ JumpToLayout new_layout
commandHandler' respond ["shrink-master-area"] = sendMessage Shrink
commandHandler' respond ["expand-master-area"] = sendMessage Expand
commandHandler' respond ["increment-masters"] = sendMessage $ IncMasterN 1
commandHandler' respond ["decrement-masters"] = sendMessage $ IncMasterN $ -1

commandHandler' respond ["rename-workspace", new_name] = renameWorkspaceByName new_name
commandHandler' respond ["go-to-workspace", workspace_name] = windows $ W.view workspace_name
commandHandler' respond ["send-to-workspace", workspace_name] = windows $ W.shift workspace_name
commandHandler' respond ["send-copy-to-workspace", workspace_name] = windows $ copy workspace_name
commandHandler' respond ["create-workspace", name] = addHiddenWorkspace name

commandHandler' respond _ = spawn "notify-send XMonad 'bad XMonad command'"
