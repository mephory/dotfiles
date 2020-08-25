module WorkspaceHelpers (
        WS (..)
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

data WS = WS { wsName :: String
             , wsKey :: Maybe KeySym
             , wsVisible :: Bool
             }
