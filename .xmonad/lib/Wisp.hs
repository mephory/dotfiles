module Wisp (
  WispConfig (..),
  activateWispConfig,
  gruvbox,
  nord
) where

import XMonad.Core
import System.Environment (setEnv)
import Control.Monad

data WispConfig = WispConfig
  { configName :: String
  , backgroundColor :: String
  , backgroundColorAlpha :: String
  , foregroundColor :: String
  , color0 :: String
  , color1 :: String
  , color2 :: String
  , color3 :: String
  , color4 :: String
  , color5 :: String
  , color6 :: String
  , color7 :: String
  , color8 :: String
  , color9 :: String
  , color10 :: String
  , color11 :: String
  , color12 :: String
  , color13 :: String
  , color14 :: String
  , color15 :: String
  , normalFont :: String
  }
  deriving (Read, Show)

gruvbox :: WispConfig
gruvbox = WispConfig
          { configName           = "gruvbox"
          , backgroundColor      = "#1d2021"
          , backgroundColorAlpha = "#1d202188"
          , foregroundColor      = "#ebdbb2"
          , color0               = "#282828"
          , color1               = "#cc241d"
          , color2               = "#98971a"
          , color3               = "#d79921"
          , color4               = "#458588"
          , color5               = "#b16286"
          , color6               = "#689d6a"
          , color7               = "#a89984"
          , color8               = "#928374"
          , color9               = "#fb4934"
          , color10              = "#b8bb26"
          , color11              = "#fabd2f"
          , color12              = "#83a598"
          , color13              = "#d3869b"
          , color14              = "#8ec07c"
          , color15              = "#ebdbb2"
          , normalFont           = "xft:hack:size=11"
          }

nord :: WispConfig
nord = WispConfig
       { configName           = "nord"
       , backgroundColor      = "#292e39"
       , backgroundColorAlpha = "#292e3988"
       , foregroundColor      = "#d8dee9"
       , color0               = "#3b4252"
       , color1               = "#bf616a"
       , color2               = "#a3be8c"
       , color3               = "#ebcb8b"
       , color4               = "#81a1c1"
       , color5               = "#b48ead"
       , color6               = "#88c0d0"
       , color7               = "#e5e9f0"
       , color8               = "#4c566a"
       , color9               = "#bf616a"
       , color10              = "#a3be8c"
       , color11              = "#ebcb8b"
       , color12              = "#81a1c1"
       , color13              = "#b48ead"
       , color14              = "#8fbcbb"
       , color15              = "#eceff4"
       , normalFont           = "xft:hack:size=11"
       }

activateWispConfig :: WispConfig -> X ()
activateWispConfig cfg = do
  io $ setEnv "WISP_BACKGROUND_ALPHA" (backgroundColorAlpha cfg)
  io $ setEnv "WISP_BACKGROUND" (backgroundColor cfg)
  io $ setEnv "WISP_FOREGROUND" (foregroundColor cfg)
  io $ setEnv "WISP_FONT"       (normalFont cfg)
  io $ setEnv "WISP_COLOR0"     (color0 cfg)
  io $ setEnv "WISP_COLOR1"     (color1 cfg)
  io $ setEnv "WISP_COLOR2"     (color2 cfg)
  io $ setEnv "WISP_COLOR3"     (color3 cfg)
  io $ setEnv "WISP_COLOR4"     (color4 cfg)
  io $ setEnv "WISP_COLOR5"     (color5 cfg)
  io $ setEnv "WISP_COLOR6"     (color6 cfg)
  io $ setEnv "WISP_COLOR7"     (color7 cfg)
  io $ setEnv "WISP_COLOR8"     (color8 cfg)
  io $ setEnv "WISP_COLOR9"     (color9 cfg)
  io $ setEnv "WISP_COLOR10"    (color10 cfg)
  io $ setEnv "WISP_COLOR11"    (color11 cfg)
  io $ setEnv "WISP_COLOR12"    (color12 cfg)
  io $ setEnv "WISP_COLOR13"    (color13 cfg)
  io $ setEnv "WISP_COLOR14"    (color14 cfg)
  io $ setEnv "WISP_COLOR15"    (color15 cfg)
  return ()

