module Wisp (
  WispConfig (..),
  activateWispConfig,
  gruvbox,
  nord,
  solarized,
  solarizedLight,
  dracula,
  grayscale,
  mocha,
  rgbaColor,
  argbColor
) where

import XMonad.Core
import System.Environment (setEnv)
import Control.Monad
import Text.Printf

rgbaColor :: (WispConfig -> String) -> WispConfig -> String
rgbaColor f cfg = f cfg ++ toHex (alpha cfg)
  where toHex x = (printf "%02X" (round (x*255) :: Integer)) :: String
        removePound = drop 1

argbColor :: (WispConfig -> String) -> WispConfig -> String
argbColor f cfg = "#" ++ toHex (alpha cfg) ++ removePound (f cfg)
  where toHex x = (printf "%02X" (round (x*255) :: Integer)) :: String
        removePound = drop 1

data WispConfig = WispConfig
  { configName :: String
  , alpha :: Rational
  , bgColor :: String
  , bgSecondary :: String
  , fgColor :: String
  , fgSecondary :: String
  , focusedColor :: String
  , unfocusedColor :: String
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
          , alpha                = 0.95
          , bgColor              = "#1d2021"
          , bgSecondary          = "#32302f"
          , fgColor              = "#ebdbb2"
          , fgSecondary          = "#928374"
          , unfocusedColor       = "#282828"
          -- , focusedColor         = "#504945"
          , focusedColor         = "#a6adc8"
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

mocha :: WispConfig
mocha = WispConfig
          { configName           = "catppuccin-mocha"
          , alpha                = 0.95
          , bgColor              = "#1e1e2e"
          , bgSecondary          = "#24273a"
          , fgColor              = "#cdd6f4"
          , fgSecondary          = "#303446"
          , unfocusedColor       = "#24273a"
          -- , focusedColor         = "#51576d"
          , focusedColor         = "#a6adc8"
          -- , focusedColor         = "#cba6f7"
          , color0               = "#45475a"
          , color1               = "#f38ba8"
          , color2               = "#a6e3a1"
          , color3               = "#f9e2af"
          , color4               = "#89b4fa"
          , color5               = "#f5c2e7"
          , color6               = "#94e2d5"
          , color7               = "#a6adc8"
          , color8               = "#585b70"
          , color9               = "#f38ba8"
          , color10              = "#a6e3a1"
          , color11              = "#f9e2af"
          , color12              = "#89b4fa"
          , color13              = "#f5c2e7"
          , color14              = "#94e2d5"
          , color15              = "#a6adc8"
          , normalFont           = "xft:hack:size=11"
          }

nord :: WispConfig
nord = WispConfig
       { configName           = "nord"
       , alpha                = 0.97
       , bgColor              = "#292e39"
       , bgSecondary          = "#4c566a"
       , fgColor              = "#d8dee9"
       , fgSecondary          = "#4c566a"
       , unfocusedColor       = "#3b4252"
       , focusedColor         = "#4c566a"
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

solarized :: WispConfig
solarized = WispConfig
       { configName           = "solarized"
       , alpha                = 0.97
       , bgColor              = "#002b36"
       , bgSecondary          = "#073642"
       , fgColor              = "#657b83"
       , fgSecondary          = "#073642"
       , unfocusedColor       = "#073642"
       , focusedColor         = "#586e75"
       , color0               = "#073642"
       , color1               = "#dc322f"
       , color2               = "#859900"
       , color3               = "#b58900"
       , color4               = "#268bd2"
       , color5               = "#d33682"
       , color6               = "#2aa198"
       , color7               = "#eee8d5"
       , color8               = "#002b36"
       , color9               = "#cb4b16"
       , color10              = "#586e75"
       , color11              = "#657b83"
       , color12              = "#839496"
       , color13              = "#6c71c4"
       , color14              = "#93a1a1"
       , color15              = "#fdf6e3"
       , normalFont           = "xft:hack:size=11"
       }

solarizedLight :: WispConfig
solarizedLight = WispConfig
       { configName           = "solarized"
       , alpha                = 0.97
       , bgColor              = "#fdf6e3"
       , bgSecondary          = "#eee8d5"
       , fgColor              = "#839496"
       , fgSecondary          = "#bbbbbb"
       , focusedColor         = "#839496"
       , unfocusedColor       = "#eee8d5"
       , color0               = "#eee8d5"
       , color1               = "#dc322f"
       , color2               = "#859900"
       , color3               = "#b58900"
       , color4               = "#268bd2"
       , color5               = "#d33682"
       , color6               = "#2aa198"
       , color7               = "#073642"
       , color8               = "#fdf6e3"
       , color9               = "#cb4b16"
       , color10              = "#93a1a1"
       , color11              = "#839496"
       , color12              = "#657b83"
       , color13              = "#6c71c4"
       , color14              = "#586e75"
       , color15              = "#002b36"
       , normalFont           = "xft:hack:size=11"
       }

dracula :: WispConfig
dracula = WispConfig
       { configName           = "dracula"
       , alpha                = 0.97
       , bgColor              = "#282a36"
       , bgSecondary          = "#44475a"
       , fgColor              = "#f8f8f2"
       , fgSecondary          = "#44475a"
       , focusedColor         = "#44475a"
       , unfocusedColor       = "#4d4d4d"
       , color0               = "#000000"
       , color1               = "#ff5555"
       , color2               = "#50fa7b"
       , color3               = "#f1fa8c"
       , color4               = "#bd93f9"
       , color5               = "#ff79c6"
       , color6               = "#8be9fd"
       , color7               = "#bfbfbf"
       , color8               = "#4d4d4d"
       , color9               = "#ff6e67"
       , color10              = "#5af78e"
       , color11              = "#f4f99d"
       , color12              = "#caa9fa"
       , color13              = "#ff92d0"
       , color14              = "#9aedfe"
       , color15              = "#e6e6e6"
       , normalFont           = "xft:hack:size=11"
       }

grayscale :: WispConfig
grayscale = WispConfig
       { configName           = "zellner"
       , alpha                = 0.97
       , bgColor              = "#f7f7f7"
       , bgSecondary          = "#d1d1d1"
       , fgColor              = "#464646"
       , fgSecondary          = "#d1d1d1"
       , focusedColor         = "#888888"
       , unfocusedColor       = "#d1d1d1"
       , color0 = "#f7f7f7"
       , color1 = "#7c7c7c"
       , color2 = "#8e8e8e"
       , color3 = "#a0a0a0"
       , color4 = "#686868"
       , color5 = "#747474"
       , color6 = "#868686"
       , color7 = "#464646"
       , color8 = "#ababab"
       , color9 = "#999999"
       , color10 = "#e3e3e3"
       , color11 = "#b9b9b9"
       , color12 = "#525252"
       , color13 = "#252525"
       , color14 = "#5e5e5e"
       , color15 = "#101010"
       , normalFont           = "xft:hack:size=11"
       }

activateWispConfig :: WispConfig -> X ()
activateWispConfig cfg = do
  io $ setEnv "WISP_THEME" (configName cfg)
  io $ setEnv "WISP_BACKGROUND_RGBA" (rgbaColor bgColor cfg)
  io $ setEnv "WISP_BACKGROUND_ARGB" (argbColor bgColor cfg)
  io $ setEnv "WISP_BACKGROUND" (bgColor cfg)
  io $ setEnv "WISP_BACKGROUND_SECONDARY" (bgSecondary cfg)
  io $ setEnv "WISP_FOREGROUND" (fgColor cfg)
  io $ setEnv "WISP_FOREGROUND_SECONDARY" (fgSecondary cfg)
  io $ setEnv "WISP_FOCUSED" (focusedColor cfg)
  io $ setEnv "WISP_UNFOCUSED" (unfocusedColor cfg)
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

