{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

import XMonad
import XMonad.Actions.NoBorders
import XMonad.Actions.FloatSnap
import XMonad.Actions.DynamicProjects
import XMonad.Actions.OnScreen
import XMonad.Actions.WindowBringer
import XMonad.Layout.PerWorkspace
import XMonad.Layout.BoringWindows
import XMonad.Layout.Simplest
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BinaryColumn
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FloatNext
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn)
import XMonad.Prompt
import Data.List (elemIndex, isPrefixOf, isInfixOf, find)
import Data.Maybe (fromMaybe)
import System.Exit
import System.Environment (getEnv)
import System.FilePath (joinPath)
import Graphics.X11.ExtraTypes.XF86

import WorkspaceHelpers
import SubmapWithHints (submapWithHints)
import UnicodeUtils (appendFileUtf8)
import ZoomWindow (toggleZoom)
import DynamicScratchpads (spawnDynamicSP, makeDynamicSP)
import FloatCenterWindow (centerFloatingWindow, makeFloatingCenterWindow)
import Opacity (changeOpacity)
import LowerDocks (addDock, delDock)
import qualified Wisp as WSP
-- import Wal

import qualified XMonad.StackSet as W
import qualified Data.Map as M

myWispConfig = WSP.mocha

modKey = mod1Mask
altModm = mod1Mask .|. mod4Mask

myWorkspaces :: [WS]
myWorkspaces = [ WS "1"       modKey    (Just xK_1)           True
               , WS "2"       modKey    (Just xK_2)           True
               , WS "3"       modKey    (Just xK_3)           True
               , WS "4"       modKey    (Just xK_4)           True
               , WS "5"       modKey    (Just xK_5)           True
               , WS "6"       modKey    (Just xK_6)           True
               , WS "7"       modKey    (Just xK_7)           True
               , WS "8"       modKey    (Just xK_8)           True
               , WS "9"       modKey    (Just xK_9)           True
               , WS "TAB"     modKey    (Just xK_Tab)         True
               , WS "sys"     modKey    (Just xK_F1)          False
               , WS "F2"      modKey    (Just xK_F2)          True
               , WS "F3"      modKey    (Just xK_F3)          True
               , WS "'1"      altModm   (Just xK_1)           False
               , WS "'2"      altModm   (Just xK_2)           False
               , WS "'3"      altModm   (Just xK_3)           False
               , WS "'4"      altModm   (Just xK_4)           False
               , WS "'5"      altModm   (Just xK_5)           False
               , WS "NSP"     modKey    Nothing               False
               ]

wsNames :: [String]
wsNames = map wsName myWorkspaces

findWs :: String -> Maybe WS
findWs s = find (\w -> wsName w == s) myWorkspaces

workspaceVisible :: String -> Bool
workspaceVisible s = fromMaybe False $ findWs s >>= Just . wsVisible

main = do
    homeDir <- getEnv "HOME"
    safeSpawn "mkfifo" ["/tmp/workspace-info"]
    xmonad $ docks $ ewmh $ dynamicProjects projects $ withUrgencyHook NoUrgencyHook def {
      -- simple stuff
        terminal           = "alacritty"
      , focusFollowsMouse  = True
      , borderWidth        = 1
      , modMask            = modKey
      , workspaces         = wsNames
      , normalBorderColor  = WSP.unfocusedColor myWispConfig
      , focusedBorderColor = WSP.focusedColor myWispConfig

      -- key bindings
      , keys               = myKeys
      , mouseBindings      = myMouseBindings

      -- hooks, layouts
      , layoutHook         = myLayout
      , manageHook         = addDock
                             <+> myManageHook
                             <+> namedScratchpadManageHook myScratchpads
                             <+> floatPlacement
                             <+> floatNextHook
      , handleEventHook    = delDock <+> myEventHook
      , logHook            = myLogHook homeDir
      , startupHook        = myStartupHook homeDir
      , clientMask         = focusChangeMask .|. visibilityChangeMask .|. clientMask def
    }

projects =
    [ Project { projectName      = "TAB"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do
                    spawn "spotify"
              }
    , Project { projectName      = "'1"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do
                    spawn "discord"
                    spawn "alacritty -e ssh -t mephory LANG=en_US.utf8 TERM=xterm-256color tmux attach -t irc"
              }
    , Project { projectName      = "7"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do
                    spawn "steam"
              }
    ]

-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_x     ), spawn "xkill")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_j     ), focusDown)
    , ((modm,               xK_k     ), focusUp)
    , ((modm,               xK_m     ), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_j     ), swapDown)
    , ((modm .|. shiftMask, xK_k     ), swapUp)
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_minus ), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_minus ), sendMessage (IncMasterN (1)))
    , ((modm .|. shiftMask, xK_f     ), toggleFloatNext >> runLogHook)
    , ((modm              , xK_Escape), spawn "slock")
    , ((modm              , xK_o     ), withFocused toggleZoom)
    , ((modm              , xK_slash ),  submapWithHints xpc $
        [ ("q   recompile xmonad"       , (0, xK_q), spawn "xmonad --recompile && xmonad --restart")
        , ("k   default keyboard layout", (0, xK_k), spawn "setxkbmap us -option compose:ralt")
        , ("m-Q   exit xmonad"          , (modm .|. shiftMask, xK_q     ), io exitSuccess)
        ])

    , ((modm,               xK_p             ), spawn "rofi -modi drun,run -show drun -show-icons -display-drun '\xf0e7' -theme ~/.config/rofi/launcher.rasi")
    , ((modm              , xK_n             ), spawn "rofi-pass")
    , ((modm .|. shiftMask, xK_n             ), spawn "rofi-pass --insert")
    , ((modm              , xK_BackSpace     ), spawn "rofi-pass")
    , ((modm .|. shiftMask, xK_BackSpace     ), spawn "rofi-pass --insert")
    , ((modm .|. shiftMask, xK_l             ), spawn "rofi-sd")
    , ((modm              , xK_period        ), switchProjectPrompt xpc)
    , ((modm .|. shiftMask, xK_period        ), shiftToProjectPrompt xpc)

    -- Dunst
    , ((modm              , xK_bracketleft ), spawn "dunstctl history-pop")
    , ((modm .|. shiftMask, xK_bracketleft ), spawn "dunstctl context")
    , ((modm              , xK_bracketright), spawn "dunstctl close")
    , ((modm .|. shiftMask, xK_bracketright), spawn "dunstctl close-all")

    -- Various
    , ((modm .|. shiftMask, xK_slash    ), spawn "python /home/mephory/code/fbg/fbg.py toggle")
    , ((modm              , xK_BackSpace), mempty)
    , ((modm              , xK_semicolon), gotoMenuConfig $ def { menuCommand = "rofi", menuArgs = ["-dmenu", "-i"] })
    , ((modm .|. shiftMask, xK_semicolon), spawn "xdotool getactivewindow set_window --classname class-removed")
    , ((modm              , xK_i        ), spawn "polybar-msg cmd toggle")

    -- Change workspaces on vertical monitor
    , ((altModm              , xK_r ), windows $ onScreen (W.greedyView "'1") FocusCurrent 1)
    , ((altModm .|. shiftMask, xK_r ), windows $ W.shift "'1")
    , ((altModm              , xK_d ), windows $ onScreen (W.greedyView "'2") FocusCurrent 1)
    , ((altModm .|. shiftMask, xK_d ), windows $ W.shift "'2")

    -- Floating Positions
    , ((modm              , xK_f     ) , withFocused makeFloatingCenterWindow)
    , ((modm              , xK_Left  ) , withFocused $ snapMove L Nothing)
    , ((modm              , xK_Right ) , withFocused $ snapMove R Nothing)
    , ((modm              , xK_Up    ) , withFocused $ snapMove U Nothing)
    , ((modm              , xK_Down  ) , withFocused $ snapMove D Nothing)
    , ((modm .|. shiftMask, xK_Left  ) , withFocused $ snapShrink R Nothing)
    , ((modm .|. shiftMask, xK_Right ) , withFocused $ snapGrow R Nothing)
    , ((modm .|. shiftMask, xK_Up    ) , withFocused $ snapShrink D Nothing)
    , ((modm .|. shiftMask, xK_Down  ) , withFocused $ snapGrow D Nothing)
    , ((modm              , xK_g     ) , withFocused centerFloatingWindow)

    -- Screenshots
    , ((modm              , xK_comma ), spawn "imgs")
    , ((modm .|. shiftMask, xK_comma ), spawn "imgs -s")
    , ((0                 , xK_Print ), spawn "imgs -d screenshots -n -t activewindow")
    , ((shiftMask         , xK_Print ), spawn "imgs -d screenshots -n -t select")
    , ((modm              , xK_Print ), spawn "imgs -d screenshots -n -t all")
    , ((modm              , xK_0     ), spawn "upload-screenshot -u")
    , ((modm .|. shiftMask, xK_0     ), spawn "upload-screenshot -s")
    , ((modm .|. shiftMask, xK_v     ), spawn "screenshot-google-image-search")

    -- Scratchpads
    , ((modm              , xK_v        ), namedScratchpadAction myScratchpads "terminal"      )
    , ((modm              , xK_c        ), namedScratchpadAction myScratchpads "terminal-2"    )
    , ((modm              , xK_x        ), namedScratchpadAction myScratchpads "terminal-3"    )
    , ((modm              , xK_z        ), namedScratchpadAction myScratchpads "music"         )
    -- , ((modm              , xK_b        ), namedScratchpadAction myScratchpads "terminal-large")
    , ((modm .|. shiftMask, xK_p        ), namedScratchpadAction myScratchpads "color"         )
    , ((modm              , xK_backslash), namedScratchpadAction myScratchpads "bot-term"      )
    , ((modm              , xK_a        ), spawnDynamicSP "dyn1"                               )
    , ((modm .|. shiftMask, xK_a        ), withFocused $ makeDynamicSP "dyn1"                  )
    , ((modm              , xK_s        ), spawnDynamicSP "dyn2"                               )
    , ((modm .|. shiftMask, xK_s        ), withFocused $ makeDynamicSP "dyn2"                  )
    , ((modm              , xK_d        ), spawnDynamicSP "dyn3"                               )
    , ((modm .|. shiftMask, xK_d        ), withFocused $ makeDynamicSP "dyn3"                  )
    , ((altModm           , xK_f        ), namedScratchpadAction myScratchpads "obsidian"      )
    , ((modm              , xK_b        ), namedScratchpadAction myScratchpads "obsidian"      )

    -- Media Keys
    , ((0, xF86XK_AudioMute), spawn "amixer sset Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "pulsemixer --change-volume -1")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pulsemixer --change-volume +1")
    , ((0, xF86XK_AudioPlay), spawn "polybar-playerctl toggle")
    , ((0, xF86XK_AudioPrev), spawn "polybar-playerctl previous")
    , ((0, xF86XK_AudioNext), spawn "polybar-playerctl next")
    , ((shiftMask, xF86XK_AudioPlay), spawn "polybar-playerctl switch")
    , ((0, xF86XK_Tools), spawn "polybar-playerctl switch")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    , ((0, xF86XK_Display), spawn "xbacklight -set 85")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- [((m .|. modm, k), f i)
    [((m .|. wsm, k), f i)
        | (i, (Just k), wsm) <- map (\ws -> (wsName ws, wsKey ws, wsMod ws)) myWorkspaces
        , (f, m) <- [(windows . W.greedyView, 0), (windows . W.shift, shiftMask)]]

    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (f))
        | (key, sc) <- zip [xK_w, xK_e, xK_q] [0..]
        , (f, m) <- [(windows . W.view, 0), (windows . W.shift, shiftMask)]]




runUnlessIgnored action w = runQuery mouseIgnore w >>= \b -> if b then mempty else action w
    where mouseIgnore = className =? "dota2"

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, button1), runUnlessIgnored (\w -> focus w >> mouseMoveWindow w))
    , ((modm, button2), runUnlessIgnored (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), runUnlessIgnored (\w -> focus w >> mouseResizeWindow w))
    , ((modm , button4) , \w -> changeOpacity w (5/100))
    , ((modm , button5) , \w -> changeOpacity w (-5/100))
    , ((0    , 10 :: Button), \_ -> spawn "polybar-playerctl toggle")
    , ((0    , 13 :: Button), \_ -> spawn "polybar-playerctl switch")
    ]


myLayout = smartBorders $ boringWindows $
    onWorkspace "1"  (full ||| fullscreen ||| tiled ||| mtiled) $
    onWorkspace "5"  (noBorders $ fullscreen) $
    onWorkspace "7"  (avoidStruts $ spaced $ Tall 1 (3/100) (1/4)) $
    onWorkspace "'1" (avoidStruts $ verticalLayout) $
    onWorkspace "'2" (avoidStruts $ verticalLayout) $
    onWorkspace "'3" (avoidStruts $ verticalLayout) $
    onWorkspace "'4" (avoidStruts $ verticalLayout) $
    onWorkspace "'5" (avoidStruts $ verticalLayout) $
    defaultConf
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled      = spaced $ avoidStruts $ Tall nmaster delta ratio
        mtiled     = spaced $ avoidStruts $ Mirror (Tall 1 (3/100) (1/2))
        full       = avoidStruts $ spaced $ Full
        fullscreen = Full
        defaultConf = tiled ||| mtiled ||| full
        verticalLayout = spaced (BinaryColumn 0.0 32 ||| BinaryColumn 1.0 32 ||| BinaryColumn 2.0 32)

        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 0.6
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- spaced = spacingRaw True (Border 1 1 1 1) False (Border 0 0 0 0) True
        spaced = spacingRaw False (Border 10 10 10 10) False (Border 10 10 10 10) True

myManageHook = composeAll
    [ resource  =? "desktop_window"      --> doIgnore
    , resource  =? "kdesktop"            --> doIgnore
    , className =? "qutebrowser"         --> doShift "1"
    , className =? "explorer.exe"        --> doShift "9"
    , title     =? "Wine System Tray"    --> doShift "9"
    , className =? "dota2"               --> doShift "5" <+> (doF . W.sink =<< ask)
    , resource  =? "polybar-pavucontrol" --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "vselect"             --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "vselect-record-area" --> placeHook (fixed (0, 0)) <+> doFloat
    , title     =? "pinentry"            --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , className =? "Pinentry-gtk-2"      --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    -- , title     =? "popup-editor"        --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    -- , className =? "popup-editor"        --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "popup-editor"        --> (customFloating $ centeredRect 0.5 0.5)
    , className =? "popup-editor"        --> (customFloating $ centeredRect 0.5 0.5)
    , className =? "feh-float"           --> doF W.shiftMaster <+> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "Microsoft Teams Notification" --> placeHook (fixed (1, 1)) <+> doFloat
    , title     =? "Picture-in-Picture"  --> (customFloating $ W.RationalRect 0.65 0.65 0.3 0.3)
    , className =? "Steam"               --> doShift "7"
    , className =? "discord"             --> doShift "'1"
    ]
    where
      centeredRect w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

floatPlacement = placeHook (withGaps (10, 10, 10, 10) $ smart (0.5, 0.2))

myEventHook = mempty

myStartupHook homeDir = do
    WSP.activateWispConfig myWispConfig
    safeSpawn (joinPath [homeDir, ".config", "alacritty", "build_config.sh"]) []
    safeSpawn "restart-polybar" []
    safeSpawn "restart-dunst" []
    safeSpawn "restart-picom" []

myScratchpads = [ NS "terminal"
                     "alacritty --class scratchpad --title 'Alacritty (v)'"
                     (resource =? "scratchpad")
                     centered
                , NS "terminal-2"
                     "alacritty --class scratchpad-2 --title 'Alacritty (c)'"
                     (resource =? "scratchpad-2")
                     centered
                , NS "terminal-3"
                     "alacritty --class scratchpad-3 --title 'Alacritty (x)'"
                     (resource =? "scratchpad-3")
                     centered
                , NS "bot-term"
                     "alacritty --class bot-term"
                     (resource =? "bot-term")
                     bottom
                , NS "terminal-large"
                     "alacritty --class scratchpad-large --title 'mutt' -e neomutt"
                     (resource =? "scratchpad-large")
                     centeredBig
                , NS "music"
                     "alacritty --class music -e pulsemixer"
                     (resource =? "music")
                     centered
                , NS "irc"
                     "alacritty --class irc -e ssh -t mephory LANG=en_US.utf8 TERM=xterm-256color tmux attach -t irc"
                     (resource =? "irc")
                     centeredBig
                , NS "color"
                     "gcolor3"
                     (resource =? "gcolor3")
                     centeredOriginalSize
                , NS "obsidian"
                     "obsidian"
                     (resource =? "obsidian")
                     obsidianGeometry
                ]
    where
        centered = customFloating $ centeredRect 0.5 0.5
        bottom = customFloating $ W.RationalRect 0 (1-0.4) 1 0.4
        centeredBig = (customFloating $ centeredRect 0.6 0.6)
        centeredOriginalSize = placeHook (fixed (0.5, 0.5)) <+> doFloat
        centeredRect w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h
        obsidianGeometry = (customFloating $ centeredRect 0.6 0.85)

myLogHook homeDir = dynamicLogWithPP $ def {
      ppCurrent         = clickable currentFmt
    , ppVisible         = clickable visibleFmt
    , ppHidden          = onlyIf workspaceVisible (clickable hiddenFmt)
    , ppHiddenNoWindows = \_ -> ""
    , ppUrgent          = clickable urgentFmt
    , ppSep             = "  "
    , ppWsSep           = ""
    , ppLayout          = \_ -> ""
    , ppTitle           = \_ -> ""
    , ppExtras          = [willFloatNextPP floatNextStr]
    , ppOrder           = \(w:l:t:es) -> [w, l] ++ es ++ [t]
    , ppOutput          = appendFileUtf8 "/tmp/workspace-info" . (++"\n")
    }
    where
        hiddenFmt  = bg (WSP.argbColor WSP.bgColor myWispConfig) .
                     fg (WSP.fgSecondary myWispConfig) .
                     addPadding
        currentFmt = bg (WSP.argbColor WSP.bgSecondary myWispConfig) .
                     fg (WSP.fgColor myWispConfig) .
                     addPadding
        urgentFmt  = bg (WSP.argbColor WSP.color1 myWispConfig) .
                     fg (WSP.fgColor myWispConfig) .
                     addPadding
        visibleFmt = addPadding
        addPadding = ("  " ++) . (++ "  ")
        bg c       = (("%{B" ++ c ++ "}") ++) . (++ "%{B-}")
        fg c       = (("%{F" ++ c ++ "}") ++) . (++ "%{F-}")
        clickable f w   = fromMaybe (f w) (elemIndex w wsNames >>= \i -> Just $ "%{A1:wmctrl -s " ++ show i ++ ":}" ++ f w ++ "%{A}")
        layoutName s
            | "Mirror Tall" `isInfixOf` s = "MT"
            | "Tall" `isInfixOf` s        = " T"
            | "Full" `isInfixOf` s        = " F"
            | "OneBig" `isPrefixOf` s     = "OB"
            | otherwise                   = "  "
        floatNextStr s = case s of
            ""        -> " "
            otherwise -> "\xef\x8b\x92"
        onlyIf p f x = if p x then f x else ""

xpc = def { bgColor = WSP.bgColor myWispConfig
          , fgColor = WSP.fgColor myWispConfig
          , borderColor = WSP.fgColor myWispConfig
          , fgHLight = WSP.bgColor myWispConfig
          , bgHLight = WSP.fgColor myWispConfig
          , font = WSP.normalFont myWispConfig
          , height = 22
          }
