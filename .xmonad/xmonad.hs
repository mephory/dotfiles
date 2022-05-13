{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders
import XMonad.Actions.FloatSnap
import XMonad.Actions.DynamicProjects
import XMonad.Actions.Search
import XMonad.Actions.OnScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.BinaryColumn
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FloatNext
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, runInTerm)
import XMonad.Prompt
import Data.List (elemIndex, isPrefixOf, isInfixOf, find)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import System.Exit
import System.Environment (getEnv)
import System.FilePath (joinPath)
import Graphics.X11.ExtraTypes.XF86

import WorkspaceHelpers
import SubmapWithHints (submapWithHints)
import Passwords (passwordPrompt, genPasswordPrompt)
import UnicodeUtils (appendFileUtf8)
import ZoomWindow (toggleZoom)
import DynamicScratchpads (spawnDynamicSP, makeDynamicSP)
import FloatCenterWindow (centerFloatingWindow, makeFloatingCenterWindow)
import Opacity (changeOpacity, setOpacity)
import LowerDocks (addDock, delDock)
import PictureInPicture (togglePip, pipEventHook, pipPP)
import qualified Wisp as WS

import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dmenu as D
import qualified Data.Map as M

myWispConfig = WS.nord

myWorkspaces :: [WS]
myWorkspaces = [ WS "1"       0          (Just xK_1)           True
               , WS "2"       0          (Just xK_2)           True
               , WS "3"       0          (Just xK_3)           True
               , WS "4"       0          (Just xK_4)           True
               , WS "5"       0          (Just xK_5)           True
               , WS "6"       0          (Just xK_6)           True
               , WS "7"       0          (Just xK_7)           True
               , WS "8"       0          (Just xK_8)           True
               , WS "9"       0          (Just xK_9)           True
               , WS "\xf362"  0          (Just xK_Tab)         True
               , WS "'"       0          (Just xK_apostrophe)  True
               , WS "sys"     0          (Just xK_F1)          False
               , WS "F2"      0          (Just xK_F2)          True
               , WS "F3"      0          (Just xK_F3)          True
               , WS "NSP"     0          Nothing               False
               , WS "'1"      mod4Mask   (Just xK_1)           False
               , WS "'2"      mod4Mask   (Just xK_2)           False
               , WS "'3"      mod4Mask   (Just xK_3)           False
               , WS "'4"      mod4Mask   (Just xK_4)           False
               , WS "'5"      mod4Mask   (Just xK_5)           False
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
      , modMask            = mod1Mask
      , workspaces         = wsNames
      , normalBorderColor  = WS.unfocusedColor myWispConfig
      , focusedBorderColor = WS.focusedColor myWispConfig

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
      , handleEventHook    = delDock <+> myEventHook <+> pipEventHook
      , logHook            = myLogHook homeDir
      , startupHook        = myStartupHook homeDir
      , clientMask         = focusChangeMask .|. visibilityChangeMask .|. clientMask def
    }

projects =
    [ Project { projectName      = "sys"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do
                    spawn "easyeffects"
              }
    , Project { projectName      = "\xf362"
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
    ]

jisho = searchEngine "jisho" "https://jisho.org/search/"

-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "rofi -modi drun,run -show drun -show-icons -display-drun '\xf0e7'")
    , ((modm .|. shiftMask, xK_x     ), spawn "xkill")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp)
    , ((modm,               xK_m     ), windows W.focusMaster)
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_minus ), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_minus ), sendMessage (IncMasterN (1)))
    , ((modm .|. shiftMask, xK_f     ), toggleFloatNext >> runLogHook)
    , ((modm              , xK_Escape), spawn "slock")
    , ((modm              , xK_grave ), toggleWS' ["NSP"])
    , ((modm              , xK_o     ), withFocused toggleZoom)

    -- Prompts
    , ((modm              , xK_n     ), passwordPrompt xpc)
    , ((modm .|. shiftMask, xK_n     ), genPasswordPrompt xpc)

    -- Dunst
    , ((modm              , xK_bracketleft), spawn "dunstctl history-pop")
    , ((modm .|. shiftMask, xK_bracketleft), spawn "dunstctl context")
    , ((modm              , xK_bracketright), spawn "dunstctl close")
    , ((modm .|. shiftMask, xK_bracketright), spawn "dunstctl close-all")

    -- Various
    , ((modm              , xK_y     ), selectSearch jisho)
    , ((modm              , xK_slash),  submapWithHints xpc $
        [ ("q   recompile xmonad"       , (0, xK_q), spawn "xmonad --recompile && xmonad --restart")
        , ("f   view-fbg"               , (0, xK_f), spawn "view-fbg filtered")
        , ("k   default keyboard layout", (0, xK_k), spawn "setxkbmap us -option compose:ralt")
        , ("m-Q   exit xmonad"          , (modm .|. shiftMask, xK_q     ), io exitSuccess)
        ])
    , ((modm              , xK_u     ), spawn "qutebrowser :yank")
    , ((modm              , xK_semicolon), spawn "rofi-sd")
    , ((modm              , xK_period), switchProjectPrompt xpc)
    , ((modm              , xK_comma), spawn "imgs")
    , ((modm .|. shiftMask, xK_comma), spawn "imgs -s")
    , ((modm              , xK_BackSpace), spawn "rofi-spt")
    -- , ((modm              , xK_r     ), windows $ onScreen (W.greedyView "1'") (FocusTagVisible "'1") 1)
    , ((modm              , xK_r     ), windows $ onScreen (W.greedyView "'1") FocusCurrent 1)
    , ((modm .|. shiftMask, xK_r     ), windows $ W.shift "'1")
    , ((modm              , xK_d     ), windows $ onScreen (W.greedyView "'2") FocusCurrent 1)
    , ((modm .|. shiftMask, xK_d     ), windows $ W.shift "'2")
    -- , ((modm .|. controlMask, xK_d     ), onScreen' (spawn "xdotool --clearmodifiers key F5") FocusCurrent 1)
    , ((modm .|. controlMask, xK_d     ), onScreen' (spawn "xdotool key --clearmodifiers F5") FocusCurrent 1)
    , ((modm .|. shiftMask, xK_slash ), spawn "python /home/mephory/code/fbg/fbg.py toggle")
    , ((modm .|. shiftMask, xK_semicolon), togglePip)

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
    -- , ((0                 , xK_Print ), spawn "import -window root $HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png")
    -- , ((shiftMask         , xK_Print ), spawn "import +repage $HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png")
    , ((0                 , xK_Print ), spawn "imgs -d screenshots -n -t activewindow")
    , ((shiftMask         , xK_Print ), spawn "imgs -d screenshots -n -t select")
    , ((modm              , xK_0     ), spawn "upload-screenshot -window root")
    , ((modm .|. shiftMask, xK_0     ), spawn "upload-screenshot")
    , ((modm .|. shiftMask, xK_v     ), spawn "screenshot-google-image-search")
    -- , ((modm              , xK_Print ), spawn "screenshot-menu")
    , ((modm              , xK_Print ), spawn "imgs -d screenshots")
    -- , ((modm              , xK_d     ), spawn "imgs -s")

    -- Scratchpads
    , ((modm              , xK_v     ), namedScratchpadAction myScratchpads "terminal")
    , ((modm              , xK_c     ), namedScratchpadAction myScratchpads "terminal-2")
    , ((modm              , xK_x     ), namedScratchpadAction myScratchpads "terminal-3")
    , ((modm              , xK_z     ), namedScratchpadAction myScratchpads "music")
    , ((modm              , xK_b     ), namedScratchpadAction myScratchpads "irc")
    , ((modm .|. shiftMask, xK_p     ), namedScratchpadAction myScratchpads "color")
    , ((modm              , xK_backslash), namedScratchpadAction myScratchpads "bot-term")
    , ((modm .|. shiftMask, xK_o     ), namedScratchpadAction myScratchpads "icloud")
    -- , ((modm              , xK_r     ), namedScratchpadAction myScratchpads "discord")
    , ((modm              , xK_a     ), spawnDynamicSP "dyn1")
    , ((modm .|. shiftMask, xK_a     ), withFocused $ makeDynamicSP "dyn1")
    , ((modm              , xK_s     ), spawnDynamicSP "dyn2")
    , ((modm .|. shiftMask, xK_s     ), withFocused $ makeDynamicSP "dyn2")
    -- , ((modm              , xK_d     ), spawnDynamicSP "dyn3")
    -- , ((modm .|. shiftMask, xK_d     ), withFocused $ makeDynamicSP "dyn3")

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
    [((m .|. modm .|. wsm, k), f i)
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


myLayout = noBorders $ 
    onWorkspace "1" (full ||| fullscreen ||| tiled ||| mtiled) $
    onWorkspace "5" fullscreen $
    onWorkspace "7" (focusIndicator $ avoidStruts $ Tall 1 (3/100) (1/4)) $
    onWorkspace "'1" (focusIndicator $ avoidStruts $ verticalLayout) $
    onWorkspace "'2" (focusIndicator $ avoidStruts $ verticalLayout) $
    onWorkspace "'3" (focusIndicator $ avoidStruts $ verticalLayout) $
    onWorkspace "'4" (focusIndicator $ avoidStruts $ verticalLayout) $
    onWorkspace "'5" (focusIndicator $ avoidStruts $ verticalLayout) $
    onWorkspace "sys" (focusIndicator $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ avoidStruts $ Mirror Grid)
    defaultConf
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled      = focusIndicator $ spaced $ avoidStruts $ Tall nmaster delta ratio
        mtiled     = focusIndicator $ spaced $ avoidStruts $ Mirror (Tall 1 (3/100) (1/2))
        full       = avoidStruts $ spaced $ Full
        fullscreen = Full
        defaultConf = tiled ||| mtiled ||| full
        verticalLayout = BinaryColumn 0.0 32 ||| BinaryColumn 1.0 32 ||| BinaryColumn 2.0 32
        horizontalLayout = focusIndicator $ spaced $ avoidStruts $ ThreeColMid 1 (3/100) (1/2)

        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 0.6
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        spaced = spacingRaw True (Border 1 1 1 1) False (Border 0 0 0 0) True
        focusIndicator = noFrillsDeco shrinkText focusIndicatorTheme

myManageHook = composeAll
    [ resource  =? "desktop_window"      --> doIgnore
    , resource  =? "kdesktop"            --> doIgnore
    , className =? "Firefox"             --> doShift "1"
    -- , className =? "qutebrowser"         --> doShift "1"
    , className =? "explorer.exe"        --> doShift "9"
    , title     =? "Wine System Tray"    --> doShift "9"
    , className =? "dota2"               --> doShift "5" <+> (doF . W.sink =<< ask)
    , resource  =? "polybar-pavucontrol" --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "vselect"             --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "vselect-record-area" --> placeHook (fixed (0, 0)) <+> doFloat
    , title     =? "pinentry"            --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , className =? "Pinentry-gtk-2"      --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , className =? "feh-float"           --> doF W.shiftMaster <+> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "Microsoft Teams Notification" --> placeHook (fixed (1, 1)) <+> doFloat
    , title     =? "Picture-in-Picture"  --> (customFloating $ W.RationalRect 0.65 0.65 0.3 0.3)
    , className =? "Steam"               --> doShift "7"
    , className =? "discord"             --> doShift "'1"
    , className =? "willow"              --> doFloat
    ]

floatPlacement = placeHook (withGaps (10, 10, 10, 10) $ smart (0.5, 0.2))

myEventHook = mempty

myStartupHook homeDir = do
    WS.activateWispConfig myWispConfig
    safeSpawn (joinPath [homeDir, ".config", "alacritty", "build_config.sh"]) []
    safeSpawn "restart-polybar" []
    safeSpawn "restart-dunst" []

myScratchpads = [ NS "terminal"   spawnTerminal  findTerminal  manageSP
                , NS "terminal-2" spawnTerminal2 findTerminal2 manageSP
                , NS "terminal-3" spawnTerminal3 findTerminal3 manageSP
                , NS "bot-term"   spawnBotTerm   findBotTerm   manageBotTermSP
                , NS "music"      spawnMusic     findMusic     manageSP
                , NS "irc"        spawnIrc       findIrc       manageIrcSP
                , NS "server"     spawnServer    findServer    manageIrcSP
                , NS "color"      spawnColor     findColor     manageColorSP
                , NS "icloud"     spawnIcloud    findIcloud    manageIcloudSP
                -- , NS "discord"    spawnDiscord   findDiscord   manageDiscordSP
                ]
    where
        spawnTerminal  = "alacritty --class scratchpad --title 'Alacritty (v)'"
        findTerminal   = resource =? "scratchpad"
        spawnTerminal2 = "alacritty --class scratchpad-2 --title 'Alacritty (c)'"
        findTerminal2  = resource =? "scratchpad-2"
        spawnTerminal3 = "alacritty --class scratchpad-3 --title 'Alacritty (x)'"
        findTerminal3  = resource =? "scratchpad-3"
        spawnBotTerm   = "alacritty --class bot-term"
        findBotTerm    = resource =? "bot-term"
        spawnMusic     = "alacritty --class music -e pulsemixer"
        findMusic      = resource =? "music"
        spawnIrc       = "alacritty --class irc -e ssh -t mephory LANG=en_US.utf8 TERM=xterm-256color tmux attach -t irc"
        findIrc        = resource =? "irc"
        spawnServer    = "alacritty --class server-scratch -e ssh -t mephory LANG=en_US.utf8 TERM=xterm-256color tmux attach -t apps"
        findServer      = resource =? "server-scratch"
        spawnColor     = "gcolor3"
        findColor      = resource =? "gcolor3"
        spawnIcloud    = "icloud"
        findIcloud     = className =? "icloud"
        spawnDiscord   = "discord"
        findDiscord    = className =? "discord"
        manageSP = customFloating $ centeredRect 0.5 0.5
        manageBotTermSP = customFloating $ W.RationalRect 0 (1-0.4) 1 0.4
        manageIrcSP = (customFloating $ centeredRect 0.6 0.6)
        manageColorSP = placeHook (fixed (0.5, 0.5)) <+> doFloat
        manageIcloudSP = customFloating $ W.RationalRect 0 0 1 0.6
        manageDiscordSP = (customFloating $ centeredRect 0.7 0.7)
        centeredRect w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

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
    , ppExtras          = [willFloatNextPP floatNextStr, pipPP id]
    , ppOrder           = \(w:l:t:es) -> [w, l] ++ es ++ [t]
    , ppOutput          = appendFileUtf8 "/tmp/workspace-info" . (++"\n")
    }
    where
        hiddenFmt  = bg (WS.bgColor myWispConfig) .
                     fg (WS.fgSecondary myWispConfig) .
                     addPadding
        currentFmt = bg (WS.bgSecondary myWispConfig) .
                     fg (WS.fgColor myWispConfig) .
                     addPadding
        urgentFmt  = bg (WS.color1 myWispConfig) .
                     fg (WS.fgColor myWispConfig) .
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
            otherwise -> "·"
        onlyIf p f x = if p x then f x else ""

xpc = def { bgColor = WS.bgColor myWispConfig
          , fgColor = WS.fgColor myWispConfig
          , borderColor = WS.fgColor myWispConfig
          , fgHLight = WS.bgColor myWispConfig
          , bgHLight = WS.fgColor myWispConfig
          , font = WS.normalFont myWispConfig
          , height = 22
          }

focusIndicatorTheme :: Theme
focusIndicatorTheme =
  def { fontName              = "xft:inconsolata:size=10"
      , inactiveBorderColor   = WS.unfocusedColor myWispConfig
      , inactiveColor         = WS.unfocusedColor myWispConfig
      , inactiveTextColor     = WS.unfocusedColor myWispConfig
      , activeBorderColor     = WS.focusedColor myWispConfig
      , activeColor           = WS.focusedColor myWispConfig
      , activeTextColor       = WS.focusedColor myWispConfig
      , urgentBorderColor     = "#ff0000"
      , urgentTextColor       = "#ff0000"
      , decoHeight            = 4
      }

