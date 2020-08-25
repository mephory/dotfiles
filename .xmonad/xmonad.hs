{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.NoBorders
import XMonad.Actions.FloatSnap
import XMonad.Actions.DynamicProjects
import XMonad.Layout.PerWorkspace
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
import System.Environment
import Graphics.X11.ExtraTypes.XF86

import WorkspaceHelpers
import SubmapWithHints (submapWithHints)
import Passwords (passwordPrompt, genPasswordPrompt)
import UnicodeUtils (appendFileUtf8)
import ZoomWindow (toggleZoom)
import DynamicScratchpads (spawnDynamicSP, makeDynamicSP)
import FloatCenterWindow (centerFloatingWindow, makeFloatingCenterWindow)
import Opacity (changeOpacity, setOpacity)
import SetXrdbEnv (setXrdbEnv)
import LowerDocks (addDock, delDock)
import qualified PictureInPicture as P

import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dmenu as D
import qualified Data.Map as M

myWorkspaces :: [WS]
myWorkspaces = [ WS "1"       (Just xK_1)           True
               , WS "2"       (Just xK_2)           True
               , WS "3"       (Just xK_3)           True
               , WS "4"       (Just xK_4)           True
               , WS "5"       (Just xK_5)           True
               , WS "6"       (Just xK_6)           True
               , WS "7"       (Just xK_7)           True
               , WS "8"       (Just xK_8)           True
               , WS "9"       (Just xK_9)           True
               , WS "\xf362"  (Just xK_Tab)         True
               , WS "'"       (Just xK_apostrophe)  True
               , WS "sys"     (Just xK_F1)          False
               , WS "F2"      (Just xK_F2)          True
               , WS "NSP"     Nothing               False
               ]

wsNames :: [String]
wsNames = map wsName myWorkspaces

findWs :: String -> Maybe WS
findWs s = find (\w -> wsName w == s) myWorkspaces

workspaceKey :: String -> Maybe KeySym
workspaceKey s = findWs s >>= wsKey

workspaceVisible :: String -> Bool
workspaceVisible s = fromMaybe False $ findWs s >>= Just . wsVisible

main = do
    homeDir <- getEnv "HOME"
    setXrdbEnv
    xpc <- buildXPC
    focusIndicatorTheme <- buildFocusIndicatorTheme
    safeSpawn "mkfifo" ["/tmp/workspace-info"]
    xmonad $ docks $ ewmh $ dynamicProjects projects $ withUrgencyHook NoUrgencyHook def {
      -- simple stuff
        terminal           = "termite"
      , focusFollowsMouse  = True
      , borderWidth        = 0
      , modMask            = mod1Mask
      , workspaces         = wsNames
      , normalBorderColor  = "#3b4252"
      , focusedBorderColor = "#3c5a70"

      -- key bindings
      , keys               = \x -> myKeys x xpc
      , mouseBindings      = myMouseBindings

      -- hooks, layouts
      , layoutHook         = myLayout focusIndicatorTheme
      , manageHook         = addDock
                             <+> myManageHook
                             <+> namedScratchpadManageHook myScratchpads
                             <+> floatPlacement
                             <+> floatNextHook
      , handleEventHook    = delDock <+> myEventHook
      -- , logHook            = P.pipLogHook <+> myLogHook homeDir
      , logHook            = myLogHook homeDir
      , startupHook        = myStartupHook
    }

projects =
    [ Project { projectName      = "sys"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do
                    spawn "termite"
                    spawn "pavucontrol"
                    runInTerm "" "htop"
                    spawn "termite"
              }
    , Project { projectName      = "\xf362"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do
                    spawn "spotify"
              }
    ]

-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) xpc = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "rofi -modi drun,run -show drun -show-icons")
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
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_f     ), toggleFloatNext >> runLogHook)
    , ((modm              , xK_Escape), spawn "gllock")
    , ((modm              , xK_grave ), toggleWS' ["NSP"])
    , ((modm              , xK_o     ), withFocused toggleZoom)

    -- Prompts
    , ((modm              , xK_n     ), passwordPrompt xpc)
    , ((modm .|. shiftMask, xK_n     ), genPasswordPrompt xpc)

    -- Various
    , ((modm              , xK_i     ), spawn "tesstest")
    , ((modm .|. shiftMask, xK_slash ), spawn "sodomsg")
    -- , ((modm              , xK_semicolon), withFocused P.toggle)
    , ((modm .|. shiftMask, xK_semicolon), spawn "rofi-twitch")
    , ((modm              , xK_slash),  submapWithHints xpc $
        [ ("q   recompile xmonad"       , (0, xK_q), spawn "xmonad --recompile && xmonad --restart")
        , ("m   draw dota minimap image", (0, xK_m), spawn "dota-minimap-image")
        , ("f   view facebook graph"    , (0, xK_f), spawn "view-fbg filtered")
        , ("k   default keyboard layout", (0, xK_k), spawn "setxkbmap us -option compose:ralt")
        , ("e   edit config file"       , (0, xK_e), spawn "edit-config")
        , ("m-Q   exit xmonad"          , (modm .|. shiftMask, xK_q     ), io exitSuccess)
        ])
    , ((modm              , xK_u     ), spawn "qutebrowser")

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
    , ((modm              , xK_r     ) , withFocused $ \w -> setOpacity w (2/3))
    , ((modm .|. shiftMask, xK_r     ) , withFocused $ \w -> setOpacity w 1.0)

    -- Screenshots
    , ((0                 , xK_Print ), spawn "import -window root $HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png")
    , ((shiftMask         , xK_Print ), spawn "import +repage $HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png")
    , ((modm              , xK_0     ), spawn "upload-screenshot -window root")
    , ((modm .|. shiftMask, xK_0     ), spawn "upload-screenshot")
    , ((modm .|. shiftMask, xK_v     ), spawn "screenshot-google-image-search")
    , ((modm              , xK_Print ), submapWithHints xpc $ screenshotMap 1 xpc)

    -- Scratchpads
    , ((modm              , xK_v     ), namedScratchpadAction myScratchpads "terminal")
    , ((modm              , xK_c     ), namedScratchpadAction myScratchpads "terminal-2")
    , ((modm              , xK_z     ), namedScratchpadAction myScratchpads "music")
    , ((modm              , xK_b     ), namedScratchpadAction myScratchpads "irc")
    , ((modm .|. shiftMask, xK_p     ), namedScratchpadAction myScratchpads "color")
    , ((modm              , xK_backslash), namedScratchpadAction myScratchpads "bot-term")
    , ((modm .|. shiftMask, xK_o     ), namedScratchpadAction myScratchpads "icloud")
    , ((modm              , xK_x     ), namedScratchpadAction myScratchpads "discord")
    , ((modm              , xK_a     ), spawnDynamicSP "dyn1")
    , ((modm .|. shiftMask, xK_a     ), withFocused $ makeDynamicSP "dyn1")
    , ((modm              , xK_s     ), spawnDynamicSP "dyn2")
    , ((modm .|. shiftMask, xK_s     ), withFocused $ makeDynamicSP "dyn2")
    , ((modm              , xK_d     ), spawnDynamicSP "dyn3")
    , ((modm .|. shiftMask, xK_d     ), withFocused $ makeDynamicSP "dyn3")

    -- Media Keys
    , ((0, xF86XK_AudioMute), spawn "amixer sset Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "pulsemixer --change-volume -1")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pulsemixer --change-volume +1")
    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    , ((0, xF86XK_Display), spawn "xbacklight -set 85")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), f i)
        | (i, (Just k)) <- map (\ws -> (wsName ws, wsKey ws)) myWorkspaces
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
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), runUnlessIgnored (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), runUnlessIgnored (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), runUnlessIgnored (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modm , button4) , \w -> changeOpacity w (5/100))
    , ((modm , button5) , \w -> changeOpacity w (-5/100))
    , ((0    , 10 :: Button), \_ -> spawn "polybar-playerctl toggle")
    , ((0    , 11 :: Button), \_ -> spawn "polybar-playerctl next")
    , ((0    , 12 :: Button), \_ -> spawn "polybar-playerctl switch")
    ]

screenshotMap n xpc =
    [ ("Current Screenshot: " ++ show n, (0, xK_q), mempty)
    , ("t   take screenshot with mode",
        (0, xK_t), submapWithHints xpc
          [
            ("f   Full", (0, xK_f), spawn $ unwords ["sleep", "0.2", ";", "import", "-window", "root", newScreenshotName])
          , ("c   Choose Window/Area", (0, xK_c), spawn $ unwords ["sleep", "0.2", ";", "import", newScreenshotName])
          , ("w   Focused Window", (0, xK_w), spawn $ unwords ["sleep", "0.2", ";", "import", "-window", "$(xdotool getwindowfocus -f)", newScreenshotName])
          , ("1   Screen 1", (0, xK_1), spawn $ unwords ["sleep", "0.2", ";", "import", "-window", "root", "-crop", "2560x1440+0+0", "+repage", newScreenshotName])
          , ("2   Screen 2", (0, xK_2), spawn $ unwords ["sleep", "0.2", ";", "import", "-window", "root", "-crop", "2560x1440+2560+0", "+repage", newScreenshotName])
          , ("3   Screen 3", (0, xK_3), spawn $ unwords ["sleep", "0.2", ";", "import", "-window", "root", "-crop", "2560x1440+5120+0", "+repage", newScreenshotName])
          ])
    , ("v   view",
        (0, xK_v), spawn $ unwords ["feh", filename])
    , ("c   draw a circle",
        (0, xK_c), spawn $ unwords ["vcircle", filename, nextFilename])
    , ("r   draw a rectangle",
        (0, xK_r), spawn $ unwords ["vrect", filename, nextFilename])
    , ("x   crop",
        (0, xK_x), spawn $ unwords ["vcrop", filename, nextFilename])
    , ("u   upload",
        (0, xK_u), spawn $ unwords ["upload", "--notify", "-p", filename])
    , ("U   upload with mode",
        (shiftMask, xK_u), submapWithHints xpc
          [
            ("p - Private", (0, xK_p), spawn $ unwords ["upload", "--notify", "-p", filename])
          , ("a - Public", (0, xK_a), spawn $ unwords ["upload", "--notify", filename])
          , ("t - Temporary", (0, xK_t), spawn $ unwords ["upload --notify", "-t", filename])
          ])
    , ("D   delete",
        (shiftMask, xK_d), spawn $ unwords ["rm", filename])
    , ("1   operate on latest screenshot",
        (0, xK_1), submapWithHints xpc $ screenshotMap 1 xpc)
    , ("2   operate on second latest screenshot",
        (0, xK_2), submapWithHints xpc $ screenshotMap 2 xpc)
    , ("3   operate on third latest screenshot",
        (0, xK_3), submapWithHints xpc $ screenshotMap 3 xpc)
    , ("4   operate on fourth latest screenshot",
        (0, xK_4), submapWithHints xpc $ screenshotMap 4 xpc)
    , ("5   operate on fifth latest screenshot",
        (0, xK_5), submapWithHints xpc $ screenshotMap 5 xpc)
    ]
        where filename = "\"$(nth-last-screenshot " ++ show n ++ ")\""
              nextFilename = "\"$(nextname " ++ filename ++ ")\""
              newScreenshotName = "\"$HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png\""


myLayout fiTheme = noBorders $ 
    onWorkspace "1" (full ||| fullscreen ||| tiled ||| mtiled) $
    onWorkspace "5" fullscreen $
    onWorkspace "7" (focusIndicator $ avoidStruts $ Tall 1 (3/100) (1/4)) $
    onWorkspace "sys" (focusIndicator $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ avoidStruts $ Grid)
    defaultConf
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled      = focusIndicator $ spaced $ avoidStruts $ Tall nmaster delta ratio
        mtiled     = focusIndicator $ spaced $ avoidStruts $ Mirror (Tall 1 (3/100) (1/2))
        full       = avoidStruts $ spaced $ Full
        fullscreen = Full
        defaultConf = tiled ||| mtiled ||| full

        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 0.6
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        spaced = spacingRaw True (Border 5 5 5 5) False (Border 8 8 8 8) True
        focusIndicator = noFrillsDeco shrinkText fiTheme

myManageHook = composeAll
    [ resource  =? "desktop_window"      --> doIgnore
    , resource  =? "kdesktop"            --> doIgnore
    , className =? "Firefox"             --> doShift "1"
    , className =? "qutebrowser"         --> doShift "1"
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
    ]

floatPlacement = placeHook (withGaps (10, 10, 10, 10) $ smart (0.5, 0.2))

myEventHook = mempty

myStartupHook = do
    safeSpawn "reload-termite-config" []
    safeSpawn "restart-polybar" []
    safeSpawn "restart-dunst" []

myScratchpads = [ NS "terminal"   spawnTerminal  findTerminal  manageSP
                , NS "terminal-2" spawnTerminal2 findTerminal2 manageSP
                , NS "terminal-3" spawnTerminal3 findTerminal3 manageSP
                , NS "bot-term"   spawnBotTerm   findBotTerm   manageBotTermSP
                , NS "music"      spawnMusic     findMusic     manageSP
                , NS "irc"        spawnIrc       findIrc       manageIrcSP
                , NS "color"      spawnColor     findColor     manageColorSP
                , NS "icloud"     spawnIcloud    findIcloud    manageIcloudSP
                , NS "discord"    spawnDiscord   findDiscord   manageDiscordSP
                ]
    where
        spawnTerminal  = "termite --name scratchpad"
        findTerminal   = resource =? "scratchpad"
        spawnTerminal2 = "termite --name scratchpad-2"
        findTerminal2  = resource =? "scratchpad-2"
        spawnTerminal3 = "termite --name scratchpad-3"
        findTerminal3  = resource =? "scratchpad-3"
        spawnBotTerm   = "termite --name bot-term --class invertedTerm"
        findBotTerm    = resource =? "bot-term"
        spawnMusic     = "termite --name music -e 'pulsemixer'"
        findMusic      = resource =? "music"
        -- spawnIrc       = "xterm -name irc -e 'tmux-attach-or-new irc weechat'"
        spawnIrc       = "termite --name irc -e 'ssh -t mephory LANG=en_US.utf8 TERM=xterm-256color tmux attach -t irc'"
        -- spawnIrc       = "xterm -name irc -e 'ssh -t mephory tmux attach -t irc'"
        findIrc        = resource =? "irc"
        spawnColor     = "gcolor3"
        findColor      = resource =? "gcolor3"
        spawnIcloud    = "icloud"
        findIcloud     = className =? "icloud"
        spawnDiscord   = "discord"
        findDiscord   = className =? "discord"
        manageSP = customFloating $ centeredRect 0.5 0.5
        manageBotTermSP = customFloating $ W.RationalRect 0 (1-0.4) 1 0.4
        manageIrcSP = (customFloating $ centeredRect 0.6 0.6)
        manageColorSP = placeHook (fixed (0.5, 0.5)) <+> doFloat
        manageIcloudSP = customFloating $ W.RationalRect 0 0 1 0.6
        manageDiscordSP = (customFloating $ centeredRect 0.7 0.7) <+> (ask >>= \w -> liftX (setOpacity w (9/10)) >> mempty)
        centeredRect w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

myLogHook homeDir = dynamicLogWithPP $ def {
      ppCurrent         = clickable currentFmt
    , ppVisible         = clickable visibleFmt
    , ppHidden          = onlyIf workspaceVisible (clickable addPadding)
    -- , ppHiddenNoWindows = onlyIf (/= "NSP") $ clickable [dzenColor "#93a1a1" "#002b36" . addPadding . wsNum]
    , ppHiddenNoWindows = \_ -> ""
    , ppUrgent          = clickable urgentFmt
    , ppSep             = "  "
    , ppWsSep           = "" , ppLayout          = ("%{u#458588}%{+u} " ++) . (++ " %{-u}") . layoutName , ppTitle           = \_ -> ""
    , ppExtras          = [willFloatNextPP floatNextStr, P.pipPP pipStr]
    , ppOrder           = \(w:l:t:es) -> [w, l] ++ es ++ [t]
    , ppOutput          = appendFileUtf8 "/tmp/workspace-info" . (++"\n")
    }
    where
        addPadding      = ("  " ++) . (++ "  ")
        currentFmt      = (" %{u#cc241d}%{+u} " ++) . (++ " %{-u} ")
        visibleFmt      = (" %{u#484848}%{+u} " ++) . (++ " %{-u} ")
        urgentFmt       = ("%{B#cc241d}  " ++) . (++ "  %{B-}")
        clickable f w   = fromMaybe (f w) (elemIndex w wsNames >>= \i -> Just $ "%{A1:wmctrl -s " ++ show i ++ ":}" ++ f w ++ "%{A}")
        layoutName s
            | "Mirror Tall" `isInfixOf` s = "\xe003"
            | "Tall" `isInfixOf` s        = "\xe002"
            | "Full" `isInfixOf` s        = "\xe001"
            | "OneBig" `isPrefixOf` s     = "O"
            | otherwise                   = "\xe005"
        floatNextStr s = case s of
            ""        -> " "
            otherwise -> "·"
        pipStr s = case s of
            "PiP" -> "\xf2d2"
            ""    -> " "
        contextNameStr s = case s of
            "default" -> Nothing
            x         -> Just x
        onlyIf p f x = if p x then f x else ""

buildXPC = do
    bgColor <- getEnv "XRDBBACKGROUND"
    fgColor <- getEnv "XRDBFOREGROUND"
    borderColor <- getEnv "XRDBFOREGROUND"
    fgHLight <- getEnv "XRDBBACKGROUND"
    bgHLight <- getEnv "XRDBFOREGROUND"
    return $ def
        { bgColor = bgColor
        , fgColor = fgColor
        , borderColor = borderColor
        , fgHLight = fgHLight
        , bgHLight = bgHLight
        , font = "xft:hack:size=11"
        , height = 22
        }

buildFocusIndicatorTheme :: IO Theme
buildFocusIndicatorTheme = do
    inactive <- getEnv "XRDBCOLOR0"
    -- active <- getEnv "XRDBCOLOR4"
    active <- return "#3c5a70"
    return $ def
        { fontName              = "xft:inconsolata:size=10"
        , inactiveBorderColor   = inactive
        , inactiveColor         = inactive
        , inactiveTextColor     = inactive
        , activeBorderColor     = active
        , activeColor           = active
        , activeTextColor       = active
        , urgentBorderColor     = "#ff0000"
        , urgentTextColor       = "#ff0000"
        , decoHeight            = 4
        }
