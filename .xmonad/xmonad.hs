{-# LANGUAGE FlexibleInstances #-}
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.NoBorders
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatSnap
import XMonad.Actions.DynamicProjects
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
-- import MyManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FloatNext
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Loggers
import XMonad.Util.Run (safeSpawn, runInTerm)
import XMonad.Prompt
import Data.List (elemIndex, isPrefixOf, isInfixOf)
import Data.Maybe (fromJust, fromMaybe)
import System.Exit
import System.Environment
import Graphics.X11.ExtraTypes.XF86

import SubmapWithHints (submapWithHints)
import Passwords (passwordPrompt, genPasswordPrompt)
import UnicodeUtils (appendFileUtf8)
import FullscreenToggle (toggleFullscreen)
import DynamicScratchpads (spawnDynamicSP, makeDynamicSP)
import FloatCenterWindow (centerFloatingWindow, makeFloatingCenterWindow)
import FloatsOnTop (floatsOnTop)
import Polybar (addDock, delDock)
import Opacity (changeOpacity)

import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dmenu as D
import qualified Contexts as C
import qualified Data.Map as M

instance Read (Layout Window) where
    readsPrec _ = readsLayout (Layout myLayout)

main = do
    homeDir <- getEnv "HOME"
    safeSpawn "mkfifo" ["/tmp/workspace-info"]
    xmonad $ ewmh $ dynamicProjects projects $ withUrgencyHook NoUrgencyHook def {
      -- simple stuff
        terminal           = "urxvt"
      , focusFollowsMouse  = True
      , borderWidth        = 0
      , modMask            = mod1Mask
      , workspaces         = myWorkspaces
      , normalBorderColor  = "#2b2b2b"
      , focusedBorderColor = "#383838"

      -- key bindings
      , keys               = myKeys
      , mouseBindings      = myMouseBindings

      -- hooks, layouts
      , layoutHook         = myLayout
      , manageHook         = floatsOnTop
                             <+> addDock
                             <+> manageDocks
                             <+> insertPosition Below Newer
                             <+> myManageHook
                             <+> namedScratchpadManageHook myScratchpads
                             <+> floatPlacement
                             <+> floatNextHook
      , handleEventHook    = delDock <+> docksEventHook <+> myEventHook
      , logHook            = myLogHook homeDir
      , startupHook        = docksStartupHook <+> myStartupHook
    }

myXPConfig = def
    { bgColor     = "#282828"
    , fgColor     = "#ebdbb2"
    , borderColor = "#ebdbb2"
    , fgHLight    = "#282828"
    , bgHLight    = "#ebdbb2"
    , font        = "xft:inconsolata:size=12"
    , height      = 22
    }

myWorkspaces    = ["1","2","3","4","5","6","7","8","9","q",";","'","sys","F2","F3","F4"] ++ ["NSP"]
myBrowser    = "qutebrowser"

projects =
    [ Project { projectName      = "sys"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do
                    runInTerm "" "glances"
                    spawn "pavucontrol"
                    runInTerm "" "htop"
                    spawn "urxvt"
              }
    ]

-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ map (\(a, b, c) -> (b, c)) (keysWithDescription conf)
keysWithDescription conf@(XConfig {XMonad.modMask = modm}) =
    [ ("Launch a Terminal",
        (modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ("Run Program",
        (modm,               xK_p     ), spawn "rofi -modi drun,run -show drun -show-icons")
    , ("Select and Kill Window",
        (modm .|. shiftMask, xK_x     ), spawn "xkill")
    , ("Close Focused Window",
        (modm .|. shiftMask, xK_c     ), kill)
    , ("Rotate through the available layouts",
        (modm,               xK_space ), sendMessage NextLayout)
    , ("Reset the layout to default",
        (modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ("Move focus to the next window",
        (modm,               xK_j     ), windows W.focusDown)
    , ("Move focus to the previous window",
        (modm,               xK_k     ), windows W.focusUp  )
    , ("Move focus to the master window",
        (modm,               xK_m     ), windows W.focusMaster  )
    , ("Swap the focused window and the master window",
        (modm,               xK_Return), windows W.swapMaster)
    , ("Swap the focused window with the next window",
        (modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ("Swap the focused window with the previous window",
        (modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ("Shrink the master area",
        (modm,               xK_h     ), sendMessage Shrink)
    , ("Expand the master area",
        (modm,               xK_l     ), sendMessage Expand)
    , ("Push window back into tiling",
        (modm,               xK_t     ), withFocused $ windows . W.sink)
    , ("Increment the number of windows in the master area",
        (modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ("Deincrement the number of windows in the master area",
        (modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ("Float next window",
        (modm .|. shiftMask, xK_f     ), toggleFloatNext >> runLogHook)
    , ("Toggle border",
        (modm              , xK_u     ), withFocused toggleBorder)
    , ("Lock screen",
        (modm              , xK_Escape), spawn "slock")
    , ("Toggle between two most recently viewed workspaces",
        (modm              , xK_grave ), toggleWS' ["NSP"])
    , ("Toggle fullscreen for focused window",
        (modm              , xK_o     ), withFocused toggleFullscreen)

    -- Prompts
    , ("Copy password to clipboard",
        (modm              , xK_n     ), passwordPrompt myXPConfig)
    , ("Generate password",
        (modm .|. shiftMask, xK_n     ), genPasswordPrompt myXPConfig)

--, ((modm                  , xK_a        ), sequence_ $ [windows $ copy i | i <- XMonad.workspaces conf])    -- Pin to all workspaces
--, ((modm .|. controlMask  , xK_a        ), windows $ kill8 )    -- remove from all but current


    -- Various
    , ("Toggle Polybar",
        (modm              , xK_i     ), spawn "polybar-msg cmd toggle" >> sendMessage ToggleStruts)
    , ("sodomsg",
        (modm .|. shiftMask, xK_slash ), spawn "sodomsg")
    , ("Switch context",
        (modm              , xK_BackSpace), C.listContextNames >>= D.menuArgs "rofi" ["-dmenu", "-p", "Context"] >>= C.createAndSwitchContext)
    , ("Delete context",
        (modm .|. shiftMask, xK_BackSpace), C.listContextNames >>= D.menuArgs "rofi" ["-dmenu", "-p", "Context"] >>= C.deleteContext >> return ())
    , ("Action Menu",
        (modm              , xK_slash),  submapWithHints myXPConfig $
            [ ("q - recompile xmonad"       , (0, xK_q), spawn "xmonad --recompile && xmonad --restart")
            , ("m - draw dota minimap image", (0, xK_m), spawn "dota-minimap-image")
            , ("f - view facebook graph"    , (0, xK_f), spawn "view-fbg filtered")
            , ("m-Q - exit xmonad"          , (modm .|. shiftMask, xK_q     ), io exitSuccess)
            ])

    -- Floating Positions
    , ("Float current window in the center of the screen",
        (modm              , xK_f     ) , withFocused makeFloatingCenterWindow)
    , ("Move window to the left",
        (modm              , xK_Left  ) , withFocused $ snapMove L Nothing)
    , ("Move window the the right",
        (modm              , xK_Right ) , withFocused $ snapMove R Nothing)
    , ("Move window up",
        (modm              , xK_Up    ) , withFocused $ snapMove U Nothing)
    , ("Move window down",
        (modm              , xK_Down  ) , withFocused $ snapMove D Nothing)
    , ("Resize window left",
        (modm .|. shiftMask, xK_Left  ) , withFocused $ snapShrink R Nothing)
    , ("Resize window right",
        (modm .|. shiftMask, xK_Right ) , withFocused $ snapGrow R Nothing)
    , ("Resize window down",
        (modm .|. shiftMask, xK_Up    ) , withFocused $ snapShrink D Nothing)
    , ("Resize window up",
        (modm .|. shiftMask, xK_Down  ) , withFocused $ snapGrow D Nothing)
    , ("Center window",
        (modm              , xK_g     ) , withFocused centerFloatingWindow)
    , ("Make window transparent",
        (modm              , xK_r     ) , withFocused $ \w -> setOpacity w 0.5)
    , ("Make window opaque",
        (modm .|. shiftMask, xK_r     ) , withFocused $ \w -> setOpacity w 1.0)

    -- Screenshots
    , ("Take a screenshot",
        (0                 , xK_Print ), spawn "import -window root $HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png")
    , ("Take a screenshot of an area",
        (shiftMask         , xK_Print ), spawn "import +repage $HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png")
    , ("Take and upload a screenshot",
        (modm              , xK_0     ), spawn "upload-screenshot -window root")
    , ("Take and upload a screenshot of an area",
        (modm .|. shiftMask, xK_0     ), spawn "upload-screenshot")
    , ("Search google images for screenshot",
        (modm .|. shiftMask, xK_v     ), spawn "screenshot-google-image-search")
    , ("Various screenshot actions",
        (modm              , xK_Print ), submapWithHints myXPConfig $ screenshotMap 1)

    -- Scratchpads
    , ("Spawn Terminal 1",
        (modm              , xK_v     ), namedScratchpadAction myScratchpads "terminal")
    , ("Spawn Terminal 2",
        (modm              , xK_c     ), namedScratchpadAction myScratchpads "terminal-2")
    , ("Spawn Music/Volume control",
        (modm              , xK_z     ), namedScratchpadAction myScratchpads "music")
    , ("Spawn IRC",
        (modm              , xK_b     ), namedScratchpadAction myScratchpads "irc")
    , ("Spawn Color Picker",
        (modm .|. shiftMask, xK_p     ), namedScratchpadAction myScratchpads "color")
    , ("Spawn Terminal 3",
        (modm              , xK_backslash), namedScratchpadAction myScratchpads "bot-term")
    , ("Spawn iCloud",
        (modm .|. shiftMask, xK_o     ), namedScratchpadAction myScratchpads "icloud")
    , ("Spawn Discord",
        (modm              , xK_x     ), namedScratchpadAction myScratchpads "discord")
    , ("Spawn Dynamic 1",
        (modm              , xK_a     ), spawnDynamicSP "dyn1")
    , ("Set Dynamic 1",
        (modm .|. shiftMask, xK_a     ), withFocused $ makeDynamicSP "dyn1")
    , ("Spawn Dynamic 2",
        (modm              , xK_s     ), spawnDynamicSP "dyn2")
    , ("Set Dynamic 2",
        (modm .|. shiftMask, xK_s     ), withFocused $ makeDynamicSP "dyn2")
    , ("Spawn Dynamic 3",
        (modm              , xK_d     ), spawnDynamicSP "dyn3")
    , ("Set Dynamic 3",
        (modm .|. shiftMask, xK_d     ), withFocused $ makeDynamicSP "dyn3")

    -- Media Keys
    , ("Toggle Mute",
        (0, xF86XK_AudioMute), spawn "amixer sset Master toggle")
    , ("Decrease Volumne",
        (0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 2dB-")
    , ("Increase Volume",
        (0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 2dB+")
    , ("Lower Backlight",
        (0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ("Raise Backlight",
        (0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    , ("Set Defualt Backlight",
        (0, xF86XK_Display), spawn "xbacklight -set 85")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [("Switch to workspace " ++ (show i), (m .|. modm, k), windows $ f i)
        | (i, k) <- (zip (XMonad.workspaces conf) [xK_1 .. xK_9]) ++ [
              ("sys", xK_F1)
            , ("q", xK_q)
            , (";", xK_semicolon)
            , ("'", xK_apostrophe)
            , ("F2", xK_F2)
            , ("F3", xK_F3)
            , ("F4", xK_F4)
        ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [("Move window to workspace " ++ (show sc), (m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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
    ]

screenshotMap n =
    [ ("Current Screenshot: " ++ show n, (0, xK_q), mempty)
    , ("t   take screenshot with mode",
        (0, xK_t), submapWithHints myXPConfig
          [
            ("f - Full", (0, xK_f), spawn $ unwords ["import", "-window", "root", newScreenshotName])
          , ("c - Choose Window/Area", (0, xK_c), spawn $ unwords ["import", newScreenshotName])
          , ("w - Focused Window", (0, xK_w), spawn $ unwords ["import", "-window", "$(xdotool getwindowfocus -f)", newScreenshotName])
          , ("1 - Screen 1", (0, xK_1), spawn $ unwords ["import", "-window", "root", "-crop", "1920x1080+0+0", "+repage", newScreenshotName])
          , ("2 - Screen 2", (0, xK_2), spawn $ unwords ["import", "-window", "root", "-crop", "1920x1080+1920+0", "+repage", newScreenshotName])
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
        (shiftMask, xK_u), submapWithHints myXPConfig
          [
            ("p - Private", (0, xK_p), spawn $ unwords ["upload", "--notify", "-p", filename])
          , ("a - Public", (0, xK_a), spawn $ unwords ["upload", "--notify", filename])
          , ("t - Temporary", (0, xK_t), spawn $ unwords ["upload --notify", "-t", filename])
          ])
    , ("D   delete",
        (shiftMask, xK_d), spawn $ unwords ["rm", filename])
    , ("1   operate on latest screenshot",
        (0, xK_1), submapWithHints myXPConfig $ screenshotMap 1)
    , ("2   operate on second latest screenshot",
        (0, xK_2), submapWithHints myXPConfig $ screenshotMap 2)
    , ("3   operate on third latest screenshot",
        (0, xK_3), submapWithHints myXPConfig $ screenshotMap 3)
    , ("4   operate on fourth latest screenshot",
        (0, xK_4), submapWithHints myXPConfig $ screenshotMap 4)
    , ("5   operate on fifth latest screenshot",
        (0, xK_5), submapWithHints myXPConfig $ screenshotMap 5)
    ]
        where filename = "\"$(nth-last-screenshot " ++ show n ++ ")\""
              nextFilename = "\"$(nextname " ++ filename ++ ")\""
              newScreenshotName = "\"$HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png\""


myLayout = onWorkspace "1" (full ||| fullscreen ||| tiled ||| mtiled) $
           onWorkspace "2" defaultConf $
           onWorkspace "3" defaultConf $
           onWorkspace "4" defaultConf $
           onWorkspace "5" fullscreen $
           onWorkspace "6" defaultConf $
           onWorkspace "7" (focusIndicator $ avoidStruts $ Tall 1 (3/100) (1/4)) $
           onWorkspace "8" defaultConf $
           onWorkspace "9" defaultConf $
           onWorkspace "sys" (focusIndicator $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ avoidStruts $ Grid)
           defaultConf
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled      = focusIndicator $ avoidStruts $ Tall nmaster delta ratio
        mtiled     = focusIndicator $ avoidStruts $ Mirror (Tall 1 (3/100) (1/2))
        full       = avoidStruts $ Full
        fullscreen = Full
        defaultConf = tiled ||| mtiled ||| full

        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 0.6
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

focusIndicatorTheme = def
    { fontName              = "xft:inconsolata:size=10"
    , inactiveBorderColor   = "#2b2b2b"
    , inactiveColor         = "#2b2b2b"
    , inactiveTextColor     = "#2b2b2b"
    , activeBorderColor     = "#3c5a70"
    , activeColor           = "#3c5a70"
    , activeTextColor       = "#3c5a70"
    , urgentBorderColor     = "#ff0000"
    , urgentTextColor       = "#ff0000"
    , decoHeight            = 4
    }

focusIndicator = noFrillsDeco shrinkText focusIndicatorTheme

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
    , title     =? "pinentry"            --> doF W.shiftMaster <+> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , className =? "feh-float"           --> doF W.shiftMaster <+> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "Microsoft Teams Notification" --> placeHook (fixed (1, 1)) <+> doFloat
    , title     =? "Picture-in-Picture"  --> (customFloating $ W.RationalRect 0.65 0.65 0.3 0.3)
    ]

myEventHook = fullscreenEventHook -- <+> evasiveWindowEventHook

myStartupHook = return ()

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
        spawnTerminal  = "urxvt -name scratchpad"
        findTerminal   = resource =? "scratchpad"
        spawnTerminal2 = "urxvt -name scratchpad-2"
        findTerminal2  = resource =? "scratchpad-2"
        spawnTerminal3 = "urxvt -name scratchpad-3"
        findTerminal3  = resource =? "scratchpad-3"
        spawnBotTerm   = "urxvt -name bot-term -class invertedTerm"
        findBotTerm    = resource =? "bot-term"
        spawnMusic     = "urxvt -name music -e 'pulsemixer'"
        findMusic      = resource =? "music"
        -- spawnIrc       = "xterm -name irc -e 'tmux-attach-or-new irc weechat'"
        spawnIrc       = "urxvt -name irc -e ssh -t mephory LANG=en_US.utf8 tmux attach -t irc"
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

opacity = flip setOpacity

centeredRect :: Rational -> Rational -> W.RationalRect
centeredRect w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

floatPlacement = placeHook (withGaps (0, 0, 0, 0) $ fixed (0, 0))

myLogHook homeDir = dynamicLogWithPP $ def {
      ppCurrent         = currentFmt
    , ppVisible         = visibleFmt
    , ppHidden          = onlyIf (not . \x -> x `elem` ["NSP", "sys"]) addPadding
    -- , ppHiddenNoWindows = onlyIf (/= "NSP") $ clickable [dzenColor "#93a1a1" "#002b36" . addPadding . wsNum]
    , ppHiddenNoWindows = \_ -> ""
    , ppUrgent          = urgentFmt
    , ppSep             = "  "
    , ppWsSep           = ""
    , ppLayout          = ("%{u#458588}%{+u} " ++) . (++ " %{-u}") . layoutName
    , ppTitle           = \_ -> ""
    , ppExtras          = [willFloatNextPP floatNextStr, C.showCurrentContextName >>= return . contextNameStr]
    , ppOrder           = \(w:l:t:es) -> [w, l] ++ es ++ [t]
    , ppOutput          = appendFileUtf8 "/tmp/workspace-info" . (++"\n")
    }
    where
        addPadding      = ("  " ++) . (++ "  ")
        currentFmt      = (" %{u#cc241d}%{+u} " ++) . (++ " %{-u} ")
        visibleFmt      = (" %{u#484848}%{+u} " ++) . (++ " %{-u} ")
        urgentFmt       = ("%{B#cc241d}  " ++) . (++ "  %{B-}")
        layoutName s
            | "Mirror Tall" `isInfixOf` s = "\xe003"
            | "Tall" `isInfixOf` s        = "\xe002"
            | "Full" `isInfixOf` s        = "\xe001"
            | "OneBig" `isPrefixOf` s     = "O"
            | otherwise                   = "\xe005"
        floatNextStr s = case s of
            "Next" -> "·"
            ""     -> " "
        contextNameStr s = case s of
            "default" -> Nothing
            x         -> Just x
        onlyIf p f x = if p x then f x else ""
