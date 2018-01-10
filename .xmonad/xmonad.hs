import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.NoBorders
import qualified XMonad.Actions.Search as S
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FloatNext
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Util.NamedScratchpad
import XMonad.Prompt
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromJust, fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit
import System.Environment
import Graphics.X11.ExtraTypes.XF86

import Passwords (passwordPrompt, genPasswordPrompt)
import UnicodeUtils (writeFileUtf8)
import FullscreenToggle (toggleFullscreen)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myXPConfig = def
    { bgColor     = "#002b36"
    , fgColor     = "#fdf6e3"
    , borderColor = "#fdf6e3"
    , fgHLight    = "#002b36"
    , bgHLight    = "#fdf6e3"
    , font        = "xft:monaco:size=10"
    , height      = 22
    }

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"] ++ ["NSP"]
myBrowser    = "qutebrowser"

-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm .|. shiftMask, xK_r     ), refresh)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)
    -- Float next window
    , ((modm .|. shiftMask, xK_f     ), toggleFloatNext >> runLogHook)
    -- Toggle struts
    -- , ((modm              , xK_o     ), sendMessage ToggleStruts)
    -- Toggle border
    , ((modm              , xK_u     ), withFocused toggleBorder)
    -- Lock screen
    , ((modm              , xK_Escape), spawn "slock")
    -- Click on a window to kill
    , ((modm .|. shiftMask, xK_x     ), spawn "xkill")
    -- Toggle between two most recently viewed workspaces
    , ((modm              , xK_grave ), toggleWS' ["NSP"])
    -- Toggle fullscreen for focused window
    , ((modm              , xK_o     ), withFocused toggleFullscreen)

    -- Prompts
    , ((modm              , xK_x     ), passwordPrompt myXPConfig)
    , ((modm              , xK_n     ), genPasswordPrompt myXPConfig)

    -- Various
    , ((modm              , xK_s     ), submap $ searchMap (S.promptSearchBrowser myXPConfig myBrowser))
    , ((modm .|. shiftMask, xK_s     ), submap $ searchMap (S.selectSearchBrowser myBrowser))

    -- Screenshots
    , ((0                 , xK_Print ), spawn "import -window root $HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png")
    , ((shiftMask         , xK_Print ), spawn "import $HOME/data/screenshots/screenshot-$(date +'%Y-%m-%d--%H-%M-%S').png")
    , ((modm              , xK_0     ), spawn "upload-screenshot -window root")
    , ((modm .|. shiftMask, xK_0     ), spawn "upload-screenshot")
    , ((modm .|. shiftMask, xK_v     ), spawn "screenshot-google-image-search")

    -- Scratchpads
    , ((modm              , xK_v     ), namedScratchpadAction myScratchpads "terminal")
    , ((modm              , xK_c     ), namedScratchpadAction myScratchpads "terminal-2")
    , ((modm              , xK_z     ), namedScratchpadAction myScratchpads "music")
    , ((modm              , xK_a     ), namedScratchpadAction myScratchpads "htop")
    , ((modm              , xK_b     ), namedScratchpadAction myScratchpads "irc")
    , ((modm .|. shiftMask, xK_p     ), namedScratchpadAction myScratchpads "color")


    -- Media Keys
    , ((0, xF86XK_AudioMute), spawn "amixer sset Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 2dB-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 2dB+")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    , ((0, xF86XK_Display), spawn "xbacklight -set 85")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
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
    ]

-- Search bindings
searchMap method = M.fromList
    [ ((0, xK_g), method S.google)
    , ((0, xK_i), method S.images)
    , ((0, xK_w), method S.wikipedia)
    , ((0, xK_y), method S.youtube)
    , ((0, xK_h), method S.hoogle)
    , ((0, xK_s), method S.multi)
    , ((0, xK_d), method $ S.searchEngine "dict" "http://dict.cc/")
    , ((0, xK_a), method $ S.searchEngine "amazon" "http://www.amazon.de/s/?field-keywords=")
    ]


-- Layouts:
myLayout = onWorkspace "1" (full ||| fullscreen ||| tiled ||| mtiled) $
           onWorkspace "2" (devLayout ||| tiled ||| mtiled ||| full) $
           onWorkspace "3" defaultConf $
           onWorkspace "4" defaultConf $
           onWorkspace "5" fullscreen $
           onWorkspace "6" (fullscreen ||| full ||| tiled ||| mtiled) $
           onWorkspace "7" (smartBorders $ avoidStruts $ Tall 1 (3/100) (1/4)) $
           onWorkspace "8" (tiled ||| full) $
           onWorkspace "9" (simpleFloat ||| tiled ||| full)
           mtiled
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled      = smartBorders $ avoidStruts $ Tall nmaster delta ratio
        mtiled     = smartBorders $ avoidStruts $ Mirror (Tall 1 (3/100) (1/2))
        devLayout  = smartBorders $ avoidStruts $ OneBig (3/4) (4/5)
        full       = smartBorders $ avoidStruts Full
        fullscreen = noBorders Full
        defaultConf = tiled ||| mtiled ||| full

        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

-- Window rules:
myManageHook = composeAll
    [ resource  =? "desktop_window"      --> doIgnore
    , resource  =? "kdesktop"            --> doIgnore
    , className =? "Firefox"             --> doShift "1"
    , className =? "qutebrowser"         --> doShift "1"
    , className =? "explorer.exe"        --> doShift "9"
    , title     =? "Wine System Tray"    --> doShift "9"
    , className =? "dota2"               --> doShift "5" <+> (doF . W.sink =<< ask)
    , title     =? "vselect"             --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "vselect-record-area" --> placeHook (fixed (0, 0)) <+> doFloat
    , title     =? "pinentry"            --> doF W.shiftMaster <+> placeHook (fixed (0.5, 0.5)) <+> doFloat
    ]

-- Event handling
myEventHook = fullscreenEventHook

-- Status bars and logging
myLogHook homeDir = dynamicLogWithPP $ def {
      ppCurrent         = clickable [dzenColor "#fffffd" "#268bd2" . addPadding . wsNum]
    , ppVisible         = clickable [dzenColor "#fffffd" "#8a8a8a" . addPadding . wsNum]
    , ppHidden          = onlyIf (/= "NSP") $ clickable [dzenColor "#586e75" "#eee8d5" . addPadding . wsNum]
    , ppHiddenNoWindows = onlyIf (/= "NSP") $ clickable [dzenColor "#93a1a1" "#eee8d5" . addPadding . wsNum]
    , ppUrgent          = clickable [dzenColor "red"     "#212121" . addPadding . wsNum]
    , ppSep             = "^fg(#e0e0bb)^r(1x16)^fg(#efefef)^r(1x16)^bg()^fg()"
    , ppWsSep           = ""
    , ppLayout          = dzenColor "#657b83" "#eee8d5" . addPadding . layoutName
    , ppTitle           = dzenColor "#657b83" "#eee8d5" . addPadding
    , ppExtras          = [willFloatNextPP (addPadding . floatNextStr)]
    , ppOrder           = \(w:l:t:es) -> [w, l] ++ es ++ [t]
    , ppOutput          = writeFileUtf8 "/tmp/.workspace-info" . (++"\n")
    }
    where
        -- general transformations
        addPadding      = ("    " ++) . (++ "    ")

        -- workspace name transformations
        wsIcon wid        = "^i(" ++ homeDir ++ "/.workspace-icons/" ++ wid ++ ".xbm)"
        wsUnicodeIcon wid = fromMaybe "N" (M.lookup wid workspaceIcons)
        wsNum wid       = show . (+1) . fromJust $ elemIndex wid myWorkspaces

        -- layout name transformation
        layoutName s
            | s == "Tall"             = "◧"
            | s == "Mirror Tall"      = "⬒"
            | s == "Full"             = "□"
            | "OneBig" `isPrefixOf` s = "◰"
            | otherwise               = "▦"

        -- other transformations
        floatNextStr _ = "◈"

        -- helpers
        dzenStartCa wid = "^ca(1, xdotool key 'alt+" ++ wsNum wid ++ "')"
        dzenEndCa _         = "^ca()"
        clickable fs = applyAll ([dzenStartCa] ++ fs ++ [dzenEndCa])
        applyAll fs x   = fs >>= ($ x)
        onlyIf p f x    = if p x then f x else ""
        workspaceIcons = M.fromList
              [ ("web"   , "☀")
              , ("dev"   , "♛")
              , ("music" , "♬")
              , ("term"  , "⌚")
              , ("game"  , "♞")
              , ("vm"    , "♟")
              , ("im"    , "✉")
              , ("other" , "ℹ")
              , ("float" , "💾")
                ]

-- Startup hook
myStartupHook = return ()

-- Scratchpads
myScratchpads = [ NS "terminal"   spawnTerminal  findTerminal  manageSP
                , NS "terminal-2" spawnTerminal2 findTerminal2 manageSP
                , NS "music"      spawnMusic     findMusic     manageSP
                , NS "htop"       spawnHtop      findHtop      manageSP
                , NS "irc"        spawnIrc       findIrc       manageIrcSP
                , NS "color"      spawnColor     findColor     manageColorSP
                ]
    where
        spawnTerminal  = "xterm -name scratchpad"
        findTerminal   = resource =? "scratchpad"
        spawnTerminal2 = "xterm -name scratchpad-2"
        findTerminal2  = resource =? "scratchpad-2"
        spawnMusic     = "xterm -name music -e 'tmux-attach-or-new music ncmpcpp pulsemixer'"
        findMusic      = resource =? "music"
        spawnHtop      = "xterm -name htop -e htop"
        findHtop       = resource =? "htop"
        -- spawnIrc       = "xterm -name irc -e 'tmux-attach-or-new irc weechat'"
        spawnIrc       = "xterm -name irc -e 'ssh -t mephory LANG=en_US.utf8 tmux attach -t irc'"
        findIrc        = resource =? "irc"
        spawnColor     = "gcolor2"
        findColor      = resource =? "gcolor2"
        manageSP = customFloating $ centeredRect 0.5 0.5
        manageIrcSP = customFloating $ centeredRect 0.6 0.6
        manageColorSP = placeHook (fixed (0.5, 0.5)) <+> doFloat

centeredRect :: Rational -> Rational -> W.RationalRect
centeredRect w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

floatPlacement = placeHook (withGaps (20, 0, 0, 0) $ fixed (0, 0))

------------------------------------------------------------------------
main = do
    homeDir <- getEnv "HOME"
    xmonad $ ewmh $ withUrgencyHook NoUrgencyHook def {
      -- simple stuff
        terminal           = "xterm"
      , focusFollowsMouse  = True
      , borderWidth        = 1
      , modMask            = mod1Mask
      , workspaces         = myWorkspaces
      , normalBorderColor  = "#666666"
      , focusedBorderColor = "#dddddd"

      -- key bindings
      , keys               = myKeys
      , mouseBindings      = myMouseBindings

      -- hooks, layouts
      , layoutHook         = myLayout
      , manageHook         = myManageHook
                             <+> namedScratchpadManageHook myScratchpads
                             <+> floatPlacement
                             <+> floatNextHook
                             <+> insertPosition Below Newer
      , handleEventHook    = myEventHook
      , logHook            = myLogHook homeDir
      , startupHook        = myStartupHook
    }
