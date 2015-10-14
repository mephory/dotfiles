import XMonad
import XMonad.Actions.CycleWS
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Circle
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FloatNext
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Place
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.Loggers
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Email
import XMonad.Prompt.Ssh
import XMonad.Prompt.RunOrRaise
import Data.Monoid
import Data.Ratio ((%))
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit
import System.Environment
import System.FilePath ((</>))
import System.FilePath.Posix (takeBaseName)
import System.Environment (getEnv)
import System.Directory (getDirectoryContents)
import System.IO
import Graphics.X11.ExtraTypes.XF86

import Passwords (passwordPrompt, genPasswordPrompt)
import UnicodeUtils (writeFileUtf8)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

homeDir = unsafePerformIO $ getEnv "HOME"

myWorkspaces    = ["web","dev","music","term","game","vm","im","other","float"]

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
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    -- , ((modm,               xK_Tab   ), windows W.focusDown)
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
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm              , xK_o     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_f     ), toggleFloatNext >> runLogHook)

    -- Prompts
    , ((modm              , xK_grave ), sshPrompt defaultXPConfig)
    , ((modm              , xK_x     ), passwordPrompt defaultXPConfig)
    , ((modm              , xK_c     ), genPasswordPrompt defaultXPConfig)

    -- Custom
    , ((modm .|. shiftMask, xK_s     ), spawn "slock")
    , ((modm .|. shiftMask, xK_p     ), spawn "gcolor2")
    , ((modm .|. shiftMask, xK_x     ), spawn "xkill")
    , ((modm .|. shiftMask, xK_t    ), spawn "mpv http://twitch.tv/stevicules")

    -- Thinkpad Function Keys
    , ((0, xF86XK_AudioMute), spawn "amixer sset Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 2dB-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 2dB+")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    , ((0, xF86XK_Display), spawn "xbacklight -set 85")
    , ((0, xF86XK_Tools), spawn "notify-send -t 2000 Temperature \"$(acpi -t)\"")
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
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w))
    -- close focused window by pressing all mouse buttons
    , ((button1Mask .|. button2Mask, button3), (\_ -> kill))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Layouts:
myLayout = onWorkspace "web"   (full ||| fullscreen ||| tiled ||| mtiled) $
           onWorkspace "dev"   (devLayout ||| tiled ||| mtiled ||| full) $
           onWorkspace "music" defaultConf $
           onWorkspace "term"  defaultConf $
           onWorkspace "game"  (fullscreen ||| tiled ||| mtiled ||| full) $
           onWorkspace "vm"    (fullscreen ||| full ||| tiled ||| mtiled) $
           onWorkspace "im"    defaultConf $
           onWorkspace "other" (tiled ||| full) $
           onWorkspace "float" (simpleFloat ||| tiled ||| full) $
           mtiled
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled      = avoidStruts $ Tall nmaster delta ratio
        mtiled     = avoidStruts $ Mirror (Tall 1 (3/100) (1/2))
        devLayout  = avoidStruts $ OneBig (3/4) (4/5)
        full       = avoidStruts $ Full
        fullscreen = noBorders $ Full
        defaultConf = tiled ||| mtiled ||| full

        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

-- Window rules:
myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , className =? "mpv"              --> doFloat
    , className =? "Gimp"             --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore 
    , className =? "Firefox"          --> doShift "web"
    , className =? "Pidgin"           --> doShift "im"
    , className =? "explorer.exe"     --> doShift "float"
    , title     =? "Wine System Tray" --> doShift "float"
    , title     =? "gcolor2"          --> placeHook (fixed (0.5, 0.5))
    ]

-- Event handling
myEventHook = mempty

-- Status bars and logging
myLogHook = dynamicLogWithPP $ defaultPP {
      ppCurrent         = clickable [dzenColor "#fffffd" "#268bd2" . addPadding . wsNum]
    , ppVisible         = clickable [dzenColor "#fffffd" "#8a8a8a" . addPadding . wsNum]
    , ppHidden          = clickable [dzenColor "#586e75" "#eee8d5" . addPadding . wsNum]
    , ppHiddenNoWindows = clickable [dzenColor "#93a1a1" "#eee8d5" . addPadding . wsNum]
    , ppUrgent          = clickable [dzenColor "red"     "#212121" . addPadding . wsNum]
    , ppSep             = "^fg(#e0e0bb)^r(1x16)^fg(#efefef)^r(1x16)^bg()^fg()"
    , ppWsSep           = ""
    , ppLayout          = dzenColor "#657b83" "#eee8d5" . addPadding . layoutName
    , ppTitle           = dzenColor "#657b83" "#eee8d5" . addPadding
    , ppExtras          = [willFloatNextPP (addPadding . floatNextStr)]
    , ppOrder           = \(w:l:t:es) -> [w, l] ++ es ++ [t]
    , ppOutput          = (writeFileUtf8 $ "/tmp/.workspace-info") . (++"\n")
    }
    where
        -- general transformations
        addPadding      = ("    " ++) . (++ "    ")

        -- workspace name transformations
        wsIcon wid        = "^i(" ++ homeDir ++ "/.workspace-icons/" ++ wid ++ ".xbm)"
        wsUnicodeIcon wid = maybe "N" id (M.lookup wid workspaceIcons)
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
        clickable fs x  = applyAll ([dzenStartCa] ++ fs ++ [dzenEndCa]) x
        applyAll fs x   = fs >>= ($ x)
        workspaceIcons = M.fromList $
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


floatPlacement = placeHook (withGaps (20, 0, 0, 0) $ fixed (0, 0))

------------------------------------------------------------------------
main = do xmonad $ withUrgencyHook NoUrgencyHook defaultConfig {
      -- simple stuff
        terminal           = "xterm",
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod1Mask,
        workspaces         = myWorkspaces,
        normalBorderColor  = "#666666",
        focusedBorderColor = "#dddddd",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook
                             <+> floatPlacement
                             <+> floatNextHook
                             <+> insertPosition Below Newer,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
