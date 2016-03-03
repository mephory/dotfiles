import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.NoBorders
import qualified XMonad.Actions.Search as S
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FloatNext
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Place
import XMonad.Util.NamedScratchpad
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Input
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromJust, fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit
import System.Environment
import Graphics.X11.ExtraTypes.XF86

import Passwords (passwordPrompt, genPasswordPrompt)
import UnicodeUtils (writeFileUtf8)

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

myWorkspaces    = ["web","dev","music","term","game","vm","im","other","float"] ++ ["NSP"]
myBrowser    = "firefox"

--- Twitch Prompt
twitchChannels = ["stevicules", "amazhs", "rsgloryandgold"]
twitchPrompt = inputPromptWithCompl myXPConfig "Twitch Channel"
                    (mkComplFunFromList twitchChannels) ?+ watchTwitchChannel
    where watchTwitchChannel x = spawn $ "mpv http://twitch.tv/" ++ x

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
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_f     ), toggleFloatNext >> runLogHook)
    -- Toggle struts
    , ((modm              , xK_o     ), sendMessage ToggleStruts)
    -- Toggle border
    , ((modm              , xK_u     ), withFocused toggleBorder)

    -- Prompts
    , ((modm .|. shiftMask, xK_grave ), sshPrompt myXPConfig)
    , ((modm              , xK_x     ), passwordPrompt myXPConfig)
    , ((modm              , xK_c     ), genPasswordPrompt myXPConfig)

    -- Custom
    , ((modm              , xK_Escape), spawn "slock")
    , ((modm .|. shiftMask, xK_x     ), spawn "xkill")
    , ((modm .|. shiftMask, xK_t     ), twitchPrompt)
    , ((modm              , xK_grave ), toggleWS' ["NSP"])
    , ((modm              , xK_s     ), submap $ searchMap (S.promptSearchBrowser myXPConfig myBrowser))
    , ((modm .|. shiftMask, xK_s     ), submap $ searchMap (S.selectSearchBrowser myBrowser))
    , ((modm              , xK_0     ), spawn "upload-screenshot -window root")
    , ((modm .|. shiftMask, xK_0     ), spawn "upload-screenshot")
    , ((modm .|. shiftMask, xK_v     ), spawn "screenshot-google-image-search")

    -- Scratchpads
    , ((modm              , xK_v     ), namedScratchpadAction myScratchpads "terminal")
    , ((modm              , xK_z     ), namedScratchpadAction myScratchpads "music")
    , ((modm              , xK_a     ), namedScratchpadAction myScratchpads "htop")
    , ((modm              , xK_b     ), namedScratchpadAction myScratchpads "irc")
    , ((modm .|. shiftMask, xK_p     ), namedScratchpadAction myScratchpads "color")

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
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


runIfNotIgnored action w = runQuery mouseIgnore w >>= \b -> if b then mempty else action w
    where mouseIgnore = className =? "dota2"

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), runIfNotIgnored (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), runIfNotIgnored (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), runIfNotIgnored (\w -> focus w >> mouseResizeWindow w))
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
    , ((0, xK_p), method $ S.searchEngine "proxer" "http://proxer.me/search?name=")
    , ((0, xK_m), method $ S.searchEngine "myanimelist" "http://myanimelist.net/anime.php?q=")
    ]


-- Layouts:
myLayout = onWorkspace "web"   (full ||| fullscreen ||| tiled ||| mtiled) $
           onWorkspace "dev"   (devLayout ||| tiled ||| mtiled ||| full) $
           onWorkspace "music" defaultConf $
           onWorkspace "term"  defaultConf $
           onWorkspace "game"  fullscreen $
           onWorkspace "vm"    (fullscreen ||| full ||| tiled ||| mtiled) $
           onWorkspace "im"    defaultConf $
           onWorkspace "other" (tiled ||| full) $
           onWorkspace "float" (simpleFloat ||| tiled ||| full)
           mtiled
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled      = avoidStruts $ Tall nmaster delta ratio
        mtiled     = avoidStruts $ Mirror (Tall 1 (3/100) (1/2))
        devLayout  = avoidStruts $ OneBig (3/4) (4/5)
        full       = avoidStruts Full
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
    [ className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore 
    , className =? "Firefox"          --> doShift "web"
    , className =? "Pidgin"           --> doShift "im"
    , className =? "explorer.exe"     --> doShift "float"
    , title     =? "Wine System Tray" --> doShift "float"
    , title     =? "vselect"          --> placeHook (fixed (0.5, 0.5)) <+> doFloat
    , title     =? "dztemp"           --> doShift "game" <+> placeHook dztempPosition <+> doFloat
    -- , className =? "mpv"              --> doFloat
    ]
    where dztempPosition = withGaps (0, 1, 1, 0) (fixed (1, 1))

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
myScratchpads = [ NS "terminal" spawnTerminal findTerminal manageSP
                , NS "music"    spawnMusic    findMusic    manageSP
                , NS "htop"     spawnHtop     findHtop     manageSP
                , NS "irc"      spawnIrc      findIrc      manageIrcSP
                , NS "color"    spawnColor    findColor    manageColorSP
                ]
    where
        spawnTerminal = "xterm -name scratchpad"
        findTerminal  = resource =? "scratchpad"
        spawnMusic    = "xterm -name music -e 'tmux-attach-or-new music ncmpcpp pulsemixer'"
        findMusic     = resource =? "music"
        spawnHtop     = "xterm -name htop -e htop"
        findHtop      = resource =? "htop"
        spawnIrc      = "xterm -name irc -e 'tmux-attach-or-new irc weechat'"
        findIrc       = resource =? "irc"
        spawnColor    = "gcolor2"
        findColor     = resource =? "gcolor2"
        manageSP = customFloating $ centeredRect 0.5 0.5
        manageIrcSP = customFloating $ centeredRect 0.6 0.6
        manageColorSP = placeHook (fixed (0.5, 0.5)) <+> doFloat

centeredRect :: Rational -> Rational -> W.RationalRect
centeredRect w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

floatPlacement = placeHook (withGaps (20, 0, 0, 0) $ fixed (0, 0))

------------------------------------------------------------------------
main = do
    homeDir <- getEnv "HOME"
    xmonad $ withUrgencyHook NoUrgencyHook def {
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
