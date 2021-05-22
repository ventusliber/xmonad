-- /home/weiting/.xmonad/xmonad.hs
------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------

-- Base
import System.Exit
import XMonad

-- Data
import Data.Monoid
import qualified Data.Map        as M
import Data.Maybe (fromJust)

--Actions
import XMonad.Actions.MouseResize


-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns


-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

-- Utilities
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

-- StackSet
import qualified XMonad.StackSet as W


------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
-- It's nice to assign values to stuff that you will use more than once
-- in the config. Setting values for things like font, terminal and editor
-- means you only have to change the value here to make changes globally.

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask -- modKey super/windows key

myTerminal :: String
myTerminal = "terminator"

myBrowser :: String
myBrowser = "google-chrome"

myBorderWidth   = 2 -- Width of the window border in pixels.

myNormColor :: String
myNormColor   = "#282c34"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"   -- Border color of focused windows


-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
        spawnOnce "nitrogen --restore &"
        spawnOnce "compton &"

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- My workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool. You need to use UnsafeStdInReader instead
-- of simply StdInReader in xmobar config so you can pass actions to it.

-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkspaces = [" dev ", " chrome ", " chat ", " spotify ", " youtube ", " sys ", " mus ", " vid ", " gfx "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

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

    -- keyboard layout --
    , ((modm .|. shiftMask, xK_u), spawn "setxkbmap -layout us")
    , ((modm .|. shiftMask, xK_z), spawn "setxkbmap -layout us")
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- -- Makes setting the spacingRaw simpler to write. The spacingRaw
-- -- module adds a configurable amount of space around windows.
-- mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
--
-- -- Below is a variation of the above except no borders are applied
-- -- if fewer than two windows. So a single window has no gaps.
-- mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True
--
-- -- Defining a bunch of layouts, many that I don't use.
-- tall     = renamed [Replace "tall"]
--           $ limitWindows 12
--           $ mySpacing 8
--           $ ResizableTall 1 (3/100) (1/2) []
-- magnify  = renamed [Replace "magnify"]
--           $ magnifier
--           $ limitWindows 12
--           $ mySpacing 8
--           $ ResizableTall 1 (3/100) (1/2) []
-- monocle  = renamed [Replace "monocle"]
--           $ limitWindows 20 Full
-- floats   = renamed [Replace "floats"]
--           $ limitWindows 20 simplestFloat
-- grid     = renamed [Replace "grid"]
--           $ limitWindows 12
--           $ mySpacing 8
--           $ mkToggle (single MIRROR)
--           $ Grid (16/10)
-- spirals  = renamed [Replace "spirals"]
--           $ mySpacing 8
--           $ spiral (6/7)
-- threeCol = renamed [Replace "threeCol"]
--           $ limitWindows 7
--           $ mySpacing' 4
--           $ ThreeCol 1 (3/100) (1/2)
-- threeRow = renamed [Replace "threeRow"]
--           $ limitWindows 7
--           $ mySpacing' 4
--           -- Mirror takes a layout and rotates it by 90 degrees.
--           -- So we are applying Mirror to the ThreeCol layout.
--           $ Mirror
--           $ ThreeCol 1 (3/100) (1/2)
--
-- tabs    = renamed [Replace "tabs"]
--           -- I cannot add spacing to this layout because it will
--           -- add spacing between window and tabs which looks bad.
--           $ tabbed shrinkText myTabConfig
--   where
--     myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=11"
--                       , activeColor         = "#292d3e"
--                       , inactiveColor       = "#3e445e"
--                       , activeBorderColor   = "#292d3e"
--                       , inactiveBorderColor = "#292d3e"
--                       , activeTextColor     = "#ffffff"
--                       , inactiveTextColor   = "#d0d0d0"
--                       }
--
-- -- Theme for showWName which prints current workspace when you change workspaces.
-- myShowWNameTheme :: SWNConfig
-- myShowWNameTheme = def
--     { swn_font              = "xft:Sans:bold:size=60"
--     , swn_fade              = 1.0
--     , swn_bgcolor           = "#000000"
--     , swn_color             = "#FFFFFF"
--     }
--
-- -- The layout hook
-- myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
--               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
--             where
--               -- I've commented out the layouts I don't use.
--               myDefaultLayout =     tall
--                                 ||| magnify
--                                 ||| noBorders monocle
--                                 ||| floats
--                                 -- ||| grid
--                                 ||| noBorders tabs
--                                 -- ||| spirals
--                                 -- ||| threeCol
--                                 -- ||| threeRow

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
-- Allows to have several floating scratchpads running different applications.
-- Import Util.NamedScratchpad.  Bind a key to namedScratchpadSpawnAction.
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "mocp" spawnMocp findMocp manageMocp
                ]
  where
    spawnTerm  = myTerminal ++ " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnMocp  = myTerminal ++ " -n mocp 'mocp'"
    findMocp   = resource =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w


-- myScratchPads :: [NamedScratchpad]
-- myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
--                 , NS "mocp" spawnMocp findMocp manageMocp
--                 , NS "calculator" spawnCalc findCalc manageCalc
--                 ]
--   where
--     spawnTerm  = myTerminal ++ " -t scratchpad"
--     findTerm   = title =? "scratchpad"
--     manageTerm = customFloating $ W.RationalRect l t w h
--                where
--                  h = 0.9
--                  w = 0.9
--                  t = 0.95 -h
--                  l = 0.95 -w
--     spawnMocp  = myTerminal ++ " -t mocp -e mocp"
--     findMocp   = title =? "mocp"
--     manageMocp = customFloating $ W.RationalRect l t w h
--                where
--                  h = 0.9
--                  w = 0.9
--                  t = 0.95 -h
--                  l = 0.95 -w
--     spawnCalc  = "qalculate-gtk"
--     findCalc   = className =? "Qalculate-gtk"
--     manageCalc = customFloating $ W.RationalRect l t w h
--                where
--                  h = 0.5
--                  w = 0.4
--                  t = 0.75 -h
--                  l = 0.70 -w


------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace.
-- Forcing programs to a certain workspace with a doShift requires xdotool
-- if you are using clickable workspaces. You need the className or title
-- of the program. Use xprop to get this info.

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out
     -- the full name of my clickable workspaces, which would look like:
     -- doShift "<action xdotool super+8>gfx</action>"
     [ className =? "obs"     --> doShift ( "video.obs" )
     , title =? "firefox"     --> doShift ( "web.browser" )
     , title =? "qutebrowser" --> doShift ( "web.browser" )
     , className =? "mpv"     --> doShift ( "video.movie player" )
     , className =? "vlc"     --> doShift ( "video.movie player" )
     , className =? "Gimp"    --> doShift ( "graphics.gimp")
     , className =? "Gimp"    --> doFloat
     , title =? "Oracle VM VirtualBox Manager"     --> doFloat
     , className =? "VirtualBox Manager" --> doShift  ( "dev.virtualization" )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ] <+> namedScratchpadManageHook myScratchPads

-- myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
-- myManageHook = composeAll
--      -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
--      -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
--      -- I'm doing it this way because otherwise I would have to write out the full
--      -- name of my workspaces and the names would be very long if using clickable workspaces.
--      [ className =? "confirm"         --> doFloat
--      , className =? "file_progress"   --> doFloat
--      , className =? "dialog"          --> doFloat
--      , className =? "download"        --> doFloat
--      , className =? "error"           --> doFloat
--      , className =? "Gimp"            --> doFloat
--      , className =? "notification"    --> doFloat
--      , className =? "pinentry-gtk-2"  --> doFloat
--      , className =? "splash"          --> doFloat
--      , className =? "toolbar"         --> doFloat
--      , title =? "Oracle VM VirtualBox Manager"  --> doFloat
--      , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
--      , className =? "brave-browser"   --> doShift ( myWorkspaces !! 1 )
--      , className =? "qutebrowser"     --> doShift ( myWorkspaces !! 1 )
--      , className =? "mpv"             --> doShift ( myWorkspaces !! 7 )
--      , className =? "Gimp"            --> doShift ( myWorkspaces !! 8 )
--      , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
--      , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
--      , isFullscreen -->  doFullFloat
--      ] <+> namedScratchpadManageHook myScratchPads


------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = fullscreenEventHook

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  -- xmproc0 <- spawnPipe "xmobar -x 0 /home/weiting/.config/xmobar/xmobarrc"
  -- Launching three instances of xmobar on their monitors.
  xmproc0 <- spawnPipe "xmobar -x 0 /home/weiting/.config/xmobar/xmobarrc0"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/weiting/.config/xmobar/xmobarrc1"

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
  xmonad $ ewmh def {
        -- simple stuff
          terminal           = myTerminal,
          focusFollowsMouse  = myFocusFollowsMouse,
          clickJustFocuses   = myClickJustFocuses,
          borderWidth        = myBorderWidth,
          modMask            = myModMask,
          workspaces         = myWorkspaces,
          normalBorderColor  = myNormColor,
          focusedBorderColor = myFocusColor,

        -- key bindings
          keys               = myKeys,
          mouseBindings      = myMouseBindings,

        -- hooks, layouts
          layoutHook         = myLayoutHook,
          manageHook         = myManageHook <+> manageDocks ,
          handleEventHook    = docksEventHook,
          startupHook        = myStartupHook,
          -- logHook            = myLogHook
          logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
                -- the following variables beginning with 'pp' are settings for xmobar.
                { ppOutput = \x -> hPutStrLn xmproc0 x                          -- xmobar on monitor 1
                , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"           -- Current workspace
                , ppVisible = xmobarColor "#98be65" "" . clickable              -- Visible but not current workspace
                , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "" . clickable -- Hidden workspaces
                , ppHiddenNoWindows = xmobarColor "#c792ea" ""  . clickable     -- Hidden workspaces (no windows)
                , ppTitle = xmobarColor "#b3afc2" "" . shorten 60               -- Title of active window
                , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"                    -- Separator character
                , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"            -- Urgent workspace
                , ppExtras  = [windowCount]                                     -- # of windows current workspace
                , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]                    -- order of things in xmobar
                }
      }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
