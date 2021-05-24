-- /hoem/weiting/.xmonad/xmonad.hs
------------------------------------------------------------------------
-- IMPORTS
------------------------------------------------------------------------

-- Base
import System.Exit
import XMonad

-- Data
import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.Map        as M
import Data.Maybe (isJust)

--Actions
import XMonad.Actions.MouseResize
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
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
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Shell

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

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
myWorkspaces = [" web ", " atom ", " chat ", " spotify ", " youtube ", " I ", " II ", " III ", " VI "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------

myKeys :: [(String, X ())]
myKeys =
  -- Workspaces
    [ ("M-.", nextScreen)  -- Switch focus to next monitor
    , ("M-,", prevScreen)  -- Switch focus to prev monitor
    , ("M-S-<Right>", shiftTo Next nonNSP >> moveTo Next nonNSP) -- Shifts focused window to next ws
    , ("M-S-<Left>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws
  -- Media
    , ("M-<Page_Down>", spawn "amixer set Master 5%- unmute") -- volume down
    , ("M-<Page_Up>", spawn "amixer set Master 5%+ unmute") -- volume up
    , ("M-<Print>", spawn "amixer -D pulse set Master 1+ toggle") --- mute or unmute

    , ("M-<Insert>", spawn "playerctl play-pause")
    , ("M-<Delete>", spawn "playerctl previous")
    , ("M-<End>", spawn "playerctl next")
    ]
  -- The following lines are needed for named scratchpads.
        where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
              nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

-- myRemoveKey :: [String] myRemoveKey = [("C-S-e")]
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------
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
          mouseBindings      = myMouseBindings,

        -- hooks, layouts
          layoutHook         = myLayoutHook,
          -- manageHook         = myManageHook <+> manageDocks ,
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
      } `additionalKeysP` myKeys  --`removeKeysP` myRemoveKey

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
