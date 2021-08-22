-- Imports
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

import qualified Data.Map as M
import Data.Maybe (fromJust)

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))

import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP)

-----------------------------

myFont :: String
myFont = "xft:ttf-dejavu:regular:size=8:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "xfce4-terminal"

myBrowser :: String
myBrowser = "chromium"

myEditor :: String
myEditor = myTerminal ++ " -e vim "

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor = "#191919"

myFocusColor :: String
myFocusColor = "#cccccc"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-----------------------------

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "lxsession"
    spawnOnce "nm-applet &"
    spawnOnce "volumeicon &"
    spawnOnce "trayer --edge top --align right --padding 2 --SetDockType true --SetPartialStrut true --expand true --width 7 --transparent true --tint 0x5f5f5f  --height 18 &"
    spawnOnce "feh --bg-scale ~/Pictures/Bg_Images/drawing.png"
    spawnOnce "feh --restore &"
    spawnOnce "--no-startup-id /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    spawnOnce "--no-startup-id numlockx on"
-----------------------------

-- The layout hook
myLayoutHook = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 2/100

-----------------------------

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "confirm"         --> doFloat
    , className =? "file_progress"   --> doFloat
    , className =? "dialog"          --> doFloat
    , className =? "download"        --> doFloat
    , className =? "error"           --> doFloat
    , className =? "Gimp"            --> doFloat
    , className =? "notification"    --> doFloat
    , className =? "pinentry-gtk-2"  --> doFloat
    , className =? "splash"          --> doFloat
    , className =? "toolbar"         --> doFloat
    , className =? "mpv"             --> doFloat
    ]

-----------------------------
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

-----------------------------
-- main function
main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    xmonad $ ewmh def
        { manageHook = myManageHook <+> manageDocks
        , handleEventHook = docksEventHook <+> fullscreenEventHook
        , modMask = myModMask
        , terminal = myTerminal
        , startupHook = myStartupHook
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormColor
        , focusedBorderColor = myFocusColor
        , layoutHook = myLayoutHook
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = \x -> hPutStrLn xmproc0 x
            , ppCurrent = xmobarColor "#cccccc" "" . wrap "[" "]"
            , ppVisible = xmobarColor "#cccccc" "" . clickable
            , ppHidden = xmobarColor "#cccccc" "" . wrap "*" "" . clickable
            , ppHiddenNoWindows = xmobarColor "#cccccc" "" . clickable
            , ppTitle = xmobarColor "#cccccc" "" -- . shorten 30
            , ppSep = "<fc=#cccccc><fn=1>  ::  </fn></fc>"
            , ppUrgent = xmobarColor "#cccccc" "" . wrap "!" "!"
            , ppExtras = [windowCount]
            , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
            }
        } `additionalKeysP` myKeys

-----------------------------

myKeys :: [(String, X ())]
myKeys = 
        [ ("M-S-c", spawn "xmonad --recompile")
        , ("M-S-r", spawn "xmonad --restart")
        , ("M-S-q", io exitSuccess)
        , ("M-q", kill) 
        , ("M-p", spawn "rofi -modi window,drun,run -show drun -sidebar-mode -lines 10 -location 0 -padding 2 -terminal terminator -width 34 -no-plugins -color-window '#191919, #191919, #cccccc' -color-normal '#191919, #cccccc, #191919, #cccccc, #191919' -color-active '#191919, #cccccc, #191919, #cccccc, #191919' -color-urgent '#191919, #cccccc, #191919, #cccccc, #191919'")
        , ("M-S-<Return>", spawn (myTerminal))
        , ("M-<Print>", spawn "flameshot full -p ~/Pictures/Screenshots/ -d 4000")
        ]
-----------------------------

