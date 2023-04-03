#!/usr/bin/env bash

xrandr --output DP-1 --scale 0.6x0.6 --right-of HDMI-1 --output DP-1 --auto
xrandr --output HDMI-1 --primary --scale 0.6x0.6 --left-of DP-1 --output HDMI-1 --auto
nitrogen --restore &
echo "Set monitor successfully"
