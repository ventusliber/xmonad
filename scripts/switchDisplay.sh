#!/bin/bash


# set dual monitors
dual () {
  xrandr --output DP-2-2 --primary --left-of DP-2-1 --output DP-2-1 --auto
  xrandr --output DP-2-1 --primary --left-of DP-2-2 --output DP-2-2 --auto
  echo  "Switch to dual monitor"
}

# set single monitor
single () {
    xrandr --output edp-1 --primary --off
    echo  "Switch to single monitor"
}

case $1 in
  single)
    single
    ;;
  dual)
    dual
    ;;
esac
