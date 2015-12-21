#!/bin/bash
shopt -s nullglob
arg=$1
./$arg
files=( *.bmp )
num=${#files[@]}

# NEED TO CHECK IF CORRECT PACKAGES ARE INSTALLED OR NOT
# IF NOT, JUST LEAVE THEM WITH THE 5 BMP FILES
if [ $num -gt 1 ]; then
  gm convert -adjoin ${files[@]} tmp.gif
  gifsicle --delay=50 --loop tmp.gif > $arg.gif
  rm tmp.gif *.bmp
fi
open $arg.gif
exit 0
