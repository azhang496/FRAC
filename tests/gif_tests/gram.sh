#!/bin/bash
shopt -s nullglob
arg=$1
./$arg
files=( *.bmp )
num=${#files[@]}
if [ $num -gt 1 ]; then
  gm convert -adjoin ${files[@]} tmp.gif
  gifsicle --delay=50 --loop tmp.gif > $arg.gif
fi
open $arg.gif
rm tmp.gif
exit 0
