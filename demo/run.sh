#!/bin/bash
shopt -s nullglob
arg=$1
exe=${arg/.frac/}
.././frac $arg
make
./$exe
files=( *[0-9].bmp )
num=${#files[@]}

# NEED TO CHECK IF CORRECT PACKAGES ARE INSTALLED OR NOT
# IF NOT, JUST LEAVE THEM WITH THE 5 BMP FILES
if [ $num -gt 2 ]; then
  gm convert -adjoin ${files[@]} tmp.gif
  gifsicle --delay=50 --loop tmp.gif > $exe.gif
  rm tmp.gif ${files[@]}
  open $exe.gif
fi
open *.bmp
exit 0
