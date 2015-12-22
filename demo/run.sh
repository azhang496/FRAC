#!/bin/bash
shopt -s nullglob
arg=$1
exe=${arg/.frac/}
.././frac $arg
make
./$exe
files=( *[0-9]-GIF.bmp )
num=${#files[@]}

# NEED TO CHECK IF CORRECT PACKAGES ARE INSTALLED OR NOT
# IF NOT, JUST LEAVE THEM WITH THE 5 BMP FILES
if [ $num -gt 2 ]; then
  gm convert -adjoin ${files[@]} tmp.gif || { echo 'you must run ./configure in top-level directory before generating GIFs' ; exit 1; }
  gifsicle --delay=50 --loop tmp.gif > $exe.gif || { echo 'you must run ./configure in top-level directory before generating GIFs' ; exit 1; }
  rm tmp.gif ${files[@]}
  open $exe.gif
fi
open *.bmp
exit 0
