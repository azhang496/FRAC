#!/bin/bash
shopt -s nullglob
arg=$1
exe=${arg/.frac/}
./frac $arg
gcc -g -Wall   -c -o $exe.o $exe.c
gcc -g -Wall   -c -o turtle.o turtle.c
gcc -g -lm  $exe.o turtle.o   -o $exe
./$exe
files=( *[0-9]-GIF.bmp )
num=${#files[@]}

if [ $num -gt 1 ]; then
  gm convert -adjoin ${files[@]} tmp.gif || { echo 'you must run ./configure in top-level directory before generating GIFs' ; exit 1; }
  gifsicle --delay=50 --loop tmp.gif > $exe.gif || { echo 'you must run ./configure in top-level directory before generating GIFs' ; exit 1; }
  rm tmp.gif ${files[@]}
  open $arg.gif
fi
open *.bmp
exit 0
