#!/bin/bash
shopt -s nullglob
arg=$1
exe=${arg/.frac/}

# Compile frac program into c
./frac $arg

# Make the c file into executable
gcc -g -Wall -Ilib/ -c -o $exe.o $exe.c
gcc -g -Wall -c -o lib/turtle.o lib/turtle.c
gcc -g -lm lib/turtle.o $exe.o -o $exe || { echo 'C' ; exit 1; }
./$exe

# If the output is a GIF
files=( *[0-9]-GIF.bmp )
num=${#files[@]}
if [ $num -gt 1 ]; then
  gm convert -adjoin ${files[@]} tmp.gif || { echo 'you must run ./configure in top-level directory before generating GIFs' ; exit 1; }
  gifsicle --delay=50 --loop tmp.gif > $exe.gif || { echo 'you must run ./configure in top-level directory before generating GIFs' ; exit 1; }
  rm tmp.gif ${files[@]}
fi
exit 0
