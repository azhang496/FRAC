#!/bin/bash
arg=$1
c=${arg/.frac/-NEW.c}
gcc="tests/"$c
./frac $1
gcc $gcc
./a.out
rm -rf a.out
exit 0
