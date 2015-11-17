#!/bin/bash

NC='\033[0m'
CYAN='\033[0;36m'
RED='\033[0;31m'
GREEN='\033[0;32m'

TEST_FILES="tests/*.frac"
EXEC="./frac"
C_EXEC="./a.out"

printf "${CYAN}Starting tests...\n\n${NC}"

for input in $TEST_FILES; do

    c_file=${input/.frac/.c}
    output=${input/.frac/.txt}
    TEMP=${input/.frac/-NEW.c}
    $EXEC $input

    if [ -e "$c_file" ]; then
        diff -wB $c_file $TEMP
        if [ "$?" -ne 0 ]; then
            printf "%-50s ${RED}ERROR\n${NC}" "checking contents of $c_file..." 1>&2
            exit 1
        fi
    fi

    if [ -e "$output" ]; then
        gcc -g -Wall $TEMP
        $C_EXEC > $TEMP
        diff -wB $output $TEMP
        if [ "$?" -ne 0 ]; then
            printf "%-50s ${RED}ERROR\n${NC}" "checking output of $output..." 1>&2
            rm -rf a.out.dSYM a.out
            exit 1
        fi
    fi

    rm -f $TEMP
    printf "%-50s ${GREEN}SUCCESS\n${NC}" "checking $input..."

done

rm -rf a.out.dSYM a.out $TEMP
exit 0
