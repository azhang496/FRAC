#!/bin/bash

NC='\033[0m'
CYAN='\033[0;36m'
RED='\033[0;31m'
GREEN='\033[0;32m'

PASS_FILES="tests/pass/*.frac"
FAIL_FILES="tests/fail/*.frac"
EXEC="./frac"
C_EXEC="./a.out"

printf "${CYAN}Starting tests...\n\n"

printf "${CYAN}Tests that should pass:\n${NC}"

for input in $PASS_FILES; do

    c_file=${input/.frac/.c}
    output=${input/.frac/.txt}
    name=${input:11}
    tmp=${name/.frac/.c}
    $EXEC $input
    if [ -e "$c_file" ]; then
        diff -wB $c_file $tmp
        if [ "$?" -ne 0 ]; then
            printf "%-60s ${RED}ERROR\n${NC}" "checking contents of $c_file..." 1>&2
            exit 1
        fi
    fi

    if [ -e "$output" ]; then
        gcc -g -Wall $tmp
        $C_EXEC > $tmp
        diff -wB $output $tmp
        if [ "$?" -ne 0 ]; then
            printf "%-60s ${RED}ERROR\n${NC}" "checking output of $output..." 1>&2
            rm -rf a.out.dSYM a.out
            exit 1
        fi
    fi

    rm -f $tmp
    printf "%-60s ${GREEN}SUCCESS\n${NC}" "checking $input..."

done

printf "\n${CYAN}Tests that should fail:\n${NC}"


for input in $FAIL_FILES; do

    output=${input/.frac/.txt}
    error="$($EXEC $input 2>&1)"

    if [ -e "$output" ]; then
        diff -u <(cat "$output") <(echo "$error")
        if [ "$?" -ne 0 ]; then
            printf "%-60s ${RED}DIDN'T FAIL\n${NC}" "checking output of $output..." 1>&2
            exit 1
        fi
    fi

    rm -f $tmp
    printf "%-60s ${GREEN}FAILED!\n${NC}" "checking $input..."

done

rm -rf a.out.dSYM a.out .DS_Store $tmp error
exit 0
