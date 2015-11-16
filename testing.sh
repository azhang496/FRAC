#!/bin/bash

NC='\033[0m'
RED='\033[0;31m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'


FRAC_FILES="tests/*.frac"
EXEC="./frac"
TMP_FILE=$(mktemp "temp.XXXXX")
printf "${CYAN}Starting tests...\n\n${NC}"

for input in $FRAC_FILES; do

    c_file = ${input/.frac/.c}

    # compile frac program to temp c file
    $($EXEC $input)
    cat "test-hello_world.c" > TMP_FILE

    # if c test file exists, compare them
    if [ -e "$c_file" ]; then
        diff -wB $c_file $TMP_FILE
        if [ "$?" -ne 0 ]; then
            printf "%-50s ${RED}ERROR\n${NC}" "  - checking $c_file..." 1>&2
            rm -f $TMP_FILE
            exit 1
        fi
    fi

    printf "%-50s ${GREEN}SUCCESS\n${NC}" "  - checking $input..."
done

rm -f $TMP_FILE
exit 0
