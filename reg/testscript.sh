#!/bin/bash
# My first script

if [ $1 ]
then
    cat ../book_src/testcases/test$1.tig > k1.tig
    sml < a
    echo "====== Ran test$1 ======="
    echo ""
    cat ../book_src/testcases/test$1.tig
    echo ""
    echo "====== Above is code for test$1 ======="
    echo ""
else
    echo "Input a test case number to run"
fi
