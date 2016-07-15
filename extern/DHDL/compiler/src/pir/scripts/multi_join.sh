#!/bin/sh

# multijoin - join multiple files

join_rec() {
    if [ $# -eq 1 ]; then
        join -t, - "$1"
    else
        f=$1; shift
        join -t, - "$f" | join_rec "$@"
    fi
}

if [ $# -le 2 ]; then
    join -t, "$@"
else
    f1=$1; f2=$2; shift 2
    join -t, "$f1" "$f2" | join_rec "$@"
fi
