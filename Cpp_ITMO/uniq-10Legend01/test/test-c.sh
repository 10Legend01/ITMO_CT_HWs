#!/bin/sh

CMD=$1
shift
for arg do
    $CMD -c $arg | diff -u --from-file ${arg}.eta.c - || exit 1
done
