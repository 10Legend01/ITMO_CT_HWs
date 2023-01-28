#!/bin/sh

CMD=$1
shift
for arg do
    $CMD -du $arg | diff -u --from-file ${arg}.eta.du - || exit 1
done
