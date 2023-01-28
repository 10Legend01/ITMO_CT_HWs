#!/bin/sh

CMD=$1
shift
for arg do
    $CMD -u $arg | diff -u --from-file ${arg}.eta.u - || exit 1
done
