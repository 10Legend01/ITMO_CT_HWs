#!/bin/sh

CMD=$1
shift
for arg do
    $CMD -d $arg | diff -u --from-file ${arg}.eta._d - || exit 1
done
