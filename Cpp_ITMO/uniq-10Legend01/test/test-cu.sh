#!/bin/sh

CMD=$1
shift
for arg do
    $CMD -cu $arg | diff -u --from-file ${arg}.eta.cu - || exit 1
done
