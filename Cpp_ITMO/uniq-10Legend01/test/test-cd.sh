#!/bin/sh

CMD=$1
shift
for arg do
    $CMD -cd $arg | diff -u --from-file ${arg}.eta.cd - || exit 1
done
