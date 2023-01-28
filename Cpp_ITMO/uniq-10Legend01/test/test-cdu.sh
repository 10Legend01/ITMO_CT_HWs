#!/bin/sh

CMD=$1
shift
for arg do
    $CMD -cdu $arg | diff -u --from-file ${arg}.eta.cdu - || exit 1
done
