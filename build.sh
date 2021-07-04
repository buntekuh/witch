#!/bin/sh
if [[ $# -ne 1 ]]
then
    file=witch
else
    file=$1
fi

sjasmplus --lst $file.asm
if [ $? -eq 0 ]; then
    fuse $file.sna
fi