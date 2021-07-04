sjasmplus --lst $1.asm
if [ $? -eq 0 ]; then
    fuse $1.sna
fi