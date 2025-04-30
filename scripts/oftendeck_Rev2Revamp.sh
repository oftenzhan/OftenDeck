#!/bin/bash

echo "=== Modify fim rotation ==="
sed -i "s/g:_orientation=3/g:_orientation=0/" "$HOME/.oftendeck/scripts/fimgs.sh"

echo "=== Modify fbterm rotation ==="
sed -i "s/^screen-rotate=.*/screen-rotate=0/" "$HOME/.fbtermrc"