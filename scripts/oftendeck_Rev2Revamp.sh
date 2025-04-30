#!/bin/bash

# Directly modify the file to change orientation from 3 to 0
sed -i "s/g:_orientation=3/g:_orientation=0/" "$HOME/.oftendeck/scripts/fimgs.sh"

