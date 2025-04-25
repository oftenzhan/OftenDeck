#!/bin/bash

# Only run on TTY2
if [ "$(tty)" = "/dev/tty2" ]; then
    setfont /usr/share/consolefonts/Lat15-TerminusBold20x10.psf.gz
    exec often-navscreen
fi
