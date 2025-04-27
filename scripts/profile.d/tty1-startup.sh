#!/bin/bash

# Only run this if we're on tty1
if [[ "$(tty)" == "/dev/tty1" ]]; then
  exec fbterm
  emacs
fi
