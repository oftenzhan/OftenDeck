#!/bin/bash

echo "=== Modify fim rotation ==="

sed -i "s/g:_orientation=.*/g:_orientation=3/" "$HOME/.oftendeck/scripts/bin/often-fimgs"

echo "=== Modify fbterm rotation ==="

sed -i "s/^screen-rotate=.*/screen-rotate=0/" "$HOME/.fbtermrc"

echo "=== Appending to /boot/firmware/cmdline.txt ==="

echo "$(cat /boot/firmware/cmdline.txt) loglevel=1 quiet logo.nologo" | sudo tee /boot/firmware/cmdline.txt > /dev/null

echo "=== Appending to /boot/firmware/config.txt ==="

echo -e "\n# Disable rainbow splash screen\ndisable_splash=1" | sudo tee -a /boot/firmware/config.txt > /dev/null

echo -e "\n# Remove boot delay (less chance to enter recovery)\nboot_delay=0" | sudo tee -a /boot/firmware/config.txt > /dev/null

echo "Done. Reboot to see the changes."