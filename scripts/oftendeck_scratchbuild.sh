#!/bin/bash
set -e

echo "=== Updating & upgrading system ==="
sudo apt update
sudo apt upgrade -y
sudo apt autoremove -y

echo "=== Installing packages ==="
sudo apt install -y \
    git \
    emacs-nox \
    hunspell \
    hunspell-en-us \
    syncthing \
    fbterm \
    fbi \
    fim \
    xfonts-terminus \
    cups

echo "=== Cloning GitHub repository ==="
git clone https://github.com/oftenzhan/OftenDeck.git ~/.oftendeck

echo "=== Making custom bin scripts executable ==="
sudo chmod +x ~/.oftendeck/scripts/bin/*

echo "=== Making profile.d scripts executable ==="
sudo chmod +x ~/.oftendeck/scripts/profile.d/*

echo "=== Symlinking custom scripts into /usr/local/bin ==="
sudo ln -sf ~/.oftendeck/scripts/bin/* /usr/local/bin/

echo "=== Symlinking login scripts into /etc/profile.d ==="
sudo ln -sf ~/.oftendeck/scripts/profile.d/* /etc/profile.d/

echo "=== Linking tutorial folder ==="
mkdir -p ~/documents/tutorials/
sudo ln -sf ~/.oftendeck/tutorials/ ~/documents/tutorials/

echo "=== Linking emacs init ==="
mkdir -p ~/.emacs.d/
sudo ln -sf ~/.oftendeck/.emacs.d/init.el ~/.emacs.d/init.el

echo "=== Clone and install image-utils ==="
git clone https://github.com/seamusdemora/RonR-RPi-image-utils.git ~/RonR-RPi-image-utils
sudo install --mode=755 ~/RonR-RPi-image-utils/image-* /usr/local/sbin

echo "=== Generate new SSH Key ==="
ssh-keygen -t ed25519 -C "oftendeck01"

echo "=== Setup Complete ==="
