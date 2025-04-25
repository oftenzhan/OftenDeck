#!/bin/bash
set -e

echo "=== Updating and upgrading system ==="
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

echo "=== Making custom bin scripts executable ==="
sudo chmod +x /home/oftendeck/.oftenconfig/scripts/bin/*

echo "=== Making profile.d scripts executable ==="
sudo chmod +x /home/oftendeck/.oftenconfig/scripts/profile.d/*

echo "=== Symlinking custom scripts into /usr/local/bin ==="
sudo ln -sf /home/oftendeck/.oftenconfig/scripts/bin/* /usr/local/bin/

echo "=== Symlinking login scripts into /etc/profile.d ==="
sudo ln -sf /home/oftendeck/.oftenconfig/scripts/profile.d/* /etc/profile.d/

echo "=== Linking tutorial folder ==="
mkdir -p /home/oftendeck/documents
sudo ln -sf /home/oftendeck/.oftenconfig/tutorial/ /home/oftendeck/documents/tutorial/

echo "=== Setup complete ==="