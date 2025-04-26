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

echo "=== Making custom bin scripts executable ==="
sudo chmod +x ~/.oftenconfig/scripts/bin/*

echo "=== Making profile.d scripts executable ==="
sudo chmod +x ~/.oftenconfig/scripts/profile.d/*

echo "=== Symlinking custom scripts into /usr/local/bin ==="
sudo ln -sf ~/.oftenconfig/scripts/bin/* /usr/local/bin/

echo "=== Symlinking login scripts into /etc/profile.d ==="
sudo ln -sf ~/.oftenconfig/scripts/profile.d/* /etc/profile.d/

echo "=== Linking tutorial folder ==="
mkdir -p ~/documents
sudo ln -sf ~/.oftenconfig/tutorial/ ~/documents/tutorial/

echo "=== Linking emacs init ==="
mkdir -p ~/.emacs/
sudo ln -sf ~/.oftenconfig/.emacs/ ~/.emacs/

echo "=== Clone and install image-utils ==="
git clone https://github.com/seamusdemora/RonR-RPi-image-utils.git ~/RonR-RPi-image-utils
sudo install --mode=755 ~/RonR-RPi-image-utils/image-* /usr/local/sbin

echo "=== Setup complete ==="
