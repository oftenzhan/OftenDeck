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

echo "=== Generate default ~./fbtermrc ==="
fbterm <<< 'exit'

echo "=== Modify fbtermrc settings ==="
sed -i \
    -e 's/^font-names=.*/font-names=Terminus:style=Bold/' \
    -e 's/^font-size=.*/font-size=40/' \
    -e 's/^screen-rotate=.*/screen-rotate=3/' \
    ~/.fbtermrc

echo "=== Autologin for all TTY ==="
USERNAME="oftendeck"

for tty in {1..6}; do
    mkdir -p /etc/systemd/system/getty@tty${tty}.service.d
    cat > /etc/systemd/system/getty@tty${tty}.service.d/override.conf <<EOF
[Service]
ExecStart=
ExecStart=-/sbin/agetty --autologin ${USERNAME} --noclear %I \$TERM
EOF
done

echo "=== Reloading systemd with autologin ==="
systemctl daemon-reload

echo "=== Setting $EDITOR as Emacs ==="

# Add default editor settings to ~/.bashrc
echo "" >> ~/.bashrc
echo "# Set default editor to Emacs" >> ~/.bashrc
echo 'export EDITOR="emacs"' >> ~/.bashrc
echo 'export VISUAL="emacs"' >> ~/.bashrc

echo "=== Setup Complete ==="
