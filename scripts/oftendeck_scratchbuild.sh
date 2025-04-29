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

echo "=== Disabling Services in Boot for Battery Saving Mode ==="

# List of services to disable
services=(
  bluetooth
  avahi-daemon
  cron
  ModemManager
  NetworkManager
  systemd-timesyncd
  triggerhappy
  wpa_supplicant
  exim4
  # nmbd
  # smbd
)

for service in "${services[@]}"; do
  echo "Disabling $service..."
  sudo systemctl disable "$service" --now
done

echo "All listed services have been disabled."

echo "=== Network-connect Syncthing ==="

# Start syncthing to generate the config file
syncthing -no-browser &

# Give it a moment to start up
sleep 5

# Stop syncthing after it's started
pkill syncthing

# Path to the syncthing config file
config_file="$HOME/.config/syncthing/config.xml"

# Use sed to specifically change localhost to 0.0.0.0 within the <gui> section
sed -i '/<gui /,/<\/gui>/s/localhost/0.0.0.0/' "$config_file"

echo "Config file updated."

echo "=== Modify Cups for printing ==="

# Add current user to lpadmin group
sudo usermod -aG lpadmin "$USER"

# Allow remote administration, remote access, and printer sharing
sudo cupsctl --remote-admin --remote-any --share-printers

# Restart CUPS service
sudo systemctl restart cups

echo "CUPS is now configured for remote access and admin."

echo "=== Generate new SSH Key ==="
ssh-keygen -t ed25519 -C "oftendeck01"

echo "=== Setup Complete ==="
