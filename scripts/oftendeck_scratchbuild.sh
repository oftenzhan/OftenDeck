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
    cups \
    pandoc

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
sudo ln -sf ~/.oftendeck/tutorials ~/documents/tutorials

echo "=== Linking emacs init ==="
mkdir -p ~/.emacs.d/
sudo ln -sf ~/.oftendeck/.emacs.d/init.el ~/.emacs.d/init.el

echo "=== Clone and install image-utils ==="
git clone https://github.com/seamusdemora/RonR-RPi-image-utils.git ~/RonR-RPi-image-utils
sudo install --mode=755 ~/RonR-RPi-image-utils/image-* /usr/local/sbin

echo "=== Generate default ~/.fbtermrc ==="
fbterm <<< 'exit'

echo "=== Modify fbtermrc settings ==="
sed -i \
    -e 's/^font-names=.*/font-names=Terminus:style=Bold/' \
    -e 's/^font-size=.*/font-size=20/' \
    -e 's/^screen-rotate=.*/screen-rotate=0/' \
    ~/.fbtermrc

echo "=== Setting $EDITOR as Emacs ==="

# Add default editor settings to ~/.bashrc
echo "" >> ~/.bashrc
echo "# Set default editor to Emacs" >> ~/.bashrc
echo 'export EDITOR="emacs"' >> ~/.bashrc
echo 'export VISUAL="emacs"' >> ~/.bashrc

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

echo "=== Autologin for all TTYs ==="
USERNAME="oftendeck"

for tty in {1..6}; do
    echo "--- Setting up autologin on tty${tty} ---"
    sudo mkdir -p /etc/systemd/system/getty@tty${tty}.service.d
    sudo tee /etc/systemd/system/getty@tty${tty}.service.d/override.conf > /dev/null <<EOF
[Service]
ExecStart=
ExecStart=-/sbin/agetty --autologin ${USERNAME} --noclear %I \$TERM
EOF
done

echo "=== Reloading systemd with autologin settings ==="
sudo systemctl daemon-reload

echo "=== Generating new SSH key ==="
mkdir -p ~/.ssh
ssh-keygen -t ed25519 -C "oftendeck01" -N "" -f ~/.ssh/id_ed25519_oftendeck01
echo "=== SSH key generated successfully! ==="

echo "=== Giving fbterm permission to change keymap ==="
sudo chmod u+s /usr/bin/fbterm

echo "=== Starting Dufs installation script ==="

echo "Installing Rust with default settings..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

echo "Loading Cargo environment..."
source "$HOME/.cargo/env"

if ! command -v cargo &> /dev/null; then
    echo "Cargo could not be found. Rust installation may have failed."
    exit 1
fi

echo "Installing Dufs (this may take a long time)..."
cargo install dufs

if command -v dufs &> /dev/null; then
    echo "Dufs installed successfully!"
    dufs --version
else
    echo "Dufs installation failed."
    exit 1
fi

echo "=== Setup Complete ==="
