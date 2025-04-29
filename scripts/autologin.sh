#!/bin/bash
set -e

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