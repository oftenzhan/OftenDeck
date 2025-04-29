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

echo "=== Generate new SSH Key ==="
ssh-keygen -t ed25519 -C "oftendeck01" -N "" -f ~/.ssh/id_ed25519_oftendeck01
echo "SSH key generated successfully!"
