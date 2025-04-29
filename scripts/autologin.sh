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
