#!/bin/bash
echo "Stopping battery saving mode..."

SERVICES=(
  bluetooth
  avahi-daemon
  cron
  ModemManager
  NetworkManager
  nmbd
  smbd
  systemd-timesyncd
  triggerhappy
  wpa_supplicant
  exim4
)

for S in "${SERVICES[@]}"; do
  echo "Starting $S..."
  sudo systemctl start "$S"
done

echo "Battery saving mode is off."
