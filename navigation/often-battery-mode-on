#!/bin/bash
echo "Starting battery saving mode..."

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
  echo "Stopping $S..."
  sudo systemctl stop "$S"
  sudo systemctl disable "$S"
done

echo "Battery saving mode is active."
