#!/bin/bash

# Start networking services
echo "Starting networking services..."
sudo systemctl start NetworkManager.service

# Start dufs and syncthing in the background
dufs -A &  # Run dufs in the background
DUFS_PID=$!  # Get the PID of dufs

syncthing &  # Run syncthing in the background
SYNCTHING_PID=$!  # Get the PID of syncthing

# Get the IP address of the machine
IP_ADDRESS=$(hostname -I | awk '{print $1}')

# Display the URLs for Syncthing and Dufs
echo "To connect to Syncthing, use: http://$IP_ADDRESS:8384/"
echo "To connect to Dufs, use: http://$IP_ADDRESS:5000/"

# Trap Ctrl+C and kill both processes, then stop the network
trap 'kill $DUFS_PID $SYNCTHING_PID; echo "Stopping dufs and syncthing..."; \
      echo "Stopping networking services..."; \
      sudo systemctl stop NetworkManager.service \
      # or if using NetworkManager
      # sudo systemctl stop NetworkManager
      # or if using Wi-Fi
      # sudo systemctl stop wpa_supplicant
      exit' SIGINT

# Wait indefinitely to keep the script running
wait
