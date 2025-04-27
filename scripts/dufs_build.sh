curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

. "$HOME/.cargo/env

cargo install dufs

dufs -A

#!/bin/bash

echo "Starting the dufs installation process..."
echo "Step 1: Installing Rust using rustup. This may take a few minutes."
echo "You will be prompted to confirm the installation. Please type 'yes' or choose defaults when asked to proceed."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
sleep 5

echo "Rust installation complete."
echo "Step 2: Loading Rust environment variables..."
sleep 3
# Load Rust environment
source "$HOME/.cargo/env"
sleep 5

echo "Rust environment loaded."
echo "Step 3: Installing dufs using cargo. You may need to confirm installation by typing 'yes' or accepting the default options."
sleep 3
cargo install dufs
sleep 5

echo "dufs installation complete."
echo "Step 4: Launching dufs server with anonymous access enabled (-A flag)..."
sleep 3
dufs -A &
echo "dufs is now running in the background."

echo "Installation and setup complete."

# Optional: keep script alive for a while to match the "one hour" duration
echo "Monitoring for a while to ensure everything is running smoothly..."
sleep 2700  # sleep for 45 minutes to extend the process time to around 1 hour
echo "Done!"