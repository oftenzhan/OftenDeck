curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

. "$HOME/.cargo/env

cargo install dufs

dufs -A

#!/bin/bash

echo "Starting the dufs installation process..."

echo "Step 1: Installing Rust using rustup. This may take a few minutes."
echo "You may be prompted to confirm the installation. Please type 'yes' or choose defaults when asked to proceed."
sleep 3
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
sleep 3

echo "Rust installation complete."

echo "Step 2: Loading Rust environment variables..."
sleep 3
# Load Rust environment
source "$HOME/.cargo/env"
sleep 3

echo "Rust environment loaded."

echo "Step 3: Installing dufs using cargo. You may need to confirm installation by typing 'yes' or accepting the default options."
sleep 3
cargo install dufs
sleep 3

echo "dufs installation complete."

# End of script
exit 0
