#!/bin/bash

# Exit if any command fails
set -e

echo "Starting Dufs installation script..."

# Step 1: Install Rust (accept all defaults non-interactively)
echo "Installing Rust with default settings..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Step 2: Load Cargo environment
echo "Loading Cargo environment..."
source "$HOME/.cargo/env"

# Step 3: Verify Cargo is available
if ! command -v cargo &> /dev/null; then
    echo "Cargo could not be found. Rust installation may have failed."
    exit 1
fi

# Step 4: Install Dufs without prompts
echo "Installing Dufs (this may take a long time)..."
cargo install dufs --quiet

# Step 5: Confirm Dufs installed
if command -v dufs &> /dev/null; then
    echo "Dufs installed successfully!"
    dufs --version
else
    echo "Dufs installation failed."
    exit 1
fi