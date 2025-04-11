#!/bin/bash

# Get the current terminal size (width and height)
TERMINAL_WIDTH=$(tput cols)
TERMINAL_HEIGHT=$(tput lines)

# Function to display the main menu
show_menu() {
  CHOICE=$(whiptail --title "Main Menu" --menu "Choose an option" $TERMINAL_HEIGHT $TERMINAL_WIDTH 6 \
    "Settings" "(Configure device)" \
    "Sharing" "(Wirelessly connect to device)" \
    "Backup" "(Generate an SD card image backup)" \
    "Battery Saving Mode" "(Enable battery saving mode)" \
    "Shutdown" "(Shutdown the system)" \
    --ok-button "Select" --cancel-button "Exit" 3>&1 1>&2 2>&3)

  if [ $? -ne 0 ]; then
    clear
    exit 0
  fi

  case $CHOICE in
    "Settings")
      show_settings
      ;;
    "Sharing")
      show_sharing
      ;;
    "Backup")
      /usr/local/bin/often-backup.sh
      show_menu
      ;;
    "Battery Saving Mode")
      show_battery_menu
      ;;
    "Shutdown")
      shutdown now
      ;;
    *)
      echo "Invalid option"
      show_menu
      ;;
  esac
}

# Function to display settings sub-menu
show_settings() {
  SETTING_CHOICE=$(whiptail --title "Settings Menu" --menu "Choose a setting" $TERMINAL_HEIGHT $TERMINAL_WIDTH 3 \
    "RasPi Config" "(Configure Raspberry Pi)" \
    "Console Font" "(Change font settings)" \
    --ok-button "Select" --cancel-button "Back" 3>&1 1>&2 2>&3)

  if [ $? -ne 0 ]; then
    show_menu
  fi

  case $SETTING_CHOICE in
    "RasPi Config")
      sudo raspi-config
      ;;
    "Console Font")
      sudo dpkg-reconfigure console-setup
      ;;
    *)
      echo "Invalid option"
      ;;
  esac

  show_menu
}

# Function to display sharing sub-menu
show_sharing() {
  SHARING_CHOICE=$(whiptail --title "Sharing Options" --menu "Choose a sharing option" $TERMINAL_HEIGHT $TERMINAL_WIDTH 3 \
    "WiFi AP" "(Enable hotspot w/o Internet & Share)" \
    "WiFi Network" "(Connect to external WiFi & Share)" \
    --ok-button "Select" --cancel-button "Back" 3>&1 1>&2 2>&3)

  if [ $? -ne 0 ]; then
    show_menu
  fi

  case $SHARING_CHOICE in
    "WiFi AP")
      /usr/local/bin/often-wifi-ap.sh
      ;;
    "WiFi Network")
      /usr/local/bin/often-wifi-network.sh
      ;;
    *)
      echo "Invalid option"
      ;;
  esac

  show_menu
}

# Function to display Battery Saving Mode sub-menu
show_battery_menu() {
  BATTERY_CHOICE=$(whiptail --title "Battery Saving Mode" --menu "By default, Battery Saving Mode is turned on at boot." $TERMINAL_HEIGHT $TERMINAL_WIDTH 2 \
    "Turn Off" "(Turn Off Battery Saving Mode)" \
    "Turn On" "(Reenable Battery Saving Mode)" \
    --ok-button "Select" --cancel-button "Back" 3>&1 1>&2 2>&3)

  if [ $? -ne 0 ]; then
    show_menu
  fi

  case $BATTERY_CHOICE in
    "Turn Off")
      /usr/local/bin/often-battery-mode-off.sh
      ;;
    "Turn On")
      /usr/local/bin/often-battery-mode-on.sh
      ;;
    *)
      echo "Invalid option"
      ;;
  esac

  show_menu
}

# Function to shutdown the system
shutdown_system() {
  whiptail --title "Shutdown" --yesno "Do you really want to shutdown?" $TERMINAL_HEIGHT $TERMINAL_WIDTH \
    --yes-button "Yes" --no-button "Back"

  if [ $? -ne 0 ]; then
    show_menu
  fi

  sudo shutdown now
}

# Main script execution
show_menu
