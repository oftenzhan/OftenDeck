# Introduction
The custom Navigation uses `whiptail`. Whiptail is already installed in Rasberry Pi OS Lite. I put them all these files in /usr/local/bin/ so I can run it anywhere in terminal. Of course, they have to be set as executables. Reminder, this uses Emacs and Dufs to open files. If Emacs and Dufs are not installed, these files won't work.

There are several files:
| File           | Description                                                                 |
|----------------|-----------------------------------------------------------------------------|
| `navscreen`    | Main navigation screen; launches other scripts.                             |
| `open_file_script`    | File browser script for directory navigation. Only accesses one-level-down subdirectories. Deeper directories won’t be reached. You can’t save directly in the home folder — only in its immediate subfolders. |
| `new_file_script`     | Opens a new file. Cannot create directories — this must be done manually in the terminal. |
| `wifi_hotspot_no_internet` | Activates Microjournal’s Wi-Fi hotspot, launches Dufs (via Docker), starts Syncthing, and resets ownership of uploaded files to avoid root permissions. |
| `wifi_with_internet`| Connects to external Wi-Fi, then opens Dufs and Syncthing. Also resets file ownership. |
## navscreen

```
#!/bin/bash

# Get the current terminal size (width and height)
TERMINAL_WIDTH=$(tput cols)
TERMINAL_HEIGHT=$(tput lines)

# Function to display the menu
show_menu() {
  CHOICE=$(whiptail --title "Main Menu" --menu "Choose an option" $TERMINAL_HEIGHT $TERMINAL_WIDTH 5 \
    "Open File" "(Open an existing file)" \
    "New File" "(Create a new file)" \
    "Settings" "(Configure device)" \
    "Sharing" "(Wirelessly connect to device)" \
    "Shutdown" "(Shutdown the system)" \
    --ok-button "Select" --cancel-button "Exit" 3>&1 1>&2 2>&3)

  # If "Exit" (Cancel) is selected, end the script
  if [ $? -ne 0 ]; then
    clear
    exit 0
  fi

  case $CHOICE in
    "Open File")
      /usr/local/bin/open_file_script
      show_menu # Go back to the main menu after the script runs
      ;;
    "New File")
      /usr/local/bin/new_file_script
      show_menu
      ;;
    "Settings")
      show_settings
      ;;
    "Sharing")
      show_sharing
      ;;
    "Shutdown")
      shutdown_system
      ;;
    *)
      echo "Invalid option"
      show_menu
      ;;
  esac
}

# Function to display settings menu
show_settings() {
  SETTING_CHOICE=$(whiptail --title "Settings Menu" --menu "Choose a setting" $TERMINAL_HEIGHT $TERMINAL_WIDTH 2 \
    "Raspberry Pi" "(Configure Raspberry Pi)" \
    "Font" "(Change font settings)" \
    --ok-button "Select" --cancel-button "Back" 3>&1 1>&2 2>&3)

  # If "Back" (Cancel) is selected, return to main menu
  if [ $? -ne 0 ]; then
    show_menu
  fi

  case $SETTING_CHOICE in
    "Raspberry Pi")
      sudo raspi-config
      ;;
    "Font")
      sudo dpkg-reconfigure console-setup
      ;;
    *)
      echo "Invalid option"
      ;;
  esac

  # Always return to main menu after action
  show_menu
}

# Function to display sharing options
show_sharing() {
  SHARING_CHOICE=$(whiptail --title "Sharing Options" --menu "Choose a sharing option" $TERMINAL_HEIGHT $TERMINAL_WIDTH 2 \
    "WiFi AP" "(Enable hotspot w/o Internet & Share)" \
    "Shared WiFi" "(Connect to external WiFi & Share)" \
    --ok-button "Select" --cancel-button "Back" 3>&1 1>&2 2>&3)

  # If "Back" (Cancel) is selected, return to main menu
  if [ $? -ne 0 ]; then
    show_menu
  fi

  case $SHARING_CHOICE in
    "WiFi AP")
      /usr/local/bin/wifi_hotspot_no_internet
      ;;
    "Shared WiFi")
      /usr/local/bin/wifi_with_internet
      ;;
    *)
      echo "Invalid option"
      ;;
  esac

  # Always return to main menu after action
  show_menu
}

# Function to shutdown the system
shutdown_system() {
  whiptail --title "Shutdown" --yesno "Do you really want to shutdown?" $TERMINAL_HEIGHT $TERMINAL_WIDTH \
    --yes-button "Yes" --no-button "Back"

  # If "Back" (No) is selected, return to main menu
  if [ $? -ne 0 ]; then
    show_menu
  fi

  sudo shutdown now
}

# Main script execution
show_menu
```

## open_file_script

```
```

## new_file_script

```
```

## wifi_hotspot_no_internet

```
```

## wifi_with_internet

```
```
