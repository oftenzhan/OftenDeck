# Introduction
The custom Navigation uses `whiptail`. Whiptail is already installed in Rasberry Pi OS Lite. I put them all these files in `/usr/local/bin/` so I can run it anywhere in terminal. Of course, they have to be set as executables. Reminder, this uses `Emacs` and `Dufs` to open files. If Emacs and Dufs are not installed, these files won't work.

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
Built using [FileBrowser](https://github.com/pageauc/FileBrowser)
with minor alterations.

```
#!/bin/bash

# Set the starting directory
base_directory="/home/microjournal/documents/"

# Get terminal dimensions
height=$(tput lines)
width=$(tput cols)

# Get the list of subdirectories
folders=($(ls -d "$base_directory"*/ | sed "s|$base_directory||" | sed 's|/||'))

# Make sure there are folders to choose from
if [ ${#folders[@]} -eq 0 ]; then
    whiptail --msgbox "No folders found in $base_directory" $height $width
    exit 1
fi

# Turn folder list into whiptail menu options
folder_options=()
for folder in "${folders[@]}"; do
    folder_options+=("$folder" "")
done

# Let the user choose a subdirectory with full screen
subdirectory=$(whiptail --title "Choose a folder" --menu "Available folders in $base_directory:" $height $width ${#folders[@]} "${folder_options[@]}" 3>&1 1>&2 2>&3)

# If user cancels, exit
if [ -z "$subdirectory" ]; then
    exit 1
fi

# Construct the full directory path
directory="$base_directory$subdirectory"

# Ask for the file name, fully fullscreen
filename=$(whiptail --inputbox "Enter the name of the file (e.g., file.txt):" $height $width 3>&1 1>&2 2>&3)

# If user cancels or leaves filename blank, exit
if [ -z "$filename" ]; then
    exit 1
fi

# Construct the full file path
filepath="$directory/$filename"

# Confirm and open in Emacs, fully fullscreen
if whiptail --yesno "Open $filepath in Emacs?" $height $width; then
    emacs "$filepath"
else
    whiptail --msgbox "Operation cancelled." $height $width
fi
```

## new_file_script

```
#!/bin/bash

startdir="/home/microjournal/documents"
filext='md|txt|org'  # Multiple file extensions separated by |
menutitle="File Selection Menu"

#------------------------------------------------------------------------------
function Filebrowser()
{
    # Get current terminal size
    HEIGHT=$(tput lines)
    WIDTH=$(tput cols)

    # No padding, use the full terminal dimensions
    ((HEIGHT=HEIGHT))
    ((WIDTH=WIDTH))

    if [ -z $2 ] ; then
        dir_list=$(ls -lhp | awk -F ' ' '{ print $9 " " $5 }')
    else
        cd "$2"
        dir_list=$(ls -lhp | awk -F ' ' '{ print $9 " " $5 }')
    fi

    curdir=$(pwd)
    if [ "$curdir" == "/" ] ; then  # Check if you are at root folder
        selection=$(whiptail --title "$1" \
                              --menu "PgUp/PgDn/Arrow Enter Selects File/Folder\nor Tab Key\n$curdir" $HEIGHT $WIDTH 0 \
                              --cancel-button Cancel \
                              --ok-button Select $dir_list 3>&1 1>&2 2>&3)
    else   # Not Root Dir so show ../ BACK Selection in Menu
        selection=$(whiptail --title "$1" \
                              --menu "PgUp/PgDn/Arrow Enter Selects File/Folder\nor Tab Key\n$curdir" $HEIGHT $WIDTH 0 \
                              --cancel-button Cancel \
                              --ok-button Select ../ BACK $dir_list 3>&1 1>&2 2>&3)
    fi

    RET=$?
    if [ $RET -eq 1 ]; then  # Check if User Selected Cancel
       return 1
    elif [ $RET -eq 0 ]; then
       if [[ -d "$selection" ]]; then  # Check if Directory Selected
          Filebrowser "$1" "$selection"
       elif [[ -f "$selection" ]]; then  # Check if File Selected
          if [[ $selection =~ \.($filext)$ ]]; then   # Check if selected file matches allowed extensions
            if (whiptail --title "Confirm Selection" --yesno "DirPath : $curdir\nFileName: $selection" $HEIGHT $WIDTH \
                         --yes-button "Confirm" \
                         --no-button "Retry"); then
                filename="$selection"
                filepath="$curdir"    # Return full filepath and filename as selection variables
            else
                Filebrowser "$1" "$curdir"
            fi
          else   # Not correct extension so Inform User and restart
             whiptail --title "ERROR: Invalid File Extension" \
                      --msgbox "$selection\nYou Must Select a file with one of the following extensions: $filext" $HEIGHT $WIDTH
             Filebrowser "$1" "$curdir"
          fi
      else
          # Could not detect a file or folder so Try Again
          whiptail --title "ERROR: Selection Error" \
                   --msgbox "Error Changing to Path $selection" $HEIGHT $WIDTH
          Filebrowser "$1" "$curdir"
       fi
    fi
}

Filebrowser "$menutitle" "$startdir"

exitstatus=$?
if [ $exitstatus -eq 0 ]; then
    if [ "$selection" == "" ]; then
        echo "User Pressed Esc with No File Selection"
    else
	emacs $filepath/$filename
    fi
else
    echo "User Pressed Cancel with No File Selected."
fi
```

## wifi_hotspot_no_internet

```
#!/bin/bash

# Function to run when Ctrl + C (SIGINT) is detected
cleanup() {
    echo "Disabling NetworkManager.service..."
    sudo systemctl stop NetworkManager.service
    echo "NetworkManager.service disabled."
    echo "Changing documents ownership from root to microjournal"
    sudo chown -R microjournal:microjournal /home/microjournal/documents/
    exit 0
}

# Trap Ctrl + C (SIGINT) and call the cleanup function
trap cleanup SIGINT
sudo systemctl start NetworkManager.service
sleep 5
sudo nmcli device wifi hotspot ssid microjournal password microjournal ifname wlan0
echo "########################################"
echo "usr: microjournal pwd: microjournal"
echo "Open http://$(hostname -I | awk '{print $1}'):5000 for Dufs file browsing"
echo "Open http://$(hostname -I | awk '{print $1}'):8384 for Syncthing settings"
echo "Ctrl + C to exit"
echo "########################################"
syncthing > /dev/null 2>&1 &
docker run -v /home/microjournal/documents:/data -p 5000:5000 --rm sigoden/dufs /data -A > /dev/null 2>&1
# Wait indefinitely until Ctrl + C is pressed
while true; do
    sleep 1
done
```

## wifi_with_internet

```
#!/bin/bash

# Function to run when Ctrl + C (SIGINT) is detected
cleanup() {
    echo "Disabling NetworkManager.service..."
    sudo systemctl stop NetworkManager.service
    echo "NetworkManager.service disabled."
    echo "Changing documents ownership from root to microjournal"
    sudo chown -R microjournal:microjournal /home/microjournal/documents/
    exit 0
}

# Trap Ctrl + C (SIGINT) and call the cleanup function
trap cleanup SIGINT
sudo systemctl start NetworkManager.service
sleep 5
echo "########################################"
echo "usr: microjournal pwd: microjournal"
echo "Open http://$(hostname -I | awk '{print $1}'):5000 for Dufs file browsing"
echo "Open http://$(hostname -I | awk '{print $1}'):8384 for Syncthing settings"
echo "Ctrl + C to exit"
echo "########################################"
syncthing > /dev/null 2>&1 &
docker run -v /home/microjournal/documents:/data -p 5000:5000 --rm sigoden/dufs /data -A > /dev/null 2>&1
# Wait indefinitely until Ctrl + C is pressed
while true; do
    sleep 1
done
```
