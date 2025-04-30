# Introduction

Everytime I do a complete re-install, I type through my process and try to make it simpler (not only for myself, but also for people who want to install it on their own devices).

The biggest updates from Version 1.0 and 1.1 is the use of symlinks and shell scripts.

All of the main files are within the `~/.oftendeck` folder. Then it is the symlinked to other folders. This makes it easier for myself to edit and provide updates via github, and for you to install easily onto your device.

For installation, the steps are simple.
1. Install Raspberry Pi OS Lite
1. Connect to Wifi
2. Download and run General Install Script
3. Download and run Specific Device Script
4. Install Emacs plugins

And then you are ready to use the OftenDeck. 

# Installation

## Flashing the SD Card

Flash an SD card with Raspberry Pi OS Lite. 

## Screen Adjustments

Depending on your device, you need to adjust your screen. Typically this means rotating the screen and perhaps adjusting screen resolution. 

We will only do minor edits to the screen so that we can access the terminal console, turn on wifi, and then download and run the bash script. The bash scripts will do the rest of the edits.

- For Raspberry Pi 4, official 7" touch screen, and Smartipi case, no additional configuration is needed.
- For Microjournal Rev.2.ReVamp, the following needs to be added to the `cmdline.txt`:
   - `video=HDMI-A-1:400x1280M@60,rotate=90 
`
## Turn on Wifi

To turn on Wifi, type into the terminal:

```
sudo raspi-config
```

And then navigate to Enable Wifi.




### Turn on Wifi

### Run Script

```
curl -sSL https://raw.githubusercontent.com/oftenzhan/OftenDeck/main/scripts/oftendeck_scratchbuild.sh | bash
```
## Install Git and download Repository

Before installing the GitHub repository, we need to install the git app. 

### Turn on Wifi

Type the following into the terminal:

```
sudo raspi-config
```

`1 System Options > S1 Wireless LAN`

Go through and enter your SSID and password.

Once you are connected to the internet, update, upgrade, and 

```
sudo apt upgrade -y
sudo apt update -y
sudo apt autoremove -y
sudo apt install git -y
```

### Download and Install OftenDeck GitHub page.
```
cd
git clone https://github.com/oftenzhan/OftenDeck.git .oftendeck
```

### (For developers)

create ssh key

```
ssh-keygen -t ed25519 -C "oftendeck01"
```

show ssh key

```
cat ~/.ssh/id_ed25519.pub
```

copy to GitHub account under Settings > SSH keys

change repo to use ssh instead of https

```
cd /path/to/your/repo
git remote set-url origin git@github.com:oftenzhan/OftenDeck.git

```

push with this

```
git add .
git commit -m "Your commit message here"
git push
```

The first time it commits, it will ask you for Author Identity Info. Put your Name and email address. 

```
git config --global user.name "Often Zhan"
git config --global user.email "oftenzhan@gmail.com"
```


# Turn on TTY1 autologin.

```
sudo raspi-config
```

`1 System Options > S6 Auto Login > Yes`



```
curl -sSL https://raw.githubusercontent.com/oftenzhan/OftenDeck/main/scripts/oftendeck_scratchbuild.sh | bash
```











```
Note:

When exporting, remove...
- private and public SSH key
- shell history
- WiFi credentials
- Git Credentials
- Authorized SSH
```