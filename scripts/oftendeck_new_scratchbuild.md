This is a complete remake of my Raspberry Pi OS for OftenDeck Ver 1.1. It uses an install script and symlinks to keep everything neat and organized together.

## Flashing the SD Card

Flash an SD card with raspberry pi OS lite. 

## Screen Adjustments

The /boot/cmdline.txt and/or the /boot/config.txt May need to be changed to fit screen rotation.

- For Raspberry Pi 4, official 7" touch screen, and Smartipi case, no additional configuration is needed.
- For Microjournal Rev.2.ReVamp, the following needs to be added to the `cmdline.txt`:
   - `video=HDMI-A-1:400x1280M@60,rotate=90 
`
## Run Install Script

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
Note:

When exporting, remove...
- private and public SSH key
- shell history
- WiFi credentials
- Git Credentials
- Authorized SSH
```