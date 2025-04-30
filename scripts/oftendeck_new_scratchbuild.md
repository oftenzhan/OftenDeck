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
And then navigate to Wifi and type in SSID and Password.

```
1 System Options → S1 Wireless LAN → Country → Type in Wifi Name → Type in Wifi Password
```
Afterwards, you can exit `raspi-config`.

### Run Script

Go to the terminal and type:

```
curl -sSL https://raw.githubusercontent.com/oftenzhan/OftenDeck/main/scripts/oftendeck_scratchbuild.sh | bash
```

It should take an hour to download. Hopefully it doesn't spout any errors. If it does, let me know, and do it manually step-by-step using the [detailed build guide](detailed_build.md).

```
Note:

When exporting, remove...
- private and public SSH key
- shell history
- WiFi credentials
- Git Credentials
- Authorized SSH
```