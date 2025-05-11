# Introduction

currenty, the OftenDeck is at Version 1.1. The biggest updates from Version 1.0 and 1.1 is the use of symlinks and shell scripts.

All of the main files in are within the `~/.oftendeck` folder. Then the files are the symlinked to other system folders like `usr/local/bin` or `~/.emacs.d/init.el`. This makes it easier for myself to edit and provide updates via github, and for you to install easily onto your device.

The way its designed, minor updates can be updated using the following commands (assuming you are connected to the internet):

```
cd ~/.oftendeck
git pull
```

# Installation

For installation, the steps are simple.
1. Install Raspberry Pi OS Lite with correct screen configurations.
2. Connect to WiFi via `raspi-config`
3. Download and run General Install Script
4. Download and run Specific Device Script
5. Install Emacs plugins

And then you are ready to enjoy the OftenDeck!

# Step 1: Flashing RasPi OS Lite

## Flashing the SD Card

Flash an SD card with Raspberry Pi OS Lite. 

## Substep 1.1: Screen Adjustments

Depending on your device, you may need to adjust your screen. Typically this means rotating the screen and perhaps adjusting screen resolution. 

We will only do a minor edit to the screen so that we can access the terminal console, turn on WiFi, and then download and run the bash script from my GitHub page. The bash scripts will do the rest of the edits.

- For Raspberry Pi 4, official 7" touch screen, and Smartipi case, no additional configuration is needed.
- For Microjournal Rev.2.ReVamp, the following needs to be added to the `cmdline.txt`:
   - `video=HDMI-A-1:400x1280M@60,rotate=90 
`
# Step 2: Turn on WiFi

To turn on WiFi, type into the terminal:

```
sudo raspi-config
```

And then navigate to Wifi and type in SSID and Password.

```
1 System Options → S1 Wireless LAN → Country → Type in Wifi Name → Type in Wifi Password
```

Afterwards, you can exit `raspi-config`.

# Set 3. Run General Installation Script

Go to the terminal and type:

```
curl -sSL https://raw.githubusercontent.com/oftenzhan/OftenDeck/main/scripts/oftendeck_scratchbuild.sh | bash
```

After download in executing, it takes 35 minutes to an hour to fully run. Hopefully it doesn't spout any errors. 

Near the very end, it will seem like it is stuck, saying something like "Installing 186 of 187" and linger there for a couple of minutes. It is not stuck; this last step just takes the longest.

If it breaks for some reason, please let me know. You'll probably need to install it manually step-by-step using the [detailed build guide](detailed_build.md). Let me know what the error was so I can fix it.

# Set 4. Run Specific Device Script

Currently, there are two scripts options for custom devices:
1. The RasPi 4 with official 7-inch touch screen.
2. Raspberry Pi Zero 2W with WiseCoco 7.84 inch. (The MicroJournal Rev.2.ReVamp)

These are the two devices that I currently own.

Looking at the General Install Script, it should work for all Debian devices... I just haven't tried it. 

## Script Option 1: RasPi 4 with Official 7-inch touch screen

```sh
bash ~/.oftendeck/scripts/oftendeck_SmartiPi.sh
```

What the script does:
- Speeds up Boot
  - Removes Raspberry Pi OS rainbow boot image
  - Removes btart-up scrolling boot text

My current workhorse is a Raspberry Pi 4 with the official 7-inch touch screen. (I have the touch screen disabled.)

So far, no edits. So far everything works right out of the box. :)

## RasPi Zero 2w with WiseCoco 7.84-inch Screen.

```sg
bash ~/.oftendeck/scripts/oftendeck_Rev2Revamp.sh
```

What the script does:
- Speeds up Boot
  - Removes Raspberry Pi OS rainbow boot image
  - Removes btart-up scrolling boot text
- Rotates programs that use the framebuffer
  - Rotates fbterm
  - Rotates custom `fbgs` called often-fimgs

There are some issues with the newer `vc4-kms-v3d` driver. I cannot turn it off because I cannot find the HDMI timings for the `config.txt`. 

It should look something like this, but I cannot find it:

```
hdmi_group=2
hdmi_mode=87
hdmi_timings=400 0 100 10 140 1280 10 20 20 2 0 0 0 60 0 43000000 3
```

Thus, I cannot use `display_rotate` which is used to rotate the framebuffer. So far, I have not found a solution. I have to rotate every application that uses framebuffer rather than rotate the framebuffer itself.

In `fbterm`, I rotate it using `rotate=3` in the initrc.

In `often-fimgs` (a stripped down version of `fbgs` which uses `fim` instead of `fbi`), I had to rewrite the script to make ghostscript generate the PNG images rotated.

If someone knows how to rotate the framebuffer on the WiseCoCo 7.84, it would save me a lot of heartache.

# Todo

These are updates I plan to do whenever I have time. These are minor annoyances but not necessary for the overall function of the OftenDeck.

- OftenDeck Configurations
   - Change startup to Journal.md file.
   - fix FIM with different keybindings consistent with the rest of the OftenDeck

- Documentation
  - Directions to link my files to a private repository.
  - Directions on setting up printer
  - Directions on how to use this with Jekyll to make a blog.





