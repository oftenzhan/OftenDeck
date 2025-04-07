# Installation Guide

This is a step by step installation guide from scratch.

## Step 1: Initial Boot

I etch Raspberry Pi OS Lite 32-bit onto an SD card. The version I am using is `2024-11-19*`. I chose lite because I don't want a full desktop environment; 32-bit was chosen because it uses less RAM.

I used the `Disks` utility on Linux Mint Mate, but you should etch it using the software for your OS. 

There are several things that are important to do before we unmount the microsd card and put it into the MicroJournal.

We need to rotate the monitor. 

Before closing, you need to go to `/boot/cmdline.txt` and add the following to the beginning:

```sh
video=HDMI-A-1:400x1280M@60,rotate=90 
```

When you first boot up the OS, there will be some gooblygook. Then it will restart and boot. Then it will go through configuration. First for Keyboard. I chose English (US). My username and password is `oftendeck`. I will use that for file configurations from now on. Change it to your own if you used a different username and password.

Then, I edited some of the configurations:

```sh
sudo raspi-config
```

Then I did the following configurations:
> System Options > Boot / Auto Login > Console Autologin
> System Options > Wireless LAN > US United States > Type in Wifi Name > Type in Wifi Password

Once you set in your configurations, it will ask you to boot again.

Because of the small text, I adjusted the font of TTY again.

```sh
sudo dpkg-reconfigure console-setup
```

I set my font even larger to utf-8, guess optimal, Terminal Bold, 16x32.

This is a good time to take a break. To shut down the device, type:

```sh
shutdown now
```

Remember to click the off button switch to not drain the battery (if you are using battery).

## Step 2: Install `image-utils`

`image-utils` is a way to make image  backups for Raspberry Pi. This is both for you and for me. 

For you:
Like of using `dd`, this will make an `.img`file, but this will be much smaller. `dd` just copied the whole disk while `image-utils` copies the files. So my 128gb SD card containing 3gb of data using `dd` would be a 128gb large. Using `image-utils` would be 3gb large. Additionally,`image-utils` can be used within the Raspberry Pi OS instead of doing it externally and unmounted like `dd`.

For me:
I can make bootable images to share to you on GitHub
## Step 3: Install Dufs
This makes it easier to upload scripts.

### Make a Backup partition

First you need to make a backup partition. I made mine 20gb. It probably doesn't have to be that big, but I am saving different iterations as I create the MicroDeck. Ideally, you would connect to an external usb stick or a network drive. 

The MicroJournal Rev. 2 ReVamp does not have any way to connect external peripherals, a usb-stick is a no-go. We will have network syncs later, but this approach keeps everything self-contained.

Clone the Repository

```sh
$ sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
$ sudo apt install git -y
$ cd && pwd
/home/pi
$ git clone https://github.com/seamusdemora/RonR-RPi-image-utils.git
```
