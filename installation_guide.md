# Update Keyboard through Vial

I recommend taking out the SD card to ensure that the SD card does not accidently get corrupted. The left knob button should be assigned to <f5> and the right knob should be assigned to <f6>.

The picture below shows what my Vial configuration looks like as reference.

![First Layer](./OftenDeck-Vial-1.png)
!![Second Layer](./OftenDeck-Vial-2.png)

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

The folder we are going to save the backups in is `/mnt/backups`

```sh
sudo mkdir /mnt/backups
```

Then, go through the prompts. When it prompts you where to save the file, I put in `/mnt/backup/oftenzhan.img

Afterwards, I run it through `image-shrink` to shrink the .img to make it fit in an smaller SD cards.

To make the root partition fit the entire SD card, use the sudo raspi-config settings to expand the partition to fit the entire SD card. 

## Step 3: Install Dufs
This makes it easier to upload scripts.

1. Install Rust

In order to install `Dufs`, Rust and Cargo need to be installed. 

```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

2. Install Dufs

Afterwards, Dufs is installed using cargo.

```
cargo install dufs
```

This takes a long time because it is compiling it. It takes around an hour.

3. Run Dufs

Do a test run with Dufs by running

```
dufs -A
```

4. Edit the html and CSS

Dufs is very barebones but that is what makes it work very well. You can change the look and feel of it. I changed the CSS to be a bit more mobile friendly (allowing text to dynamically zoom). This is my file for `index.css`.

```
html {
  font-family: -apple-system, BlinkMacSystemFont, Roboto, Helvetica, Arial, sans-serif;
  line-height: 1.5;
  color: #24292e;
}

body {
  margin: 0;
}

.hidden {
  display: none !important;
}

.head {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  padding: 0.6em 1em;
  position: sticky;
  top: 0;
  background-color: white;
}

.breadcrumb {
  font-size: 1.25em;
  padding-right: 0.6em;
  word-break: break-word;
}

.breadcrumb>a {
  color: #0366d6;
  text-decoration: none;
}

.breadcrumb>a:hover {
  text-decoration: underline;
}

.breadcrumb>b {
  color: #24292e;
}

.breadcrumb>.separator {
  color: #586069;
  padding: 0 0.25em;
}

.toolbox {
  display: flex;
  margin-right: 10px;
}

.toolbox>a,
.toolbox>div {
  height: 1.1rem;
}

.toolbox .control {
  cursor: pointer;
  padding-left: 0.25em;
}

.upload-file input {
  display: none;
}

.upload-file label {
  cursor: pointer;
}

.searchbar {
  display: flex;
  flex-wrap: nowrap;
  width: 246px;
  height: 22px;
  background-color: #fafafa;
  transition: all .15s;
  border: 1px #ddd solid;
  border-radius: 15px;
  margin-bottom: 2px;
}

.searchbar #search {
  box-sizing: border-box;
  width: 100%;
  height: 100%;
  font-size: 16px;
  line-height: 16px;
  padding: 1px;
  background-color: transparent;
  border: none;
  outline: none;
}

.searchbar .icon {
  color: #9a9a9a;
  padding: 3px 3px;
  cursor: pointer;
}

.main {
  padding: 0 1em;
}

.empty-folder {
  font-style: italic;
}

.uploaders-table th,
.paths-table th {
  text-align: left;
  font-weight: unset;
  color: #5c5c5c;
  white-space: nowrap;
}

.uploaders-table td,
.paths-table td {
  white-space: nowrap;
}

.uploaders-table .cell-status {
  width: 80px;
  padding-left: 0.6em;
}

.cell-status span {
  display: inline-block;
}

.paths-table thead a {
  color: unset;
  text-decoration: none;
}

.paths-table thead a>span {
  padding-left: 2px;
}

.paths-table tbody tr:hover {
  background-color: #fafafa;
}

.paths-table .cell-actions {
  width: 90px;
  display: flex;
  padding-left: 0.5em;
}

.paths-table .cell-mtime {
  width: 120px;
  padding-left: 0.5em;
  font-variant-numeric: tabular-nums;
}

.paths-table .cell-size {
  text-align: right;
  width: 70px;
  padding-left: 0.5em;
  font-variant-numeric: tabular-nums;
}

.path svg {
  height: 16px;
  fill: rgba(3, 47, 98, 0.5);
  padding-right: 0.5em;
  vertical-align: text-top;
}

.path {
  list-style: none;
}

.path a {
  color: #0366d6;
  text-overflow: ellipsis;
  white-space: nowrap;
  overflow: hidden;
  display: block;
  text-decoration: none;
  max-width: calc(100vw - 375px);
  min-width: 170px;
}

.path a:hover {
  text-decoration: underline;
}

.action-btn {
  padding-right: 0.3em;
  cursor: pointer;
}

.uploaders-table {
  padding: 0.5em 0;
}

.uploader {
  padding-right: 1em;
}

.editor {
  width: 100%;
  height: calc(100vh - 5rem); /* Adjust this if needed */
  border: 1px solid #ced4da;
  outline: none;
  padding: 5px;
}

.toolbox-right {
  margin-left: auto;
  margin-right: 2em;
}

.login-btn {
  cursor: pointer;
}

.save-btn {
  cursor: pointer;
  -webkit-user-select: none;
  user-select: none;
}

.logout-btn {
  cursor: pointer;
  display: flex;
  align-items: center;
}

.user-name {
  padding-left: 3px;
}

.not-editable {
  font-style: italic;
}

.retry-btn {
  cursor: pointer;
}

@media (max-width: 768px) {
  .breadcrumb {
    font-size: 1.1em;
  }
  .searchbar {
    width: 100%;
  }
}

@media (prefers-color-scheme: dark) {
  body {
    background-color: #000;
  }

  html,
  .breadcrumb>b,
  .searchbar #search {
    color: #fff;
  }

  .uploaders-table th,
  .paths-table th {
    color: #ddd;
  }

  svg,
  .path svg,
  .breadcrumb svg {
    fill: #fff;
  }

  .head {
    background-color: #111;
  }

  .searchbar {
    background-color: #111;
    border-color: #fff6;
  }

  .searchbar svg {
    fill: #fff6;
  }

  .path a {
    color: #3191ff;
  }

  .paths-table tbody tr:hover {
    background-color: #1a1a1a;
  }

  .editor {
    background: black;
    color: white;
  }
}
```

All the assets should be put in the assets folder and link it when running Dufs by typing in terminal:

```
dufs -A --assets ~/microjournal/.config/Dufs/assets
```
### Install `image-utils`

Clone the Repository

```sh
$ sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
$ sudo apt install git -y
$ cd && pwd
/home/pi
$ git clone https://github.com/seamusdemora/RonR-RPi-image-utils.git
```

### Staging & Usage

Install.

```
$ cd
$ sudo install --mode=755 ~/RonR-RPi-image-utils/image-* /usr/local/sbin
```

Now run and make a backup.

```
$ sudo image-backup

Image file to create? /mnt/Backup/oftendeck_backup.img

Initial image file ROOT filesystem size (MB) [3007]? 3500

Added space for incremental updates after shrinking (MB) [0]? 200

Create /mnt/Backup/oftendeck_backup.img (y/n)?y
```

This will take a few minutes. 

Update an existing .img backup:
Updating the image file is even easier:

```
$ sudo image-backup /mnt/Backup/oftendeck_backup.img
```

# Install Syncthing

To install Syncthing on your Raspberry Pi and configure it for Wi-Fi access, follow these steps:

1. Install Syncthing:

```
sudo apt install syncthing
```

Run syncthing to make it generate a `.config.xml` file. Once it is fully loaded (it takes less than a minute) close it by typing: `C-c`.

2. Configure Syncthing for Wi-Fi Access:

By default, Syncthing's web GUI is accessible only from the Raspberry Pi itself. To allow access over Wi-Fi, modify the GUI's listen address.

Open the Syncthing configuration file:

```
nano ~/.config/syncthing/config.xml
```

Locate the <gui> section and change the <address> to `0.0.0.0:8384`:

```
<gui enabled="true" tls="false">
    <address>0.0.0.0:8384</address>
```
Save and close the file (press <kbd>Ctrl+X</kbd>, then <kbd>Y</kbd>, and Enter).

# Install Emacs

## Installation of Emacs
Since there is no gui, just install Emacs (no X). Since we also will eventually want spellcheck (hunspell), I've included an installation of that too.

```
sudo apt-get install emacs-nox hunspell hunspell-en-us
```

## Install custom `init.el`

Copy and paste the init.el file from this GitHub into `~/.emacs/init.el`.

There shouldn't be a file there. If there is, delete it and replace it with this file.

Once the `init.el` file is placed in the proper folder, there will be an error because certain packages are not installed. Install these packages one by one by using the command `M-x package-install`

```
bind-keys
flyspell
imenu-list
dired-sidebar
markdown-mode
yassnippet
```


## Install required plugins

Here are the plugins to install to make it work:

```
markdown-mode
flyspell
dired-sidebar
sticky-keys???
```
