# Tutorial 2

## Settings:

Under Settings, there are two files that are built into all RasPi OS Lite.

The first is Raspi-Config, and the second one is Console Font.

### Raspi-Config

The most likely reason you would access RasPi-Config is to access a new WiFi network. This is how you do it:

So to set up Network Wifi:
1. Turn off Battery Saving Mode
2. Set Localization to your country. `Main Menu> Settings > RasPi Config > 5 Localization Options > L4 WLAN Country`
3. Enter Wifi SSID and password under `Main Menu > Settings > RasPi Config > System Options > Wireless LAN
4. Then you can begin `Main Menu > Sharing > Wifi Network`

After this, you do not need to turn off battery saving mode whenever you use sharing. It is done automatically. Turning off battery sharing mode is only needed when using Raspberry Config.

### Console Font

The second option one is setting up the console font. TTY1, and TTY2 **are not** changed with this. The font of TTY1 is determined by framebuffer.And TTY2 is set to a completely different frame size to accommodate the entire whiptail screen of RasPi-Config.

## Sharing

### Temporary Wifi

Once you have your Wi-Fi set up, you can use Sharing without needing to turn off battery backup mode. Both `Wifi AP` and `Wifi Network` will turn off `Battery Saving Mode` temporarily to share, and when it closes, `Battery Saving Mode` will turn on again.

### Access Point Versus Network

Using Wifi AP, a Wifi signal is created by the Writerdeck itself. This means that it cannot be connected to the internet.

Using Wifi Network, you connect to a local network. Connecting to a public network, especially at a coffee shop or hotel, has an inherit risk of people snooping at your stuff or install some sort of virus.

### Wifi Sharing

#### There are two apps that will be turned on during Wifi sharing. Here is a brief description on how they work.

- Dufs: This is a very lightweight minimalistic file sharing and uploading app. It also can create new text files and edit them. You access it on your phone or computer browser.
- Syncthing syncs folders between your devices automatically, securely, and without the cloud. It’s perfect for keeping notes, documents, or projects updated across phones, PCs, and servers.

### MarkDoc

Markdoc is a self-hosted markdown editor.

For the sake of saving RAM, MarkDoc is opened by itself. It is not very resource heavy compared to heavy `Electron` apps, nor medium `Node.js` apps, but it can consume much RAM when opening and editing large documents.

# Backup

This backup makes a ~8 gb `.img` file using RonR Image Utility. It does two things. It creates an image of the OS based on the files. Then it shrinks the root partitions to the smallest size. This allows the OS to adapt to other microsd card sizes.

Originally, I wanted it to also compress it into a `.xz` file as well, but that takes forever using the RasPi Zero. It took hours to complete. So I removed it.

