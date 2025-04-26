This is a complete remake of my Raspberry Pi OS for version 2. It uses an install script.

Flash an SD card with raspberry pi OS lite.

Let it run in Raspberry Pi. 

Enable Wifi

```
sudo raspi-config
```

`1 System Options > S1 Wireless LAN`

Go through and enter your SSID and password.

```
sudo apt upgrade -y
sudo apt update -y
sudo apt autoremove -y
sudo apt install git -y
cd
git clone https://github.com/oftenzhan/OftenDeck.git .oftendeck
```


Note:

When exporting, remove...
- private and public SSH key
- shell history
- WiFi credentials
- Git Credentials
- Authorized SSH