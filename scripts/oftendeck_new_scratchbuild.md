This is a complete remake of my Raspberry Pi OS for version 2. It uses an install script.

Flash an SD card with raspberry pi OS lite.

Let it run in Raspberry Pi. 

### Enable and connect to Wifi

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
```

### Download and Install OftenDeck GitHub page.
```
cd
git clone https://github.com/oftenzhan/OftenDeck.git .oftendeck
```

### (For developers)

create ssh key

```
ssh-keygen -t ed25519 -C "your_email@example.com"
```

show ssh key

```
cat ~/.ssh/id_ed25519.pub
```

copy to GitHub account

change repo to use ssh instead of https

```
cd /path/to/your/repo
git remote set-url origin git@github.com:username/repository.git

```

push with this

```
git add .
git commit -m "Your commit message here"
git push

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