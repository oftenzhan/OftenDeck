# Introduction

The goal is to speed up the boot. **Warning:** Any wrong codes may require you to plug this back into your home computer and edit the SD card to properly boot. Be careful. Look at other websites to double-check that I gave you the right codes.

UPDATE (04.04.25): I decided to leave all of the Systemcrl settings on for now. This is because I'm doing a lot of troubleshooting and trying out different programs. Internet constantly turn on and off as I'm testing out programs, trying out Bluetooth, 

## /boot/firmware/config.txt

- `disable_splash=1`  
  This removes the rainbow screen shown by the GPU bootloader.

- `boot_delay=0`  
  This will make it harder to get into recovery mode, as the window of opportunity is removed.

- `dtoverlay=disable-bt`  
  This disables the onboard Bluetooth module.

- `hdmi_group=2`  
  Giving a fixed resolution so Pi doesn't have to find one.

- `hdmi_mode=85`  
  Giving a fixed resolution so Pi doesn't have to find one.

- `dtoverlay=sd-overclock,100`  
  Overclock SD card, only use reliable SD cards.

- `gpu_mem=16`  
  Leave only 16MB for GPU memory because I'm not using the GUI.

- <del>`force_turbo=1`</del>
  Forces the CPU to always run at its maximum speed. (I actually turned this one off because it ate away at battery life.)

## /boot/firmware/cmdline.txt

- `loglevel=1 quiet`  
  Quiet removes the output entirely. If quiet is removed, the terminal only shows critical and emergency notifications.

- `logo.nologo`  
  Removes the Raspberry Pi logo that’s usually shown at the top of the kernel.

## Systemctl

These services can be turned on later when needed. They consume power and slow down boot time.

- `sudo systemctl disable NetworkingManager`  
  The networking service manages network interfaces and connections on the Raspberry Pi. Disabling it will stop any network interface from being automatically configured or activated on boot. It's similar to disabling `dhcpcd` but at a broader level, affecting both wired and wireless networks.

- `sudo systemctl disable sshswitch.service`  
  SSH (Secure Shell) allows remote access to the Raspberry Pi via command-line. Disabling `ssh.service` prevents the Pi from accepting remote SSH connections, which is useful if you don't need remote access or want to improve security.

- `sudo systemctl disable hciuart.service`  
  `hciuart` initializes the Bluetooth stack on the Raspberry Pi. Disabling this will prevent Bluetooth from starting at boot. If you're not using Bluetooth, this can help reduce boot time and save power.

- `sudo systemctl disable cups.service`  
  Manages printing tasks. If you're not using printing, this service can be disabled.

- `sudo systemctl disable NetworkManager-wait-online.service`  
  Disables the service that waits for network connectivity before proceeding with other services. Disabling it may cause services that rely on network connectivity at startup to fail or behave unexpectedly.

- `sudo systemctl disable apt-daily.service`  
  Disables the daily package update service that ensures your system receives regular security patches and software updates. Disabling it will stop automatic daily updates, potentially leaving your system outdated.

- `sudo systemctl disable apt-daily-upgrade.service`  
  Disables the service that applies package upgrades in conjunction with `apt-daily.service`. Disabling it prevents the system from automatically upgrading installed packages, possibly missing important updates.

- `sudo systemctl disable apt-daily.timer`  
  Disables the timer that triggers the `apt-daily.service` at scheduled times. Disabling this timer stops the automatic initiation of daily package updates.

- `sudo systemctl disable apt-daily-upgrade.timer`  
  Disables the timer that schedules the `apt-daily-upgrade.service` to run at specific intervals. Disabling it halts the automatic application of package upgrades.

- `sudo systemctl disable ModemManager.service`  
  Disables the service that manages mobile broadband (3G/4G) devices. If your system doesn't use such devices, disabling this service can free up system resources.

- `sudo systemctl disable networking.service`  
  Disables the service that handles traditional network interfaces. Disabling it may disrupt network connectivity, especially if not using NetworkManager.
