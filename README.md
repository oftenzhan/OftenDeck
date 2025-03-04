# OftenDeck

## Microjournal Rev.2.ReVamp Custom ROM

A custom settings built on RasPi OS Lite for the Microjournal Rev.2.ReVamp.

The goal of this project is to work on the software side of building a writerdeck using Raspberry Pi OS Lite (for now) and Raspberry Pi Zero 2w. The goal is fast boot times, custom navigation, seamless file sharing, and a tailored writing environment.

---

## 🚀 Features

- **Faster Boot Time:** Fast boot time by disabling unused features. (Bluetooth, Audio Drivers)
- **Custom Navigation Menu:** Built with `whiptail` (used by `raspi-config`), featuring its own file navigation for opening and creating documents.
- **Integrated File Management:**
  - **Automatic Backup:** Files sync to your phone via Syncthing whenever Wi-Fi (hotspot or external) connects.
  - **Wireless File Access:** Edit, upload, and download files using Dufs — a lightweight, clean file manager.
- **Optimized Writing Environment:**
  - Default text editor swapped from WordGrinder to Emacs for split-screen capabilities.
  - Knob side buttons mapped to toggle between split screens and switch tabs.
- **Offline File Sharing:** Share files between your phone and Microjournal over a local Wi-Fi access point — no external network required.
- **Custom Keycaps:** Swapped for Relegendary caps (x-keys) with hand-cut paper inserts from the original Microjournal keyboard guide.

---

## 🗂 File Structure

All scripts are placed in `/usr/local/bin/` and must be set as executables to run from the terminal.

| File           | Description                                                                 |
|----------------|-----------------------------------------------------------------------------|
| `navscreen`    | Main navigation screen; launches other scripts.                             |
| `open_file_script`    | File browser script for directory navigation. Only accesses one-level-down subdirectories. Deeper directories won’t be reached. You can’t save directly in the home folder — only in its immediate subfolders. |
| `new_file_script`     | Opens a new file. Cannot create directories — this must be done manually in the terminal. |
| `wifi_hotspot_no_internet` | Activates Microjournal’s Wi-Fi hotspot, launches Dufs (via Docker), starts Syncthing, and resets ownership of uploaded files to avoid root permissions. |
| `wifi_with_internet`| Connects to external Wi-Fi, then opens Dufs and Syncthing. Also resets file ownership. |
---

💡 Future Plans

- Further reduce Emacs load time.
- Clean up code
- Share .img file of this build for easy replication.
- Explore additional customization and optimization options.
   - toggle system functions
- Bluetooth music
- Speech to Text using Cheetah?
---

🖤 Special Thanks

Huge thanks to Un Kyu Lee for creating the incredible Microjournal. This project wouldn’t exist without your hard work.

