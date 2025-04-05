# OftenDeck

## Microjournal Rev.2.ReVamp Custom ROM

A custom settings built on RasPi OS Lite for the Microjournal Rev.2.ReVamp.

**The image if my sd card is not uploaded yet. I need to remove some personal files first.**

The goal of this project is to work on the software side of building a writerdeck using Raspberry Pi OS Lite (for now) and Raspberry Pi Zero 2w. The goal is fast boot times, custom navigation, seamless file sharing, and a tailored writing environment.

---

## 🚀 Features

- **Faster Boot Time:** Fast boot time by disabling unused features. (Bluetooth, Audio Drivers, etc.)
- **Custom Navigation Menu:** Built with `whiptail` (also used by `raspi-config`), featuring its own file navigation for opening and creating documents.
- **Integrated File Management:**
  - **Automatic Backup:** Files sync to your phone via Syncthing whenever Wi-Fi (hotspot or external) connects.
  - **Wireless File Access:** Edit, upload, and download files using Dufs — a lightweight, clean file manager.
- **Optimized Writing Environment:**
  - Default text editor swapped from WordGrinder to Emacs for split-screen capabilities.
  - Knob side buttons mapped to toggle between split screens and switch tabs.
- **Offline File Sharing:** Share files between your phone and Microjournal over a local Wi-Fi access point — no external network required.
- **Custom Keycaps:** Swapped for Relegendary caps (x-keys) with hand-cut paper inserts from the original Microjournal keyboard guide.

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

