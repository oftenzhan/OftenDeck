# Introduction
The custom Navigation uses `whiptail`. Whiptail is already installed in Rasberry Pi OS Lite. I put them all these files in /usr/local/bin/ so I can run it anywhere in terminal. Of course, they have to be set as executables. Reminder, this uses Emacs and Dufs to open files. If Emacs and Dufs are not installed, these files won't work.

There are several files:
| File           | Description                                                                 |
|----------------|-----------------------------------------------------------------------------|
| `navscreen`    | Main navigation screen; launches other scripts.                             |
| `open_file_script`    | File browser script for directory navigation. Only accesses one-level-down subdirectories. Deeper directories won’t be reached. You can’t save directly in the home folder — only in its immediate subfolders. |
| `new_file_script`     | Opens a new file. Cannot create directories — this must be done manually in the terminal. |
| `wifi_hotspot_no_internet` | Activates Microjournal’s Wi-Fi hotspot, launches Dufs (via Docker), starts Syncthing, and resets ownership of uploaded files to avoid root permissions. |
| `wifi_with_internet`| Connects to external Wi-Fi, then opens Dufs and Syncthing. Also resets file ownership. |
## navscreen

```

```

## open_file_script

```
```

## new_file_script

```
```

## wifi_hotspot_no_internet

```
```

## wifi_with_internet

```
```
