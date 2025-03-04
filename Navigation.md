# Introduction
The custom Navigation uses `whiptail`. Whiptail is already installed in Rasberry Pi OS Lite. I put them all these files in /usr/local/bin/ so I can run it anywhere in terminal. Of course, they have to be set as executables. Reminder, this uses Emacs and Dufs to open files. If Emacs and Dufs are not installed, these files won't work.

There are several files:
- navscreen The main navigation screen moves to different sh scripts.
- open_file The script that has a file browser that moves through your directory.
- new_file The script that opens a file. This one cannot create new directories (folders). It has to be done manually in terminal. This script only accesses one-level-down subdirectories. If there are deeper subdirectories, it won’t reach them. Also, you can’t save directly in the home folder — only in its immediate subfolders.
- wifi_hotspot & wifi_external This script tells microjournal to turn on wifi on the raspberry pi, open Dufs through docker, opens syncthing, & removes output. If any files are uploaded, they are inserted as root, so the last command is to turn all files in /home/microjournal/documents/ to be owned by microjournal so that files can be opened without sudo.
