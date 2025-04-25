# Introduction

I am using Emacs. I'm constantly playing around with the `init.el` file. This page documents my experimental settings.

# Vial, Left and Right Knob

I changed the knobs in Vial the buttons on the left and right knobs to toggle buffer and screen switching.

# Installing `fbterm`

To leverage the limits of a non-X11 system, we will run the terminal through a "frame buffer". This allows us to do several things:
- Open up different fonts
- Expand colors to from 8 in TTY to 256 in Framebuffer.
- Allows PDF preview before printing
- Allows image preview.

```sh
sudo apt install framebuffer
```

```sh
nano ~/.fbtermrc
```

Edit the line 

`rotate=3`

# Installing `fbi`

To be able to preview PDF and view image without x-11, install FBI (frame buffer image), FBGS (frame buffer ghost script), and FIM (FBI improved).

```sh
sudo apt install fbi
```
