# Tutorial Part 1A - Introduction and Basic Navigation

## Introduction to OftenDeck

Hi all, it's Often. I wanted to show you what I have been working on. This is my official introduction of the OftenDeck.

OftenDeck is:

- A suite of customized FOSS apps for compact, low-power computers.

- Designed to be a central writing hub that supports the entire creative writing process from research, prewriting, drafting, to publication.

- TUI and Emacs centric.

## Basic Navigation

To scroll up and down, you rotate the leftknob. To move the cursor left and right, you rotate the rightknob.

OftenDeck was originally built on a `MicroJournal Rev.2.ReVamp`. Thus, special considerations were made for
- ARM-based processors like the `Raspberry Pi Zero 2W` and
- small LCD screens with an ultra-wide aspect ratio like the one you see here.

Though, this can be used on other screens and other devices.

## Navigating Emacs

Today, I want to show you the basic navigation and ecosystem of the OftenDeck. 

It’s built around Emacs. As you can see, when I booted up the WriterDeck, it boots directly onto Emacs. On the default welcome screen, Emacs has a tutorial which I highly recommend as an introductory course to learn basic key bindings. I pressed `C-x C-f` and `TAB` AND `RET` to open this document. This is one way to open documents. Another way is to press `SHIFT-Leftknob` to toggle the directory tree. Here, you can navigate to other files. To move to this sidebar or just to another split screen, you press <rightknob>.

## Configuring the Leftknob and Rightknob

If you just installed this onto your Microjournal Rev.2.Revamp, the leftknob and rightknob buttons have to be bound through Vial to <f5> and <f6>. A picture of my Vial configuration is on my GitHub page in the video description below.


## Switching to Tutorial Part 1B

Now, let’s move to the other file Tutorial Part 1B using the directory tree.


---

## Using the Heading Tree

Welcome back to Tutorial Part 1A. We talked about navigating between different files using the directory tree. Now I want to show you how to navigate WITHIN a single document using the `Headings Sidebar`.

You access this by pressing shift-RightKnob. This shows all the headings of the document. But it does more than that. If you switch your cursor to that screen and scroll around and press RET, you'll automatically go to that section of the document. Move to a different section and then scroll back down or up to here.


## Dealing with Emacs Shortcuts

One of the difficulties of using Emacs is the unfamiliar shortcuts, called keybindings. It is how you navigate around, and sometimes you may get stuck. If you press the wrong button, sometimes you get stuck in the mini-buffer. Usually, pressing <C-g> cancels the command.


## Copy, Paste, and Cut in Emacs

There is a way to temporarily use common keybindings like <C-c> for copy and <C-v> for paste using `M-x cua-mode`, but this is just a temporary crutch. Eventually, you will have to learn Emacs bindings (or Vim bindings if you decide to use Evil mode.)

- To copy and paste: You do C-space and arrow keys to highlight. Then you press <M-w> to withdraw.
- To paste, you use <C-y> to yank it.
- To cut, you press <C-w> to wipe.
- To copy, you press <M-w>.
- <M-y> means to yank again, going through your clipboard history, which Emacs calls the kill ring.


## File Creation and Deletion

Try to copy and paste from one document to the next. These files are currently set to read only. To play around with editing, disable `read only` by pressing `C-x C-q`. Don't worry if you make a mistake, edits to this file CANNOT be saved in Emacs (as it is set to `write only` within the file system level). If I try to save, I would press `C-x C-s`. Normally it would save, but tutorial files cannot.

Make a new document yourself by pressing `C-x C-f`, and then save it. You can delete the file using the directory tree by pressing d and then x, or you can use the m-x delete-file.

If you do want to remove all the tutorials, you have to do it using the terminal. To leave, you type C-x C-c. Then type this in the terminal:

```
sudo rm -rf ~/documents/tutorial
```