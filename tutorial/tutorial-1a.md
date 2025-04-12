# Tutorial Part 1A - Introduction and Basic Navigation

## Introduction to OftenDeck

Hi all, it's Often. I wanted to show you what I have been working on. This is my official introduction of the OftenDeck.

OftenDeck is:

A suite of customized FOSS apps for compact, low-power computers.

Designed to be a central writing hub that supports the entire creative writing process from research, prewriting, drafting, to publication.

TUI and Emacs centric.

## Basic Navigation

To scroll up and down, you rotate the leftknob. To move the cursor left and right, you rotate the rightknob.

OftenDeck was built on a MicroJournal Rev.2.ReVamp and form factors like it. So special considerations were made for ARM-based slower processors like the Raspberry Pi Zero 2W and for small LCD screens with an ultra-wide aspect ratio like the one you see here.


## Navigating Emacs

Today, I want to show you the basic navigation and ecosystem of the OftenDeck. It’s built around Emacs.

As you can see, when I booted up the WriterDeck, it booted directly onto Emacs into the default Emacs welcome screen. On it, it has the tutorial which I highly recommend to learn the basic key bindings. I pressed C-x C-f to open up to open up this document. This is one way to open documents. Another way is to press shift-leftknob to toggle the directory tree. Here, you can navigate to other files. To move to another split screen, you press <rightknob>.


## Configuring the Leftknob and Rightknob

The leftknob and rightknob buttons have to be changed using Vial to <f5> and <f6>, respectively, for this to work. A picture of my Vial configuration is on my GitHub page in the video description below.


## Switching to Tutorial Part 1B

Now, let’s move to the other file Tutorial Part 1B using the directory tree, the rightknob, and the RET.


---

## Using the Heading Tree

Welcome back to Tutorial Part 1A. We talked about navigating between different files using the directory tree. Now I want to show you how to navigate WITHIN a single document using the Heading Tree, which is essentially a Table of Contents.

You access this by pressing shift-RightKnob. This shows all the headings of the document. But it does more than that. If you switch your cursor to that screen and scroll around and press RET, you'll automatically go to that section of the document. Move to a different section and then scroll back down or up to here.


## Dealing with Emacs Shortcuts

One of the difficulties of using Emacs is the unfamiliar shortcuts, called keybindings. It is how you navigate around, and sometimes you get stuck. If you press the wrong button, sometimes you get stuck in the mini-buffer. Usually, pressing <C-g> cancels the command.


## Copy, Paste, and Cut in Emacs

There is a way to temporarily use common keybindings like <C-c> for copy and <C-v> for paste using m-x cua-mode, but this is just a temporary crutch. Eventually, you will have to learn Emacs bindings or Vim bindings if you are using Evil mode.

To copy and paste: You do C-space and arrow keys to highlight. Then you press <M-w> to withdraw.

To paste, you use C-Y to yank it.

To cut, you press <C-w> to wipe.

<M-Y> means to yank again, going through your clipboard history, which Emacs calls the kill ring. I like to call it purgatory.


## File Creation and Deletion

Try to copy and paste from one document to the next. Don't worry if you make a mistake, edits to this file CANNOT be saved in Emacs (it is set to write only in the file system level). Let me show you. If I try to save, I would press C-x C-s. Normally it would save, but it cannot.

Make a new document yourself using C-x C-f, and then save it. You can delete the file using the directory tree by pressing d and then x, or you can use the m-x delete-file.

If you do want to remove all the tutorials, you have to do it using terminal. To leave, you type C-x C-c. Then type this in the terminal:

sudo rm -rf ~/tutorial
