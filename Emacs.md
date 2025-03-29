# Introduction

I am using Emacs. I changed the knobs in Vial the buttons on the left and right knobs to toggle buffers and screen switchings.

The splitscreen dimensions are made specifically for the font size:

`utf-8 | Terminis Bold | 12x24`

You can edit the console font by typing into the terminal:
`sudo dpkg-reconfigure console-setup`

Below is my `init.el` file.

```
;; Personally, I do not like autosave nor lockfiles. I disabled them.
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Disabled ' Suspend' because sometimes the microjournal freezes when using it.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; The side knob buttons of the microjournal were set to <f9> and <f8> using Vial, then I bound them to Emacs for navigation.  
(global-set-key (kbd "<f9>") 'other-window)
(global-set-key (kbd "<f8>") 'previous-buffer)

;; This function makes all documents word-wrap.
(global-visual-line-mode 1)

;; I removed the menu-bar because it is rarely used.
(menu-bar-mode -1)

;; Makes the top bar of the screen (header-line) show what header or subheader the cursor is on. Also, it makes the top bar white.
(which-function-mode)
(setq-default header-line-format '((which-func-mode ("" which-func-format ""))))
(set-face-attribute 'header-line nil
		    :background "white")

;; Only shows the name of the document in the bottom bar (mode-line) of the screen 
(setq-default mode-line-format '("" "%b"))

;; Splits the window into three different lengths: ~25% ~50% ~25%.
(split-window-right 25)
(other-window -1)
(split-window-horizontally 57)
```
