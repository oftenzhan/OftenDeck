;; Used for the OftenDeck
;; Last Updated 2025.04.24

;; Melpa is included to install plugins
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Backups & Autosave saved `~/.emacs.d/backups/`
(make-directory "~/.emacs.d/backups/" t)
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backups/" t)))

;; Disabled Suspend
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

;; `C-z` is bound to Undo
(global-unset-key (kbd "C-x C-z"))

;; Etch A Sketch navigation for Two-knob Keyboard
;; The <f5> and <f6> must be set in Vial
(global-set-key (kbd "<f5>") 'other-window)
(global-set-key (kbd "<f6>") 'previous-buffer)

;; Makes ALL documents word-wrap.
(global-visual-line-mode 1)

;; I removed the menu-bar because it is rarely used.
(menu-bar-mode -1)

;; This is to control the left and right sidebar.
(require 'dired-sidebar)
(require 'imenu-list)

(setq dired-sidebar-width 20)
(setq imenu-list-size 20)

(global-set-key (kbd "<f17>") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "<f18>") 'imenu-list-smart-toggle)

;; Enable `repeat-mode` by default
(repeat-mode 1)

;; Set all custom settings in custom.el
(setq custom-file "~/.emacs.d/custom.el")

;; Add Spell-Correct
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")
(setenv "LANG" "en_US.UTF-8")
(setenv "DICPATH" "/usr/share/hunspell")
(setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(setq ispell-extra-args '("-a" "-i" "utf-8"))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "M-;")
    'flyspell-auto-correct-previous-word))

;; Remove ALL horizontal screen splits
(setq split-window-preferred-function nil)

;; Mode-line to only display Filename & Word Count
(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		"File: %b "
		" | Words: "
		(:eval (format "%d" (count-words (point-min) (point-max))))
		mode-line-end-spaces))

;; Enable Print Preview
(defun run-print-preview ()
  "Run the framebuffer-based print-preview script."
  (interactive)
  (start-process "print-preview" nil "/usr/local/bin/print-preview"))
(global-set-key (kbd "C-c C-p") 'run-print-preview)
