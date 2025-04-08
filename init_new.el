;; 1. Startup Optimization
;; Melpa is installed so I can install plugins when necessary
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; 2. Package Management Setup (use-package)
;; Yasnippet setup
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("/my-snippets")))  ;; Custom snippet directory

;; Org-Capture setup
(use-package org-capture
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "/org/notes.org" "Notes") "* %? :NOTE:\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "/org/meetings.org" "Meetings") "* %? :MEETING:\n  %i\n  %a")
          ("t" "Task Management" entry (file "/org/reading-notes.org") "* %^{Book Title} - %U\n  %^{Note Type|Summary|Commentary}\n  %^{Summary or Quote}\n  %^{Additional Thoughts}")
          ("w" "Creative Writing" entry (file "/org/random-ideas.org") "* %^{Idea Title}: %^{Idea Description}")
          ("d" "Daily Reflection" entry (file+datetree "~/org/daily-reflections.org") "* %U\n  %^{Mood} - %^{Thoughts on today}"))))

;; 3. Custom File Load
;; Set all custom settings in custom.el
(setq custom-file "~/.emacs.d/custom.el")

;; 4. UI Settings
;; Disable autosave and lockfiles
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Disable Suspend
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Keybindings for side knob buttons
(global-set-key (kbd "<f5>") 'other-window)
(global-set-key (kbd "<f6>") 'previous-buffer)

;; Enable wordwrap
(global-visual-line-mode 1)

;; Remove the menu bar
(menu-bar-mode -1)

;; Configure sidebar (Dired and Imenu)
(require 'dired-sidebar)
(require 'imenu-list)

(setq dired-sidebar-width 20)
(setq imenu-list-size 20)

(global-set-key (kbd "<f17>") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "<f18>") 'imenu-list-smart-toggle)

;; Enable repeat mode by default
(repeat-mode 1)

;; 5. General Behavior Settings
;; Set up the directory for backup and auto-save files
(defvar my/backup-dir (expand-file-name "~/.emacs.d/backups/"))
(unless (file-exists-p my/backup-dir)
  (make-directory my/backup-dir t))

;; Disable lock files
(setq create-lockfiles nil)

;; Custom backup filename function
(defun my/backup-file-name (fpath)
  (let ((encoded (replace-regexp-in-string "/" "!" fpath)))
    (expand-file-name encoded my/backup-dir)))

(setq make-backup-file-name-function #'my/backup-file-name
      backup-by-copying t            ;; Don't clobber symlinks
      version-control t              ;; Use version numbers
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Custom auto-save filename function
(defun my/auto-save-file-name ()
  (if buffer-file-name
      (let* ((encoded-name (replace-regexp-in-string "/" "!" buffer-file-name))
             (autosave-name (concat "#" encoded-name "#")))
        (expand-file-name autosave-name my/backup-dir))
    (make-auto-save-file-name)))  ;; fallback

(setq auto-save-file-name-transforms `((".*" my/auto-save-file-name t)))

;; 6. Package Configuration & Keybindings
;; Configure Spell-Correct
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")
(setenv "LANG" "en_US.UTF-8")
(setenv "DICPATH" "/usr/share/hunspell")
(setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(setq ispell-extra-args '("-a" "-i" "utf-8"))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "M-;") 'flyspell-auto-correct-previous-word))

;; 7. Language-Specific Packages
;; Org-Capture configurations for specific templates
;; (Already covered in the package setup above)

;; 8. Custom Functions
;; Custom function to force vertical split (commented out)
;; (defun my-force-vertical-split (orig-fun &optional window size side)
;;   "Force every split to be vertical."
;;   (apply orig-fun (list window size 'right)))
;; (advice-add 'split-window :around #'my-force-vertical-split)

;; 9. Final Message
;; Print a final message on startup
(message "Emacs loaded successfully!")
