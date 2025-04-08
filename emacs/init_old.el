;; Melpa is installed so I can install plugins when necessary
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Personally, I do not like autosave nor lockfiles. I disabled them.
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Disabled Suspend
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; The side knob buttons of the microjournal were set to <f9> and <f8> using Vial. This binds them to Emacs for navigating buffers and screens.
(global-set-key (kbd "<f5>") 'other-window)
(global-set-key (kbd "<f6>") 'previous-buffer)

;; This function makes all documents wordwrap.
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
;;(defun my-force-vertical-split (orig-fun &optional window size side)
;;  "Force every split to be vertical."
;;  (apply orig-fun (list window size 'right)))
;;
;;(advice-add 'split-window :around #'my-force-vertical-split)

;; org-capture template

;;(with-eval-after-load 'org
;;  (setq org-capture-templates
;;     '(("a" "Task" entry (file "~/org/tasks.org")
;;	"* TODO %?\n Entered on %U\n %i\n %a")
;;       ("b" "Note" entry (file "~/org/notes.org")
;;	"* %?\n Entered on %U\n %i\n %a")))
;;  )

;;(add-hook 'org-capture-mode-hook
;;	  (lambda ()
;;	    (define-key org-capture-select-mode-map (kbd "A") #'previous-line)
;;	    (define-key org-capture-select-mode-map (kbd "B") #'next-line)))

;; Set up the directory for backup and auto-save files
(defvar my/backup-dir (expand-file-name "~/.emacs.d/backups/"))
(unless (file-exists-p my/backup-dir)
  (make-directory my/backup-dir t))

;; Disable lock files (don't create .#filename files)
(setq create-lockfiles nil)

;; Custom backup filename function (uses path encoding)
(defun my/backup-file-name (fpath)
  (let ((encoded (replace-regexp-in-string "/" "!" fpath)))
    (expand-file-name encoded my/backup-dir)))

(setq make-backup-file-name-function #'my/backup-file-name
      backup-by-copying t            ; Don't clobber symlinks
      version-control t              ; Use version numbers
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Custom auto-save filename function (uses path encoding)
(defun my/auto-save-file-name ()
  (if buffer-file-name
      (let* ((encoded-name (replace-regexp-in-string "/" "!" buffer-file-name))
             (autosave-name (concat "#" encoded-name "#")))
        (expand-file-name autosave-name my/backup-dir))
    (make-auto-save-file-name)))  ;; fallback

(setq auto-save-file-name-transforms
      `((".*" my/auto-save-file-name t)))

;; YAsnippets configurations

(use-package yasnippet
  :init
  (yas-global-mode 1)  ;; Enable YASnippet globally
  :config
  ;; Configure snippet directories (optional, use your paths)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"    ;; Default snippets
                           "~/my-snippets"))      ;; Custom snippet directory
  )

;; Org-Capture configurations

(use-package org-capture
  :bind ("C-c c" . org-capture)  ;; Bind the capture command to C-c c
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
           "* %? :NOTE:\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\n  %i\n  %a")
          ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
           "* %? :MEETING:\n  %i\n  %a")
          )))

(use-package org-capture
  :bind ("C-c c" . org-capture)
(setq org-capture-templates
      '(("t" "Task Management" entry (file "~/org/tasks.org")
         "* TODO %^{Task Name} %^G %^{Priority} %^T %^{Deadline:} %^T")
        ("r" "Reading Notes" entry (file "~/org/reading-notes.org")
         "* %^{Book Title} - %U\n  %^{Note Type|Summary|Commentary}\n  %^{Summary or Quote}\n  %^{Additional Thoughts}")
        ("w" "Creative Writing" entry (file "~/org/creative-writing.org")
         "* %^{Story Idea Title}\n  %^{Story Concept}\n  %^{Character Ideas}")
        ("i" "Random Idea" entry (file "~/org/random-ideas.org")
         "* %^{Idea Title}: %^{Idea Description}")
        ("d" "Daily Reflection" entry (file+datetree "~/org/daily-reflections.org")
         "* %U\n  %^{Mood} - %^{Thoughts on today}")))

