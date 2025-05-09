;; Used for the OftenDeck
;; Last Updated 2025.04.24

;;; ---
;;; Package Initialization and Management
;;; ---

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; ---
;;; Package Configurations
;;; ---

(use-package bind-key)
(use-package imenu-list)
(use-package dired-sidebar)
(use-package markdown-mode)
(use-package yasnippet)
(use-package pandoc-mode)
(use-package evil
  :defer t) ; installed but not enabled at startup

;;; ---
;;; General UI & Behavior Customizations
;;; ---

;; Backups & Autosave saved to `~/.emacs.d/backups/`
(make-directory "~/.emacs.d/backups/" t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/backups/" t)))

;; Word wrapping for all documents
(global-visual-line-mode 1)

;; Remove menu bar
(menu-bar-mode -1)

;; No horizontal screen splits
(setq split-window-preferred-function nil)

;; Custom settings file
(setq custom-file "~/.emacs.d/custom.el")

;; Mode-line: filename and word count only
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                "File: %b "
                " | Words: "
                (:eval (format "%d" (count-words (point-min) (point-max))))
                mode-line-end-spaces))

;;; ---
;;; Keybindings and Navigation
;;; ---

;; Disable `C-z` suspend, remap to undo
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)
(global-unset-key (kbd "C-x C-z"))

;; Etch A Sketch navigation (custom keyboard)
(global-set-key (kbd "<f5>") 'other-window)
(global-set-key (kbd "<f6>") 'previous-buffer)

;; Sidebar controls
(global-set-key (kbd "<f17>") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "<f18>") 'imenu-list-smart-toggle)

;;; ---
;;; Spellcheck Configuration (Hunspell)
;;; ---

(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")
(setenv "LANG" "en_US.UTF-8")
(setenv "DICPATH" "/usr/share/hunspell")

(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
(setq ispell-extra-args '("-a" "-i" "utf-8"))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "M-;") 'flyspell-auto-correct-previous-word))

;;; ---
;;; Miscellaneous Features
;;; ---

;; Enable repeat-mode
(repeat-mode 1)

;;; Makes PDF and previews it
(defun md-preview-with-fbgs ()
  "Convert the current Markdown file to PDF using pandoc, then open it with often-fimgs in tty7."
  (interactive)
  (when-let ((md-file (buffer-file-name)))
    (let ((pdf-file (concat (file-name-sans-extension md-file) ".pdf")))
      (save-buffer)
      ;; Debugging output to check the command being run
      (message "Running pandoc command: pandoc %s -o %s" md-file pdf-file)
      (shell-command (format "pandoc %s -o %s" md-file pdf-file))
      ;; Create tty7, run often-fimgs, then kill tty7 after use
      (shell-command
       (format "sudo openvt -c 7 -sw -- often-fimgs %s && sudo kill -9 $(ps aux | grep tty7 | awk '{print $2}')"
               pdf-file)))))

(global-set-key (kbd "C-c p") 'md-preview-with-fbgs)

;;; Prints Using Cups with Printer Prompt
(defun print-associated-pdf ()
  "Prompt for a printer and print the PDF version of the current Markdown file."
  (interactive)
  (when-let ((md-file (buffer-file-name)))
    (let* ((pdf-file (concat (file-name-sans-extension md-file) ".pdf"))
           (printers (split-string (shell-command-to-string "lpstat -p | awk '{print $2}'") "\n" t))
           (printer (completing-read "Choose printer: " printers)))
      (if (file-exists-p pdf-file)
          (progn
            (shell-command (format "lp -d %s %s" printer pdf-file))
            (message "Sent %s to printer %s" pdf-file printer))
        (message "PDF file not found: %s" pdf-file)))))

(global-set-key (kbd "C-c P") 'print-associated-pdf)