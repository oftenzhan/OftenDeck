
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

