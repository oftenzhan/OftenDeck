#!/bin/bash
set -e

echo "=== Installing Emacs packages (with MELPA) ==="

emacs --batch --eval "
(require 'package)
(add-to-list 'package-archives
             '(\"melpa\" . \"https://melpa.org/packages/\") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg
         '(bind-key
           imenu-list
           dired-sidebar
           markdown-mode
           yasnippet
           evil
           pandoc-mode))
  (unless (package-installed-p pkg)
    (package-install pkg)))
"

echo "=== Emacs package installation complete ==="