For worldbuilding, I want to do the following:

1. Names and places automatically highlighted.
2. When you place your cursor on one and press RET,
3. A sidebar opens with the corresponding notes (rather than jumping to a file).

---

# Step 1: Define Names and Notes

Generate a simple alist like this:

```
(setq my-worldbuilding-notes
      '(("John Doe" . "A retired soldier turned detective. Tall, brooding, scar across left cheek.")
        ("Eldoria" . "A mist-shrouded island kingdom known for its ancient ruins and silver forests.")
        ("Captain Blake" . "Commander of the Skyfleet, loyal but haunted by past decisions.")))
```

# Step 2: Highlight Names

Use font-lock to automatically highlight the names in writing buffer:

```
(defun my-worldbuilding-highlight ()
  (font-lock-add-keywords
   nil
   `((,(regexp-opt (mapcar 'car my-worldbuilding-notes) 'words) . font-lock-keyword-face))
   'append)
  (font-lock-flush))

(add-hook 'org-mode-hook #'my-worldbuilding-highlight) ;; or any major mode you use
```

Step 3: Bind RET to Show Notes in a Sidebar

Now override RET to check if you're on a name, and if so — display the note in a sidebar using display-buffer-in-side-window:

```
(defun my-show-worldbuilding-sidebar ()
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (if-let ((note (assoc word my-worldbuilding-notes)))
        (let ((buffer (get-buffer-create "*Worldbuilding Note*")))
          (with-current-buffer buffer
            (erase-buffer)
            (insert (format "** %s **\n\n%s" (car note) (cdr note)))
            (org-mode))  ;; optional: format as org
          (display-buffer-in-side-window buffer '((side . right))))
      (message "No worldbuilding note for: %s" word))))

;; Bind RET in your writing buffer to trigger this
(define-key org-mode-map (kbd "RET") #'my-show-worldbuilding-sidebar)
```
