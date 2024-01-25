(global-set-key (kbd "<backtab>") 'un-indent-by-removing-2-spaces)
(defun un-indent-by-removing-2-spaces ()
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^  ")
        (replace-match "")))))

(global-set-key (kbd "M-;") 'comment-line)

(use-package emacs
  :hook ((prog-mode . display-line-numbers-mode)))

(use-package drag-stuff
  :ensure t
  :bind (("M-p" . drag-stuff-up)
	 ("M-n" . drag-stuff-down)))

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1)
  (yank)
  (yank)
  (previous-line)
  (backward-word))
(global-set-key (kbd "C-c d") 'duplicate-line)

(electric-pair-mode 1)

(use-package expreg
  :ensure t
  :bind (("s-<tab>" . expreg-expand)
	 ("S-TAB" . expreg-expand)))

(key-chord-define-global "-=" 'flymake-show-buffer-diagnostics)

(show-paren-mode 1)

(use-package hungry-delete
  :ensure t
  :hook ((org-mode . hungry-delete-mode)
	 (prog-mode . hungry-delete-mode)))

(use-package iedit
  :ensure t
  :bind (("C-c ;" . iedit-mode)))

(use-package evil-numbers
  :ensure t
  :bind (("M-s-p" . evil-numbers/inc-at-pt)
	 ("M-s-n" . evil-numbers/dec-at-pt)))

(defun kill-ring-clear () (interactive) (setq kill-ring nil))

(global-set-key (kbd "C-k") (lambda () (interactive) (insert-char 32 1) (kill-whole-line)))

(defun backward-delete-word-no-copy (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-word-no-copy (- arg)))
(global-set-key (kbd "<C-backspace>") 'backward-delete-word-no-copy)
(global-set-key (kbd "<M-backspace>") 'backward-delete-word-no-copy)

(defun delete-word-no-copy (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))
(global-set-key (kbd "M-d") 'delete-word-no-copy)

(use-package markdown-mode :ensure t)

(defun open-line-below ()
  (interactive)
  (move-end-of-line 1)
  (newline))
(global-set-key [(shift return)] 'open-line-below)

(use-package outline
  :hook ((elixir-ts-mode . outline-minor-mode))
  :bind (("C-<return>" . outline-cycle)
	 ("C-S-<return>" . my/outline-cycle-buffer))
  :config (setq outline-blank-line t)
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " ⏵"))))
  (defun my/outline-cycle-buffer () (interactive)
	 (if (eq outline--cycle-buffer-state 'show-all) (setq outline--cycle-buffer-state 'top-level))
	 (outline-cycle-buffer)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(defun refill-region (beg end)
  (interactive "*r")
  (unfill-region beg end)
  (let ((end (line-end-position)))
    (fill-region beg end)))
(global-set-key (kbd "C-x r r") 'refill-region)

(defun mark-line () (back-to-indentation) (set-mark-command nil) (move-end-of-line))
(global-set-key (kbd "C-,") 'mark-line)

(use-package spell-fu
  :ensure t
  :config (setq ispell-personal-dictionary "~/.emacs.d/.local/etc/ispell/.pws")
  :hook ((prog-mode . spell-fu-mode)
	 (org-mode . spell-fu-mode)
	 (markdown-mode . spell-fu-mode)))

(use-package emacs
  :hook ((prog-mode minibuffer-setup shell-mode) . subword-mode))

(use-package sudo-edit :ensure t)

(use-package symbol-overlay
  :ensure t
  :bind (("M-i" . symbol-overlay-put))
  :hook ((prog-mode . symbol-overlay-mode)))

(use-package topsy
  :ensure t
  :hook prog-mode)

(use-package undo-fu
  :ensure t
  :bind (("C-z" . undo-fu-only-undo)
	 ("C-S-z" . undo-fu-only-redo)))
(use-package undo-fu-session
  :ensure t
  :hook (after-init . global-undo-fu-session-mode)
  :custom (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(use-package emacs
  :bind (("M-u" . upcase-char)
	 ("M-l" . downcase-dwim)))

(use-package ws-butler
  :ensure t
  :hook ((prog-mode . ws-butler-mode)))
