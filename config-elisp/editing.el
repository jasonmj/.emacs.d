(use-package aggressive-indent
  :ensure t
  :hook ((prog-mode . aggressive-indent-mode)))

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

(use-package outline
  :hook ((elixir-ts-mode . outline-minor-mode))
  :bind (("C-<return>" . outline-cycle)
	 ("C-S-<return>" . my/outline-cycle-buffer))
  :config (setq outline-blank-line t)
  (set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▸"))))
  (defun my/outline-cycle-buffer () (interactive)
	 (if (eq outline--cycle-buffer-state 'show-all) (setq outline--cycle-buffer-state 'top-level))
	 (outline-cycle-buffer)))

(use-package emacs :hook ((prog-mode . display-line-numbers-mode)))

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

(load-file "/home/jasonmj/.emacs.d/elisp/expreg.el")
(use-package expreg
  :bind (("s-<tab>" . expreg-expand)
	 ("S-TAB" . expreg-expand)))

(show-paren-mode 1)

(use-package hungry-delete
  :ensure t
  :hook ((prog-mode . hungry-delete-mode)))

(use-package iedit :ensure t :bind (("C-c ;" . iedit-mode)))

(use-package evil-numbers :ensure t)
(global-set-key (kbd "M-s-p") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "<M-mouse-5>") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "M-s-n") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "<M-mouse-4>") 'evil-numbers/dec-at-pt)

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
	(transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(use-package multiple-cursors
  :defer
  :ensure t
  :bind (("M-/" . mc/mark-next-like-this))
  :init
  (add-hook 'multiple-cursors-mode-hook
            (defun my/work-around-multiple-cursors-issue ()
              (load "multiple-cursors-core.el")
              (remove-hook 'multiple-cursors-mode #'my/work-around-multiple-cursors-issue))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package spell-fu
  :ensure t
  :hook ((prog-mode . spell-fu-mode)
	 (org-mode . spell-fu-mode)
	 (markdown-mode . spell-fu-mode)))

(use-package emacs :hook ((prog-mode . subword-mode)))

(use-package sudo-edit :ensure t)

(use-package symbol-overlay
  :ensure t
  :bind (("M-i" . symbol-overlay-put))
  :hook ((elixir-mode . symbol-overlay-mode)
	 (emacs-lisp-mode . symbol-overlay-mode)))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(global-set-key (kbd "M-u") 'upcase-char)
(global-set-key (kbd "M-l") 'downcase-dwim)

