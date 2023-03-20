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

(use-package emacs
  :hook ((c-mode . display-line-numbers-mode)
	 (sh-mode . display-line-numbers-mode)
	 (nix-mode . display-line-numbers-mode)
	 (elixir-ts-mode . display-line-numbers-mode)))

(electric-pair-mode 1)

(use-package expand-region
  :ensure t
  :bind (("s-<tab>" . er/expand-region)
	 ("S-TAB" . er/expand-region)))

(show-paren-mode 1)

(use-package hungry-delete
  :ensure t
  :hook ((c-mode . hungry-delete-mode)
	 (elixir-mode . hungry-delete-mode)
	 (emacs-lisp-mode . hungry-delete-mode)
	 (sh-mode . hungry-delete-mode)))

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
  :ensure t
  :init (setq mc/always-run-for-all t)
  :bind (("M-/" . mc/mark-next-like-this)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emacs
  :hook ((c-mode . subword-mode)
	 (sh-mode . subword-mode)
	 (nix-mode . subword-mode)
	 (elixir-ts-mode . subword-mode)))

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

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  :custom (yas-keymap-disable-hook
           (lambda () (and (frame-live-p corfu--frame)
                           (frame-visible-p corfu--frame))))
  :hook ((elixir-ts-mode . yas-minor-mode)
	 (after-init . yas-global-mode)))
