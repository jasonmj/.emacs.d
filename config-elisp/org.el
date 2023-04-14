(use-package org
  :demand t
  :bind (:map org-mode-map
	      ("<C-return>" . org-cycle)
	      ("C-k" . my/org-kill-line)
	      ("<C-tab>" . nil)
	      ("C-," . org-select-line-and-copy)
	      ("C-s-p" . org-up-element)
	      ("M-p" . org-metaup)
	      ("M-P" . drag-stuff-up)
	      ("M-n" . org-metadown)
	      ("M-N" . drag-stuff-down)
	      ("M-RET" . org-create-headline)
	      ("S-TAB" . expreg-expand)
	      ("<s-return>" . org-open-at-point)
	      ("C-o" . (lambda () (interactive) (org-beginning-of-line) (org-open-line 1))))
  :config
  (defun my/org-kill-line ()
    (interactive)
    (insert-char 32 1)
    (org-beginning-of-line)
    (org-kill-line)
    (delete-char 1))
  (defun org-create-headline ()
    (interactive)
    (org-beginning-of-line)
    (org-meta-return)
    (org-metadown))
  (defun org-select-line-and-copy ()
    (interactive)
    (org-beginning-of-line)
    (set-mark-command nil)
    (org-end-of-line)
    (copy-keep-highlight (region-beginning) (region-end)))
  :custom
  (org-startup-folded t)
  (org-todo-keyword-faces
   '(("Waiting" . (:foreground "tomato1" :weight bold))
     ("To Do" . (:foreground "dark khaki" :weight bold))
     ("In Progress" . (:foreground "DodgerBlue2" :weight bold))
     ("Done" . (:foreground "forest green" :weight bold))))
  (org-todo-keywords
   '((sequence "To Do" "In Progress" "Done" "Waiting")))
  :init (setq-local truncate-lines -1))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config (setq-default org-ellipsis " ▸"))
