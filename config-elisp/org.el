(use-package org
  :config (setq org-startup-folded 'content))

(define-key org-mode-map (kbd "<C-return>") 'org-cycle)

(define-key org-mode-map (kbd "<C-tab>") 'nil)

(defun org-move-lines()
  (define-key org-mode-map (kbd "M-p") 'org-metaup)
  (define-key org-mode-map (kbd "M-P") 'drag-stuff-up)
  (define-key org-mode-map (kbd "M-n") 'org-metadown)
  (define-key org-mode-map (kbd "M-N") 'drag-stuff-down))
(add-hook 'org-mode-hook 'org-move-lines)

(define-key org-mode-map (kbd "S-TAB") 'my/expand-region)

(add-hook 'org-mode-hook (lambda () (interactive) (hungry-delete-mode)))

(define-key org-mode-map (kbd "C-k") (lambda ()
               (interactive)
               (org-beginning-of-line)
               (org-kill-line)
               (org-kill-line)))

(add-hook 'org-mode-hook (lambda () (interactive) (setq-local truncate-lines -1)))

(define-key org-mode-map [(shift return)] 'crux-smart-open-line)

(use-package org-bullets
  :ensure t
  :hook ((org-mode . org-bullets-mode))
  :config (setq-default org-ellipsis " ▸"))

(setq org-todo-keyword-faces
      '(("Waiting" . (:foreground "tomato1" :weight bold))
        ("To Do" . (:foreground "dark khaki" :weight bold))
        ("In Progress" . (:foreground "DodgerBlue2" :weight bold))
        ("Done" . (:foreground "forest green" :weight bold))))
(setq org-todo-keywords
        '((sequence "To Do" "In Progress" "Done" "Waiting")))

(define-key org-mode-map (kbd "<s-return>") 'org-open-at-point)

(define-key org-mode-map (kbd "C-o") (lambda()
                                       (interactive)
                                       (org-beginning-of-line)
                                       (org-open-line 1)))

(define-key org-mode-map (kbd "C-s-p") 'org-up-element)

(define-key org-mode-map (kbd "C-,") (lambda ()
               (interactive)
               (org-beginning-of-line)
               (set-mark-command nil)
               (org-end-of-line)
               (copy-keep-highlight (region-beginning) (region-end))))
