(use-package magit
  :ensure t
  :init
  (with-eval-after-load 'magit-mode
    (define-key magit-mode-map (kbd "<C-tab>") 'tab-line-switch-to-next-tab))
  :bind (("C-c g" . magit))
  :config
  (setq magit-commit-show-diff nil
	magit-refresh-status-buffer nil
	magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
	magit-save-repository-buffers nil
	magit-revert-buffers 1)
  (key-chord-define-global "mg" 'magit))

(use-package magit-pretty-graph
  :straight (magit-pretty-graph :type git :host github :repo "georgek/magit-pretty-graph")
  :config
  (key-seq-define-global "pg" (lambda () (interactive) (magit-pg-repo (project-root (project-current t)))))
  (setq magit-pg-command "git --no-pager log --branches --remotes --topo-order --decorate=full --pretty=format:\"%H%x00%P%x00%an%x00%ar%x00%s%x00%d\" -n 100"))

(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))
