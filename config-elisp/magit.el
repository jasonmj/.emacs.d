(straight-use-package
 '(git-commit
   :type git
   :host github
   :repo "magit/magit"
   :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el")))
;; Now install Magit (will reuse the same repo)

(use-package magit
  :defer t
  :straight t
  :bind (("C-c g" . magit)
	   :map magit-mode-map
	   ("<C-tab>" . tab-line-switch-to-next-tab))
  :chords ("mg" . magit)
  :custom
  (magit-commit-show-diff nil)
  (magit-refresh-status-buffer nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-save-repository-buffers nil)
  (magit-revert-buffers 1))

(use-package magit-pretty-graph
  :defer t
  :straight (magit-pretty-graph :type git :host github :repo "georgek/magit-pretty-graph")
  :config
  (key-seq-define-global "pg" (lambda () (interactive) (magit-pg-repo (project-root (project-current t)))))
  :custom
  (magit-pg-command "git --no-pager log --branches --remotes --topo-order --decorate=full --pretty=format:\"%H%x00%P%x00%an%x00%ar%x00%s%x00%d\" -n 100"))

(unless (eq system-type 'darwin)
  (use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode)))
