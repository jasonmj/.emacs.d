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
(use-package magit-gh
:ensure t
:after magit)

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

(use-package diffview 
  :straight t
  :config
  ;; Pad lines to extend highlighting to window width using invisible text
  (defun diffview--pad-line-to-window-width (line)
    "Pad LINE with spaces to window width for full-width highlighting."
    (let* ((window-width (- (window-width) 1))  ; Account for fringe/margin
           (line-length (length line))
           (padding-needed (max 0 (- window-width line-length))))
      (if (> padding-needed 0)
          (concat line (propertize (make-string padding-needed ?\s) 'invisible t))
        line)))

  ;; Override diffview's print function to add padding
  (defun diffview--print-all-lines-to-buffer-padded (lines buffer-name)
    "Print lines to buffer with padding for full-width highlighting."
    (let ((old-temp-buffer (get-buffer buffer-name)))
      (when old-temp-buffer
        (kill-buffer old-temp-buffer))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (dolist (line lines)
          (insert (diffview--pad-line-to-window-width line) "\n")))))

  ;; Monkey-patch diffview to use padded output
  (advice-add 'diffview--print-all-lines-to-buffer 
              :override #'diffview--print-all-lines-to-buffer-padded)

  ;; Ensure diffview buffers display cleanly
  (add-hook 'diffview-mode-hook (lambda ()
    (display-line-numbers-mode -1)
    (scroll-bar-mode -1)
    (horizontal-scroll-bar-mode -1)
    (setq-local show-trailing-whitespace nil)
    (setq-local indicate-empty-lines nil))))

;; Make diff backgrounds extend to full window width
(setq diff-font-lock-syntax 'hunk-only)
(with-eval-after-load 'diff-mode
  (set-face-attribute 'diff-added nil :extend t)
  (set-face-attribute 'diff-removed nil :extend t)
  (set-face-attribute 'diff-context nil :extend t)
  (set-face-attribute 'diff-header nil :extend t))
