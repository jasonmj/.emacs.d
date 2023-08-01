(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :bind (("C-s-SPC" . app-launcher-run-app)))

(defun copy-file-name ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
			default-directory
		      (buffer-file-name))))
      (if filename
	  (progn
	    (kill-new filename)
	    (message "Copied '%s'" filename))
	(warn "Current buffer is not attached to a file!"))))

(use-package devdocs
  :ensure t
  :config
  (defun devdocs-at-point ()
    (interactive)
    (devdocs-lookup nil (selection-or-thing-at-point)))
  (global-set-key (kbd "M-D") 'devdocs-at-point))

(global-set-key (kbd "C-s-e") 'eval-region)

(use-package git-gutter
  :ensure t
  :hook ((prog-mode . git-gutter-mode)))

(use-package grip-mode
  :ensure t
  :defer t
  :custom
  (grip-preview-use-webkit nil)
  (grip-github-user "jasonmj")
  (grip-github-password (auth-source-pick-first-password
			 :host "api.github.com"
			 :user "jasonmj^grip")))

(use-package fold-this
  :ensure t
  :config
  (defun expand-and-fold-this ()
    (interactive)
    (expreg-expand)
    (fold-this
     (car (car (region-bounds)))
     (cdr (car (region-bounds)))))
  :bind (:map shell-mode-map ("C-<return>" . expand-and-fold-this)))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)))

(use-package hs-minor-mode
  :hook ((heex-ts-mode prog-mode) . hs-minor-mode)
  :bind (("C-r" . hs-toggle-hiding)))

(global-hl-line-mode +1)

(use-package indent-bars
  :ensure t
  :straight (:type git :host github :repo "jdtsmith/indent-bars")
  :hook ((heex-ts-mode prog-mode) . (lambda () (run-with-idle-timer 0.05 nil (lambda () (indent-tabs-mode -1) (indent-bars-mode 1))))))

(key-seq-define-global "gf" 'keyboard-escape-quit)

(mouse-avoidance-mode 'banish)
(setq mouse-avoidance-banish-position '((frame-or-window . frame)
					(side . right)
					(side-pos . 0)
					(top-or-bottom . top)
					(top-or-bottom-pos . 0)))

(use-package pinentry
  :ensure t
  :hook (after-init . pinentry-start))

(use-package sideline
  :init
  (setq sideline-backends-left-skip-current-line t   ; don't display on current line (left)
        sideline-backends-right-skip-current-line t  ; don't display on current line (right)
        sideline-order-left 'down                    ; or 'up
        sideline-order-right 'up                     ; or 'down
        sideline-format-left "%s   "                 ; format for left aligment
        sideline-format-right "   %s"                ; format for right aligment
        sideline-priority 100                        ; overlays' priority
        sideline-display-backend-name t))            ; display the backend name

(use-package sideline
  :ensure t
  :init (add-to-list 'sideline-backends-right '(sideline-blame . down)))

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'point) ; 'point to show errors only on point
					      ; 'line to show errors on the current line
  (add-to-list 'sideline-backends-right '(sideline-flymake . up)))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package tramp
  :config (put 'tramp-remote-path '(tramp-own-remote-path) nil)
	  (add-to-list 'tramp-remote-path "~/.asdf/shims/"))

(use-package transient-posframe
  :ensure t
  :hook (magit-status-mode . transient-posframe-mode))

(use-package wgrep
  :ensure t
  :custom (wgrep-auto-save-buffer t))

(use-package which-key
  :ensure t
  :custom (which-key-idle-delay 0.25)
  :hook (after-init . which-key-mode))

(use-package which-key-posframe
  :ensure t
  :config (custom-set-faces '(which-key-posframe-border ((t nil))))
  :custom
  (which-key-posframe-border-width  20)
  (which-key-posframe-poshandler 'posframe-poshandler-window-top-center-offset)
  (which-key-posframe-parameters `((alpha . 90)))
  :hook (after-init . which-key-posframe-mode))

(use-package writeroom-mode
  :ensure t
  :hook ((devdocs-mode . writeroom-mode))
  :custom (writeroom-width 100))
