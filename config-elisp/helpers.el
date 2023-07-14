(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :bind (("C-s-SPC" . app-launcher-run-app)))

(use-package blamer
  :ensure t
  :bind (("M-I" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-type 'both)
  (blamer-show-avatar-p nil)
  (blamer-idle-time 2)
  (blamer-min-offset 70)
  (blamer-posframe-configurations `(:left-fringe 20 :right-fringe 20 :y-pixel-offset 20 :x-pixel-offset -20 :border-width 1 :border-color ,(face-attribute 'default :foreground) :lines-truncate t :accept-focus nil))
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
		   :background "unspecified"
		   :italic t)))
  :config
  (setq blamer--regexp-info
   (concat "^(?\\(?1:[^ ]*\\) [^ ]*[[:blank:]]?\(\\(?2:.*\\)"
	   "\s\\(?3:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
	   "\s\\(?4:[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"))
  (with-current-buffer (get-buffer-create blamer--buffer-name) (face-remap-add-relative 'fringe :background (face-attribute 'default :background) :foreground (face-attribute 'default :background)))
  (global-blamer-mode 1))

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
  :bind (:map shell-mode-map
	 ("C-<return>" . expand-and-fold-this)))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)))

(global-hl-line-mode +1)

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
