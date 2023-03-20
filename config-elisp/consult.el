;; Example configuration for Consult
(use-package consult
  :ensure t
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
	 ("C-;" . consult-recent-file)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("s-SPC" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-SPC" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.

  :config
  ;; Find File
  (key-seq-define-global "xf" 'find-file)
  (global-set-key (kbd "M-SPC") 'project-find-file)
  
  ;; Switch buffer
  (exwm-input-set-key (kbd "C-SPC") (lambda ()
                                    (interactive)
                                    (if (project-current)
                                        (consult-project-buffer)
                                        (consult-buffer))))
  (key-seq-define-global "cz" 'execute-extended-command)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  (consult-customize consult-recent-file :preview-key 'nil
                     consult-theme :preview-key '(:debounce 0.5 any)
		     consult-project-buffer :preview-key 'nil
		     consult-buffer :preview-key 'nil)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Buffer filtering
  (setq consult-buffer-filter '("^ " "\\` " "\\*Echo Area" "\\*Minibuf" "\\*Quail Completions"
				"\\*elixir-ls" "Flymake log" "Shell command output" "direnv" "\\*scratch"
                                "\\*Messages" "\\*Warning" "*magit-" "magit-" "*vterm" "vterm" "^:" "*Occur"
                                "*straight-" "*elfeed-log" "*trace of SMTP session" "\\*Compile-Log"
                                "*format-all-error" "*Async-" "COMMIT_EDITMSG"
                                "*lsp-" "*EGLOT" "*pyls")))

(defun consult-line-at-point ()
  "Custom function to pick up a thing at a point for consult-line

If a selected region exists, it will be searched for by consult-linee
If there is a symbol at the current point, its textual representation is
searched. If there is no symbol, empty search box is started."
  (interactive)
  (consult-line (selection-or-thing-at-point)))
(key-seq-define-global "vf" 'consult-line-at-point)

(defun consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep nil (selection-or-thing-at-point)))
(exwm-input-set-key (kbd "S-SPC") 'consult-ripgrep-at-point)

(defun selection-or-thing-at-point ()
  (cond
   ;; If there is selection use it
   ((and transient-mark-mode
         mark-active
         (not (eq (mark) (point))))
    (let ((mark-saved (mark))
          (point-saved (point)))
      (deactivate-mark)
      (buffer-substring-no-properties mark-saved point-saved)))
   ;; Otherwise, use symbol at point or empty
   (t (format "%s"
              (or (thing-at-point 'symbol)
                  "")))))
