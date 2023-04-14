(use-package cape
  :ensure t
  :bind (("C-c q" . completion-at-point))
  :init
  (add-to-list 'completion-at-point-functions #'comint-completion-at-point)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

(use-package consult
  :ensure t
  :bind (("C-c h" . consult-history)
	 ("C-;" . consult-recent-file)
	 ("s-SPC" . consult-buffer)
	 ("C-x B" . consult-bookmark)
	 ("C-SPC" . consult-project-buffer)
	 ("M-y" . consult-yank-pop)
	 ("M-g g" . consult-goto-line)
	 ("M-g o" . consult-outline)
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("s-s" . consult-ripgrep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s u" . consult-focus-lines)
	 ("C-x f" . find-file)
	 ("M-SPC" . project-find-file)
	 :map shell-mode-map
	 ("C-r" . consult-history)
	 :map minibuffer-local-map
	 ("M-s" . consult-history)
	 ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-preview-key 'any)
  (consult-buffer-filter '("^ " "\\` " "\\*Echo Area" "\\*Minibuf" "\\*Quail Completions"
			   "\\*elixir-ls" "Flymake log" "Shell command output" "direnv" "\\*scratch"
			   "\\*Messages" "\\*Warning" "*magit-" "magit-process" "*vterm" "vterm" "^:"
			   "*straight-" "*elfeed-log" "*trace of SMTP session" "\\*Compile-Log" "\\*blamer"
			   "*format-all-error" "*Async-" "COMMIT_EDITMSG" "shell: " "\\*ednc-log" "TAGS"
			   "*lsp-" "*EGLOT" "*pyls" "*vc" "*citre-ctags*"))

  :config
  ;; Consult-yank-pop
  (defun my/consult-yank-pop (orig-fun &rest args)
    (interactive "p")
    (if (equal major-mode 'exwm-mode)
	(let ((inhibit-read-only t))
	  (cl-letf (((symbol-function 'insert-for-yank)
		     (lambda (str) (kill-new str)
		       (exwm-input--fake-key ?\C-v))))
	    (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'consult-yank-pop :around #'my/consult-yank-pop)

  ;; Find File
  (key-seq-define-global "xf" 'find-file)

  ;; Switch buffer
  (exwm-input-set-key (kbd "C-SPC") (lambda ()
				      (interactive)
				      (if (project-current)
					  (consult-project-buffer)
					(consult-buffer))))
  (key-seq-define-global "cz" 'execute-extended-command)

  ;; Configure previews
  (consult-customize consult-recent-file :preview-key 'nil
		     consult-theme :preview-key '(:debounce 0.5 any)
		     consult-project-buffer :preview-key 'nil
		     consult-buffer :preview-key 'nil))

(defun consult-line-at-point ()
  (interactive)
  (consult-line (selection-or-thing-at-point)))
(key-seq-define-global "vf" 'consult-line-at-point)
(key-seq-define-global "vd" 'consult-line-at-point)

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

(require 'request)

(cl-defun consult-web--handle-error (&rest args &key error-thrown &allow-other-keys)
  "Handle error from `request' with ARGS.

Display a message with the ERROR-THROWN."
  (error "Web search error: %S" error-thrown))

(defun consult-web--request (url parser &optional placeholder)
  "Search using the given URL and PARSER.

PLACEHOLDER is returned for immediate display by `completing-read'.  The
actual list of candidates is later updated by the \:success
function."
  (let (candidates)
    (request
      url
      :sync t
      :headers '(("User-Agent" . "Emacs"))
      :parser parser
      :error #'consult-web--handle-error
      :success (cl-function (lambda (&key data &allow-other-keys)
			      (setq candidates data))))
    candidates))

(defun consult-web--format-candidate (text url)
  "Format TEXT and URL as an `completing-read' candidate."
  (let ((url (url-unhex-string url)))
    (propertize (concat text "\n" (propertize url 'face 'shadow)) 'shr-url url)))

(defun consult-web-search--duckduckgo (string)
  "Retrieve search results from DuckDuckGo for STRING."
  (consult-web--request
   (concat "https://duckduckgo.com/html/?q=" (url-hexify-string string))
   (lambda ()
     (mapcar
      (lambda (a)
	(let* ((href (assoc-default 'href (dom-attributes a))))
	  (consult-web--format-candidate
	   (dom-texts a)
	   ;; DDG sometimes appends "&rut...", which I can only guess is an
	   ;; anti-bot measure. See https://github.com/mnewt/counsel-web/issues/3.
	   (substring href (string-match "http" href) (string-match "&rut=" href)))))
      (dom-by-class (libxml-parse-html-region (point-min) (point-max)) "result__a")))
   "Searching DuckDuckGo..."))

(defun consult-web-search ()
  "Search the web with Consult."
  (interactive)
  (let* ((string (read-string "Web Search: " nil nil))
	 (results (consult-web-search--duckduckgo string))
	 (selection (completing-read "Results: " results)))
    (browse-url (car (cdr (split-string selection "\n"))))))

(defun consult-web-thing-at-point ()
  "Interactively search the web for the THING at point."
  (interactive)
  (counsel-web-search (selection-or-thing-at-point)))

(use-package corfu
  :ensure t
  :init
  (setq corfu-auto-prefix 2
	corfu-auto-delay 0.35
	corfu-auto t
	corfu-cycle t
	corfu-quit-no-match t
	corfu-preselect 'first
	corfu-scroll-margin 5)
  (corfu-indexed-mode 1)
  (corfu-history-mode 1)
  (savehist-mode t)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (setq corfu-indexed-start 1)

  ;; Customize corfu--affixate to exclude space after index
  (cl-defmethod corfu--affixate :around (cands &context (corfu-indexed-mode (eql t)))
    (setq cands (cdr (cl-call-next-method cands)))
    (let* ((space #(" " 0 1 (face (:height 0.5 :inherit corfu-indexed))))
	   (width (if (length> cands (- 10 corfu-indexed-start)) 2 1))
	   (fmt (concat space
			(propertize (format "%%%ds" width)
				    'face 'corfu-indexed)
			space))
	   (align
	    (propertize (make-string width ?\s)
			'display
			`(space :align-to (+ left ,(1+ width))))))
      (cl-loop for cand in cands for index from corfu-indexed-start do
	       (setf (cadr cand)
		     (concat
		      (propertize " " 'display (format fmt index))
		      (cadr cand)
		      align)))
      (cons t cands)))

  ;; Completion in the minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
	  completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  ;; Insert indexed candidate without needing to press enter
  (defun corfu-indexed-insert (i)
    (setq corfu--index (- i 1))
    (call-interactively #'corfu-insert))
  (loopy-iter
   (with (map corfu-map))
   (numbering i :from 1 :to 9)
   (define-key map (kbd (format "s-%d" i)) `(lambda () (interactive) (corfu-indexed-insert ,i))))
  (global-corfu-mode))

(defun corfu-send-shell (&rest _)
  "Send completion candidate when inside comint/eshell."
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
    (comint-send-input))))

(advice-add #'corfu-insert :after #'corfu-send-shell)
;; Customize Emacs for Corfu usage
(use-package emacs
  :custom
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :custom (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   :map embark-general-map
   ("O" . syntax-overlay-region)
   ("W" . consult-web-search)
   :map embark-region-map
   ("O" . syntax-overlay-region)
   ("W" . consult-web-search))
  :custom (prefix-help-command . #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.6))  
  (kind-icon-extra-space t)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package marginalia
  :ensure t
  :custom (marginalia-field-width 60)
  :config (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package prescient
  :ensure t
  :demand t
  :commands prescient-persist-mode
  :custom (prescient-save-file (expand-file-name "cache/prescient-save.el" user-emacs-directory))
  :config (prescient-persist-mode))

(use-package vertico-prescient
  :ensure t
  :after prescient vertico
  :custom (vertico-prescient-completion-styles '(orderless prescient partial-completion))
  :config (vertico-prescient-mode))

(use-package corfu-prescient
  :ensure t
  :after prescient corfu
  :config (corfu-prescient-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-quick
  :after vertico
  :bind (:map vertico-map
         ("M-i" . vertico-quick-insert)
         ("C-'" . vertico-quick-exit)
         ("C-o" . vertico-quick-embark))
  :config
  (defun vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg))))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

(use-package vertico-posframe
  :ensure t
  :config
  (setq vertico-posframe-border-width 20
	vertico-posframe-hide-minibuffer t
	vertico-posframe-min-width 80
	vertico-posframe-height nil
	vertico-posframe-min-height 10
	vertico-posframe-width 110
	vertico-posframe-poshandler #'posframe-poshandler-window-top-center-offset
	vertico-posframe-parameters '((alpha-background . 85)
				      (parent-frame . nil)
				      (cursor-type . 'bar)
				      (left-fringe . 0)
				      (right-fringe . 0)))
  (defun posframe-poshandler-window-top-center-offset (info)
    "Posframe's position handler.

       Get a position which let posframe stay onto current window's
       top center.  The structure of INFO can be found in docstring of
       `posframe-show'."
    (setq-local tab-line-format nil)
    (let* ((window-left (plist-get info :parent-window-left))
	   (window-top (plist-get info :parent-window-top))
	   (window-width (plist-get info :parent-window-width))
	   (posframe-width (plist-get info :posframe-width)))
      (cons (+ window-left (/ (- window-width posframe-width) 2))
	    (+ window-top 64))))
  (vertico-indexed-mode 1)
  (setq vertico-indexed-start 1)
  (defun vertico-indexed-insert (i)
    (setq vertico--index (- i 1))
    (call-interactively #'vertico-insert)
    (call-interactively #'vertico-exit))
 (loopy-iter
   (with (map vertico-map))
   (numbering i :from 1 :to 9)
   (define-key map (kbd (format "s-%d" i)) `(lambda () (interactive) (vertico-indexed-insert ,i))))
  (custom-set-faces '(vertico-posframe-border ((t nil))))
  (vertico-posframe-mode 1))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)