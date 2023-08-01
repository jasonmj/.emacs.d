(use-package cape
  :ensure t
  :bind (("C-c q" . completion-at-point))
  :hook (shell-mode . (lambda () (define-key shell-mode-map (kbd "C-r") 'cape-history)))
  :config
  (defun cape-history (&optional interactive)
    "Complete from Eshell, Comint or minibuffer history.
     See also `consult-history' for a more flexible variant based on
     `completing-read'.  If INTERACTIVE is nil the function acts like a Capf."
    (interactive (list t))
    (if interactive
	(cape-interactive #'cape-history)
      (let (history bol)
	(cond
	 ((derived-mode-p 'eshell-mode)
	  (setq history eshell-history-ring
		bol (save-excursion (eshell-bol) (point))))
	 ((derived-mode-p 'comint-mode)
	  (setq history comint-input-ring
	    bol (save-excursion (comint-bol) (point))))
	 ((and (minibufferp) (not (eq minibuffer-history-variable t)))
	  (setq history (symbol-value minibuffer-history-variable)
		bol (line-beginning-position))))
	(when (ring-p history)
	  (setq history (ring-elements history)))
	(when history
	  `(,bol ,(point)
		 ,(cape--table-with-properties (delete-dups history) :sort nil)
		 ,@cape--history-properties)))))
  (add-hook 'shell-mode-hook (lambda ()
			       (setq-local completion-at-point-functions
					   (list (cape-capf-buster #'cape-history)
						 #'cape-dabbrev
						 #'cape-file))))

  :init
  (add-to-list 'completion-at-point-functions #'comint-completion-at-point)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

(use-package consult
  :ensure t
  :bind (("C-c h" . consult-history)
	 ("C-c l" . consult-theme)
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
	 ;; ("M-s d" . consult-find)
	 ;; ("M-s D" . consult-locate)
	 ("s-s" . consult-ripgrep)
	 ;; ("M-s r" . consult-ripgrep)
	 ("M-s" . consult-line)
	 ;; ("M-s L" . consult-line-multi)
	 ;; ("M-s u" . consult-focus-lines)
	 ("C-x f" . find-file)
	 ("M-SPC" . project-find-file)
	 :map minibuffer-local-map
	 ("M-s" . consult-history)
	 ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-line-start-from-top t)
  (consult-preview-key 'any)
  (consult-buffer-filter '("^ " "\\` " "\\*Echo Area" "\\*Minibuf" "\\*Quail Completions" "\\*Backtrace"
			   "\\*elixir-ls" "Flymake log" "Shell command output" "direnv" "\\*scratch" "Shell:"
			   "\\*Messages" "\\*Warning" "*magit-" "magit-process" "*vterm" "vterm" "^:"
			   "*straight-" "*elfeed-log" "*trace of SMTP session" "\\*Compile-Log" "\\*blamer"
			   "*format-all-error" "*Async-" "COMMIT_EDITMSG" "shell: " "\\*ednc-log" "TAGS"
			   "*lsp-" "*EGLOT" "*pyls" "*vc" "*citre-ctags*" "*flycheck-posframe-buffer*" "*xob*"))

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

  ;; Consult Mark
  (key-seq-define-global "o0" 'consult-mark)

  ;; Switch buffer
  (defun my/buffer-switch ()
				      (interactive)
				      (if (project-current)
					  (consult-project-buffer)
					(consult-buffer)))
  (emacs-set-key (kbd "C-SPC") 'my/buffer-switch)
  (key-seq-define-global "cz" 'execute-extended-command)

  (defun consult-preview-posframe-focus ()
    ;; (posframe-delete-all)
    ;; (vertico-posframe--handle-minibuffer-window)
    ;; (run-with-idle-timer 2 nil (lambda ()
    ;; 				 (let* ((frame (vertico-posframe--show vertico-posframe--buffer (point)))
    ;; 					(win (car (window-list frame)))
    ;; 					(buffer (window-buffer win)))
    ;; 				   ;; (select-frame-set-input-focus frame)
    ;; 				   ;; (select-window win)
    ;; 				   (set-buffer buffer)
    ;; 				   (switch-to-buffer buffer)
    ;; 				   )))
    )
  (add-hook 'consult-after-jump-hook 'consult-preview-posframe-focus)

  ;; Configure previews
  (consult-customize consult-recent-file :preview-key nil
		     consult-theme :preview-key nil
		     consult-project-buffer :preview-key nil
		     ;; consult-ripgrep :preview-key nil
		     ;; consult-buffer :preview-key nil
		     my/buffer-switch :preview-key nil))

(defun consult-line-at-point ()
  (interactive)
  (consult-line (selection-or-thing-at-point)))
(key-seq-define-global "vf" 'consult-line-at-point)
(key-seq-define-global "vd" 'consult-line-at-point)

(defun consult-ripgrep-at-point ()
  (interactive)
  (consult-ripgrep nil (selection-or-thing-at-point)))
(emacs-set-key (kbd "S-SPC") 'consult-ripgrep-at-point)

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
  :bind (:map corfu-map ("C-e" . corfu-complete))
  :init
  (setq corfu-auto-prefix 2
	corfu-auto-delay 0.15
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
  :config
  (vertico-mode)
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

  ;; Enable vertico-multiform
  ;; (vertico-multiform-mode)
  ;; (define-minor-mode vertico-disabled-mode
  ;; "Disable vertico."
  ;; :global t :group 'vertico
  ;; (cond
  ;;  (vertico-disabled-mode
  ;;   (advice-add 'vertico--setup :around #'ignore))
  ;;  (t
  ;;   (advice-remove 'vertico--setup #'ignore))))
  ;; (setq vertico-multiform-commands
  ;; 	'((consult-project-buffer disabled)
  ;; 	  (t posframe)))

  (defun vertico-buffer--redisplay (win)
    "Redisplay window WIN."
  (when-let (mbwin (active-minibuffer-window))
    (when (eq (window-buffer mbwin) (current-buffer))
      (unless (eq win mbwin)
	(setq-local truncate-lines (< (window-point win)
				      (* 0.8 (window-width win))))
	(set-window-point win (point))
	(set-window-hscroll win 0))
      (when (and vertico-buffer-hide-prompt
		 (not (frame-root-window-p mbwin)))
	(window-resize mbwin (- (window-pixel-height mbwin)) nil nil 'pixelwise)
	(set-window-vscroll mbwin 100))
      (let ((old cursor-in-non-selected-windows)
	    (new (and (eq (selected-window) mbwin)
		      (if (memq cursor-type '(nil t)) 'hbar cursor-type))))
	(unless (eq new old)
	  (setq-local cursor-in-non-selected-windows new)
	  (force-mode-line-update t)))))))

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
    (when (vertico-quick-jump) (embark-act arg))))

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
  :init (vertico-posframe-mode 1)
  :config
  (setq vertico-posframe-border-width 20
	vertico-posframe-hide-minibuffer t
	vertico-posframe-min-width 80
	vertico-posframe-height nil
	vertico-posframe-min-height 10
	vertico-posframe-width 110
	vertico-posframe-poshandler #'posframe-poshandler-window-top-center-offset
	vertico-posframe-parameters '((alpha . 85)
				      (parent-frame . nil)
				      (cursor . 'hbar)
				      (left-fringe . 0)
				      (right-fringe . 0)))

  (custom-set-faces `(vertico-posframe-border ((t (:background nil)))))

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
  (defun vertico-posframe--handle-minibuffer-window ()
      "Handle minibuffer window."
      (let ((show-minibuffer-p (vertico-posframe--show-minibuffer-p))
	    (minibuffer-window (active-minibuffer-window)))
	(setq-local max-mini-window-height 1)
	;; Let minibuffer-window's height = 1
	(when-let* ((win (active-minibuffer-window))
		    ((not (frame-root-window-p win))))
	  (window-resize minibuffer-window
			 (- (window-pixel-height minibuffer-window))
			 nil nil 'pixelwise))
	;; Hide the context showed in minibuffer-window.
	(set-window-vscroll minibuffer-window 100)
	(when show-minibuffer-p
	  (set-window-vscroll minibuffer-window 0)))))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)
