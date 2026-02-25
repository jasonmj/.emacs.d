(use-package eshell
  :bind (:map eshell-mode-map
		("C-l" . eshell-clear)
		("<M-tab>" . tab-bar-switch-to-next-tab))
  :config
  (require 'em-smart)
  (require 'em-tramp)
  (defun eshell-clear ()
    (interactive)
    (let ((eshell-buffer-maximum-lines 0))
	(eshell-truncate-buffer)
	(previous-line)
	(delete-char 1)))
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  :custom 
  (eshell-banner-message "")
  (eshell-visual-commands '("bat" "less" "more" "htop" "man" "vim" "fish"))
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-cmpl-autolist t)
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
  (eshell-history-size 10000)
  :hook ((eshell-mode-hook . (lambda ()
				 (setq eshell-prefer-lisp-functions t
				       password-cache t
				       password-cache-expiry 900)
				 (setq-local truncate-lines -1)
				 (setenv "TERM" "xterm-256color")))))

(use-package eshell-prompt-extras :ensure t
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
	    eshell-prompt-function 'epe-theme-lambda)))

(use-package eshell-syntax-highlighting
  :ensure t
  :after eshell-mode
  :hook (eshell-mode . eshell-syntax-highlighting-global-mode))

(defun eshell-buffer (buffer-name)
  (let* ((eshell-buffer-exists (member buffer-name
					 (mapcar (lambda (buf)
						   (buffer-name buf))
						 (buffer-list)))))
    (if eshell-buffer-exists
	  (switch-to-buffer buffer-name)
	(progn
	  (eshell 99)
	  (rename-buffer (concat "Eshell: <" buffer-name ">"))))))

(defun eshell-with-name ()
  (interactive)
  (let* ((eshell-buffers (seq-filter (lambda (buf) (eq (with-current-buffer buf major-mode) 'eshell-mode))
				       (buffer-list)))
	   (eshell-buffer-names (mapcar (lambda (buf)
					  (buffer-name buf))
					eshell-buffers))
	   (buffer-name (completing-read "Eshell buffers: " eshell-buffer-names)))
    (eshell-buffer buffer-name)))
(emacs-set-key (kbd "M-`") 'eshell-with-name)

(use-package xterm-color
  :ensure t
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  :custom
  (eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  :hook ((eshell-mode . (lambda () (setenv "TERM" "xterm-256color")))
	   (eshell-before-prompt . (lambda () (setq xterm-color-preserve-properties t)))))

(use-package shell
  :bind (:map shell-mode-map
		("C-l" . clear-shell-buffer)
		("C-1" . popper-shell-fullscreen)
		("C-d" . hungry-delete-forward))
  :custom (shell-file-name (if (eq system-type 'darwin) "/opt/homebrew/bin/bash" "/run/current-system/sw/bin/bash"))
  :config
  (defun clear-shell-buffer () (interactive)
	 (erase-buffer)
	 (comint-send-input)
	 (deactivate-mark)
	 (sleep-for 0.05)
	 (delete-region 1 (pos-bol))
	 (end-of-line)
	 (deactivate-mark))

  (defun maybe-setup-project-shell () (interactive)
    (if (and (project-current) (file-exists-p "shell.nix")) (send-to-project-shell "echo \"nix develop\" && nix develop") (clear-shell-buffer)))
  (defvar shell-outline-regexp ".*\\([0-9]+\) test\\)\\([[:space:]]\\|(\\)")
  :hook ((shell-mode . (lambda ()
                         (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
			 (setq-local outline-regexp shell-outline-regexp)
                         (local-set-key "<return>" 'comint-send-input)
                         (local-set-key "C-e" 'move-end-of-line)
			   (compilation-shell-minor-mode)
			   (run-with-idle-timer 0.5 nil 'pcomplete-shell-setup)
			   (run-with-idle-timer 0.5 nil 'bash-completion-setup)
                           (maybe-setup-project-shell)))))

(defun clean-compilation-filename (filename)
  (string-trim
   (replace-regexp-in-string "\\(\*\* \\|┃\\)" ""
     (replace-regexp-in-string "\([^\"]+?\)" ""
       (replace-regexp-in-string "^([a-zA-Z0-9]+)" ""
                                 (string-trim filename))))))

(defun compilation-find-file-fixer (orig-fun marker filename &rest args)
  (message (clean-compilation-filename filename))
  (apply orig-fun marker
	   (clean-compilation-filename filename)
	   args))
(advice-add 'compilation-find-file :around #'compilation-find-file-fixer)

(defun return-to-shell-mode () (interactive) (with-current-buffer (current-buffer) (shell-mode)))

(defun cape--iex-input-filter (input)
  (if (cape--iex-starts-with-iex input)
    (set-process-filter (current-buffer-process) 'cape--iex-bootstrap-filter)))
(defvar-local default-capfs nil "Backup of buffer-local completion-at-point-functions for cape-iex.")
(defun current-buffer-process () (get-buffer-process (current-buffer)))

(defun cape--iex-starts-with-iex (str)
    (string-match-p "iex" (s-trim (ansi-color-filter-apply str))))

(defun cape--iex-bootstrap-filter (proc output)
  (let ((lines (split-string output "\n")))
    (mapcar (lambda (line) (if (cape--iex-starts-with-iex line) (cape--iex-setup proc))) lines)
    (comint-output-filter proc output))
  )

(defun cape--iex-output-filter (proc output)
  (with-current-buffer (get-buffer-create "*tmp*") (insert (ansi-color-filter-apply output)))
  (cape--iex-maybe-restore-output-filter proc output))

(defun cape--iex-maybe-restore-output-filter (proc output)
  (mapcar (lambda (line)
	      (if (cape--iex-starts-with-iex line)
		  (set-process-filter proc 'comint-output-filter)
		nil))
	    (string-split output "\n")))

(defun cape--iex-restore-output-filter ()
  (interactive)
  (set-process-filter (current-buffer-process) 'comint-output-filter))

(defun cape--iex-setup (proc)
  (message "setting up iex autocompletion...")
  (advice-add #'comint-quit-subjob :after #'cape--iex-teardown)
  (set-process-filter proc #'cape--iex-maybe-restore-output-filter)
  (process-send-string proc "Process.put(:evaluator, IEx.Server.start_evaluator(1, []))\n")
  (setq-local default-capfs completion-at-point-functions)
  (setq-local completion-at-point-functions (cons #'cape-iex completion-at-point-functions)))

(defun cape--iex-teardown ()
  (set-process-filter (current-buffer-process) 'comint-output-filter)
  (if default-capfs (setq-local completion-at-point-functions default-capfs))
  (advice-remove #'comint-quit-subjob #'restore-default-shell-capfs))

(defun cape--iex-autocomplete (proc expr)
  (let* ((suffix "\" |> String.to_charlist() |> Enum.reverse() |> IEx.Autocomplete.expand(self()) |> (case do: ({:yes, [], x} -> Enum.map(x, &to_string/1); {:yes, x, _} -> [to_string(x)]; _ -> to_string(nil);))\n")
	   (cmd (concat "\"" expr suffix)))
    (set-process-filter proc 'cape--iex-output-filter)
    (process-send-string proc cmd)
    (sleep-for 0.1)
    (with-current-buffer (get-buffer-create "*tmp*") (cape--iex-build-completions (buffer-string)))))

(defun cape--iex-build-completions (buffer-str)
  (let* ((separator (if (eq system-type 'darwin) "\n" "\n"))
	   (strs (butlast (split-string buffer-str separator)))
	   (str (if (eq (length strs) 1) (car strs) (string-join (cdr strs))))
	   (substr (if (< (length str) 4) str (substring str 2 -2)))
	   (completions (delete-dups (split-string substr "\", \"")))
	   (cands (mapcar (lambda (completion)
			    (if (length= completion 0) nil (cape--iex-format-candidate expr completion))) completions)))
    (prescient-sort cands)))

(defun cape--iex-get-candidate-annotation (str)
  (let* ((last-char (substring str -1))
	  (last-node (cape--iex-last-node str))
	  (last-node-first-char (if (length< last-node 1) "" (substring last-node nil 1))))
    (cond
     ((equal last-node-first-char (upcase last-node-first-char)) "alias")
     ((equal last-node-first-char (downcase last-node-first-char)) "function")
     (t "IEx"))))

(defun cape--iex-get-candidate-kind (str)
  (let* ((last-char (substring str -1))
	  (last-node (cape--iex-last-node str))
	  (last-node-first-char (if (length< last-node 1) "" (substring last-node nil 1))))
    (cond
     ((equal last-node-first-char (upcase last-node-first-char)) 'snippet)
     ((equal last-node-first-char (downcase last-node-first-char)) 'function)
     (t 'text))))

(defun cape--iex-first-node (str)
  (car (split-string (concat str "") "\\.")))

(defun cape--iex-last-node (str)
  (car (last (split-string (concat str "") "\\."))))

(defun cape--iex-format-candidate (expr completion)
  (let* ((clean-completion (cape--iex-clean-up-completion completion))
	  (first-char (substring completion nil 1))
	  (last-char (substring completion -1))
	  (combined (string-merge expr clean-completion)))
    (cond
     ((equal completion ".") (concat expr clean-completion))
     ((equal first-char (upcase first-char)) (concat combined "."))
     ((and (equal first-char (downcase first-char))
	     (not (eq (string-match-p "^[0-9]+$" last-char) nil))) (concat (substring combined nil -2) "("))
     (t combined))))

(defun cape--iex-clean-up-completion (completion)
  (replace-regexp-in-string "\"\"" "" (replace-regexp-in-string "\\\\" "" (replace-regexp-in-string "\"\"" "" (replace-regexp-in-string "x>" "" completion nil t) nil t) nil t) nil t))

(defun cape-iex ()
  (when-let ((proc (get-buffer-process (current-buffer)))
	       (start (process-mark proc))
	       (end (point))
	       (expr (buffer-substring-no-properties start end)))
    `(,start ,end
	    ,(completion-table-dynamic
	     (lambda (_)
	       (when-let ((proc (get-buffer-process (current-buffer)))
			  (expr (buffer-substring-no-properties (process-mark proc) (point)))
			  (result (while-no-input (cape--iex-autocomplete proc expr))))
		 (when (get-buffer "*tmp*") (kill-buffer "*tmp*"))
		 (and (consp result) result))))
	    :exclusive 'no
	    :company-kind cape--iex-get-candidate-kind
	    :annotation-function (lambda (s) (concat " " (cape--iex-get-candidate-annotation s))))))

(defun string-merge (str1 str2)
  (let* ((first-node (cape--iex-first-node str1))
	   (last-node (cape--iex-last-node str1))
	   (last-char (substring str1 -1 nil))
	   (zipped (-zip-pair (split-string str2 "") (split-string last-node "")))
	   (combined (concat str1 (substring str2 (- (length zipped) 2)))))
    (cond
     ((and (equal first-node last-node)
	     (not (equal str1 str2))
	     (string-match-p (regexp-quote str1) str2)) str2)
     ((and (equal first-node last-node)
	     (not (equal str1 str2))) (concat str1 str2))
     ((equal last-char ".") (concat str1 str2))
     ((not (string-match-p (regexp-quote last-node) str2)) (concat str1 str2))
     (t combined))))

(defun strip-ansi-chars (str)
  (let ((clean-str (ansi-color-apply str)))
    (set-text-properties 0 (length clean-str) nil clean-str)
    clean-str))

(add-to-list 'comint-input-filter-functions 'cape--iex-input-filter)

(defun shell-buffer (buffer-name)
  (let* ((shell-buffer-exists (member buffer-name
					(mapcar (lambda (buf) (buffer-name buf))
						(buffer-list)))))
    (if shell-buffer-exists
	  (switch-to-buffer buffer-name)
	(progn
	  (shell "tmp")
	  (rename-buffer (concat "Shell: <" buffer-name ">"))))))

(defun shell-with-name ()
  (interactive)
  (let* ((shell-buffers (seq-filter (lambda (buf) (eq (with-current-buffer buf major-mode) 'shell-mode))
				       (buffer-list)))
	   (shell-buffer-names (mapcar (lambda (buf)
					  (buffer-name buf))
					shell-buffers))
	   (buffer-name (completing-read "Shell buffers: " shell-buffer-names)))
    (shell-buffer buffer-name)))
(emacs-set-key (kbd "C-\\") 'shell-with-name)

(defun send-to-project-shell (command)
    "Send COMMAND to the shell buffer for the current project."
    (let* ((project (project-current t))
           (default-directory (if project (project-root project) default-directory))
           (shell-buf (or (get-buffer "*shell*")
                          (and (fboundp 'project-shell) (get-buffer (format "*shell*<%s>" (project-name project)))))))
      (unless (and shell-buf (buffer-live-p shell-buf))
        (setq shell-buf (if (fboundp 'project-shell)
                            (project-shell)
                          (shell "*shell*"))))
      (with-current-buffer shell-buf
        (goto-char (point-max))
        (comint-send-string shell-buf (concat command "\n")))))

(use-package sticky-shell
  :straight (:type git :host github :repo "andyjda/sticky-shell")
  :hook (shell-mode . sticky-shell-mode)
  :config
  (defun clear-shell-buffer-to-last-prompt () (interactive)
	 (end-of-buffer)
	 (set-mark (point))
	 (comint-previous-prompt 1)
	 (end-of-line)
	 (forward-char)
	 (delete-active-region))
  (defun clear-shell-buffer-to-last-prompt-leave-n (n) (interactive)
	 (end-of-buffer)
	 (set-mark (point))
	 (comint-previous-prompt 1)
         (next-line n)
	 (end-of-line)
	 (forward-char)
	 (delete-active-region))
  :bind (:map shell-mode-map
		("C-S-l" . clear-shell-buffer-to-last-prompt)))

(defun syntax-overlay-region ()
  (interactive)
  (unless (region-active-p)
    (user-error "No region active"))
  (let* ((lang-mode 'elixir-mode)
	   (body-start (region-beginning))
	   (body-end (region-end))
	   (string (buffer-substring-no-properties body-start body-end))
	   (buf (current-buffer))
	   (pos 0)
	   (props)
	   (overlay)
	   (propertized-text))
    (if (fboundp lang-mode)
	  (progn
	    (setq propertized-text
		  (with-current-buffer
		      (get-buffer-create
		       (format " *fontification:%s*" lang-mode))
		    (let ((inhibit-modification-hooks nil)
			  (inhibit-message t))
		      (erase-buffer)
		      ;; Additional space ensures property change.
		      (insert string " ")
		      (funcall lang-mode)
		      (font-lock-ensure))
		    (buffer-string)))
	    (while (< pos (length propertized-text))
	      (setq props (text-properties-at pos propertized-text))
	      (setq overlay (make-overlay (+ body-start pos)
					  (+ body-start (1+ pos))
					  buf))
	      (overlay-put overlay 'face (plist-get props 'face))
	      (setq pos (1+ pos))))
	(message "%s not found" lang-mode))))

(defun popper-shell-fullscreen ()
  (interactive)
  (let* ((name (buffer-name)))
    (delete-window)
    (switch-to-buffer name)))
(emacs-set-key (kbd "C-x c") 'popper-shell-fullscreen)

(use-package comint
  :ensure nil   ;; built-in
  :bind
  (:map comint-mode-map ("<return>" . comint-send-input)))

(add-to-list 'load-path (concat "~/.local/vterm/" (string-trim (shell-command-to-string "ls ~/.local/vterm/"))))
(add-to-list 'load-path (concat "/etc/links/vterm/" (string-trim (shell-command-to-string "ls /etc/links/vterm/"))))
(require 'vterm)
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
(defun vterm-startup ()
  (define-key vterm-mode-map (kbd "C-c C-t") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-p") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-c C-\\") 'vterm-send-C-c)
  (define-key vterm-mode-map (kbd "M-p") 'vterm-send-up)
  (define-key vterm-mode-map (kbd "M-n") 'vterm-send-down)
  (define-key vterm-mode-map (kbd "C-z") 'vterm-undo)
  (define-key vterm-copy-mode-map (kbd "M-n") 'vterm-next-prompt)
  (define-key vterm-copy-mode-map (kbd "C-z") 'vterm-undo)
  (define-key vterm-copy-mode-map (kbd "M-p") 'vterm-previous-prompt)
  (define-key vterm-copy-mode-map (kbd "C-l") (lambda () (interactive) (vterm-copy-mode -1) (vterm-clear)))
  (with-eval-after-load 'centered-cursor-mode
        (add-hook 'after-change-major-mode-hook
            (lambda ()
              (centered-cursor-mode 0))
            :append
            :local))
  (setq-local global-hi-lock-mode nil)
  (setq-local global-hl-line-mode nil))
(add-hook 'vterm-mode-hook 'vterm-startup)

(use-package vterm-toggle
  :ensure t
  :config
  (emacs-set-key (kbd "C-s-t") 'vterm-toggle))
