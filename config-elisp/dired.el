(use-package dired
  :bind (("s-d" . (lambda () (interactive) (dired "~/downloads")))
	   :map dired-mode-map
	   ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (if (eq system-type 'darwin)
	(setq dired-listing-switches "-al")
    (setq dired-listing-switches "-l -A -h -v --group-directories-first"))
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  :hook ((dired-mode . auto-revert-mode)
	   (dired-mode . dired-hide-details-mode)))

(use-package all-the-icons-dired
  :ensure t
  :config
  (defun all-the-icons-dired--refresh ()
    "Display the icons of files in a dired buffer."
    (all-the-icons-dired--remove-all-overlays)
    (unless (file-remote-p default-directory)
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
	    (when (dired-move-to-filename nil)
	      (let ((file (dired-get-filename 'relative 'noerror)))
		(when file
		  (let ((icon (if (file-directory-p file)
				  (all-the-icons-icon-for-dir file
							      :face 'all-the-icons-dired-dir-face
							      :v-adjust all-the-icons-dired-v-adjust)
				(all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust))))
		    (if (member file '("." ".."))
			(all-the-icons-dired--add-overlay (point) "  \t")
		      (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
	    (forward-line 1)))))
  :hook
  (dired-mode . all-the-icons-dired-mode))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))
(defun setup-dired-open-file () (define-key dired-mode-map (kbd "S-<return>") 'dired-open-file))
(add-hook 'dired-mode-hook #'setup-dired-open-file)

(defun dired-recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((recent-dirs
	   (delete-dups
	    (mapcar (lambda (file)
		      (if (file-directory-p file) file (file-name-directory file)))
		    recentf-list))))

    (let ((dir (completing-read "Recent directory: " recent-dirs)))
	(dired dir))))
(emacs-set-key (kbd "C-x d") 'dired-recent-dirs)

(use-package dired-x
  :bind (("C-x C-d" . dired-x-find-file))
  :after dired
  :demand t
  :config
  (key-seq-define-global "xd" 'dired-x-find-file)
  ;;(require 'dired-x)
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files (concat dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$")))

(use-package direnv
  :ensure t
  :config
  (direnv-mode)

  ;; Per-directory in-flight guard: maps directory string -> live process or nil.
  (defvar my/direnv-in-flight (make-hash-table :test 'equal)
    "Hash table mapping directory strings to their live async direnv process.")

  (defun my/direnv-async-update (directory)
    "Run `direnv export json' asynchronously for DIRECTORY.
Applies the resulting environment and notifies via the echo area when done.
Skips launch if a process for DIRECTORY is already in flight.
Output is stored on the process object via `process-put' to avoid
dynamic-binding closure issues."
    (when (and directory
               (not (process-live-p (gethash directory my/direnv-in-flight))))
      (let* ((exec (or direnv--executable (direnv--detect)))
             (proc (make-process
                    :name (format "direnv-async[%s]" directory)
                    :command (list exec "export" "json")
                    :connection-type 'pipe
                    :noquery t
                    :filter (lambda (proc chunk)
                              (process-put proc 'output
                                           (concat (or (process-get proc 'output) "")
                                                   chunk)))
                    :sentinel (lambda (proc event)
                                (let ((dir (process-get proc 'directory)))
                                  (remhash dir my/direnv-in-flight)
                                  (cond
                                   ((string= event "finished\n")
                                    (condition-case err
                                        (let ((trimmed (string-trim
                                                        (or (process-get proc 'output) ""))))
                                          (when (> (length trimmed) 0)
                                            (let ((env (json-parse-string
                                                        trimmed
                                                        :object-type 'alist)))
                                              (when (> (length env) 0)
                                                (dolist (pair env)
                                                  (let ((name (car pair))
                                                        (value (cdr pair)))
                                                    (setenv name value)
                                                    (when (string= name "PATH")
                                                      (setq exec-path
                                                            (append (parse-colon-path value)
                                                                    (list exec-directory))))))
                                                (message "direnv: environment loaded (%s)"
                                                         (abbreviate-file-name dir))))))
                                      (error
                                       (message "direnv async error in %s: %s"
                                                (abbreviate-file-name dir)
                                                (error-message-string err)))))
                                   ((string-prefix-p "exited abnormally" event)
                                    (message "direnv: .envrc evaluation failed in %s"
                                             (abbreviate-file-name dir)))))))))
        (process-put proc 'directory directory)
        (puthash directory proc my/direnv-in-flight))))

  (defun my/direnv-maybe-update-async ()
    "Async replacement for `direnv--maybe-update-environment'."
    (when-let ((dir (and buffer-file-name
                         (locate-dominating-file buffer-file-name ".envrc"))))
      (my/direnv-async-update (file-truename dir))))

  (advice-add 'direnv--maybe-update-environment :override #'my/direnv-maybe-update-async)

  :custom (direnv-always-show-summary nil))

(use-package dired-single
  :ensure t
  :straight (:type git :host codeberg :repo "amano.kenji/dired-single")
  :after dired
  :bind (:map dired-mode-map
	      ("^" . (lambda nil (interactive) (dired-single-buffer "..")))
	      ("[" . (lambda nil (interactive) (dired-single-buffer "..")))
	      ("]" . dired-single-buffer)
	      ([return] . dired-single-buffer)
	      ([?\r] . dired-single-buffer)
	      ([mouse-1] . dired-single-buffer-mouse))
  :init (set (make-local-variable 'mouse-1-click-follows-link) nil))

(use-package dired-hide-dotfiles
  :ensure t
  :config
  (defun dired-toggle-dotfiles()
    (dired-hide-dotfiles-mode)
    (revert-buffer)
    (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))
  :hook (dired-mode . dired-toggle-dotfiles))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
		("<tab>" . (lambda () (interactive) (dired-subtree-toggle) (revert-buffer)))
		("<backtab>" . dired-subtree-cycle)))

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

(key-chord-define-global "wd" 'wdired-change-to-wdired-mode)

(key-seq-define-global "1`" (lambda () (interactive) (dired "~/")))
