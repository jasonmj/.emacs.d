(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq lmstudio-model "qwen2.5-7b-instruct-1m")
  (setq lmstudio-architect-model "qwen2.5-7b-instruct-1m")
  ;; (setq lmstudio-model "deepseek-coder-33b-base")
  ;; (setq lmstudio-model "mistral-nemo-instruct-2407")
  (setq aidermacs-extra-args (list "--model" (concat "lm_studio/" lmstudio-model) "--no-show-model-warnings"))
  (setq aidermacs-default-model (concat "lm_studio/" lmstudio-model))
  (setq aidermacs-architect-model (concat "lm_studio/" lmstudio-architect-model))
  (setq aidermacs-editor-model (concat "lm_studio/" lmstudio-model))
  (setq aidermacs-auto-commits nil)
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-backend 'comint)
  (aidermacs-setup-minor-mode)
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  (setenv "LM_STUDIO_API_KEY" "dummy-api-key")
  (setenv "LM_STUDIO_API_BASE" "http://localhost:1234/v1")
  :hook (comint-mode . (lambda () (if (string-match-p "*aidermacs" (buffer-name))
                                 (progn (corfu-mode -1) (olivetti-mode 1))))))

;;; llm-utils.el --- LLM helper utilities for project navigation

(require 'json)
(require 'xref)
(require 'project)
(require 'subr-x)

(defun llm-project-diff (&optional detail file)
  "Return a summary of unstaged and staged changes, or full unified diff.

DETAIL:
  nil/'summary': per-file added/removed summary (default)
  'full: full unified diff output (optionally for FILE or list of FILES)
FILE:
  Optional filename (string) or list of filenames to limit the diff scope."
  (interactive)
  (let* ((detail (or detail 'summary))
         (file-args
          (cond
           ((null file) "")
           ((listp file) (mapconcat #'shell-quote-argument file " "))
           ((stringp file) (shell-quote-argument file))
           (t "")))
         (diff-cmd (concat "git diff --unified=0 --no-color " file-args))
         (raw-diff (shell-command-to-string diff-cmd)))
    (if (eq detail 'full)
        ;; Full diff output (as string)
        raw-diff
      ;; Summary only (default): per-file line count
      (let* ((lines (split-string raw-diff "\n" t))
             (result '())
             (cur-file nil)
             (add 0)
             (rm 0))
        (dolist (l lines)
          (cond
           ;; Detect new file diff header
           ((string-match "^diff --git a/\\([^ ]+\\) b/\\([^ ]+\\)" l)
            ;; Push previous file's counts if any
            (when cur-file
              (push (list :file cur-file :added add :removed rm) result))
            (setq cur-file (match-string 2 l))
            (setq add 0 rm 0))
           ;; Added lines (ignore index/meta lines)
           ((and cur-file (string-prefix-p "+" l) (not (string-prefix-p "+++" l)))
            (cl-incf add))
           ;; Removed lines (ignore index/meta lines)
           ((and cur-file (string-prefix-p "-" l) (not (string-prefix-p "---" l)))
            (cl-incf rm))))
        ;; Push last file
        (when cur-file
          (push (list :file cur-file :added add :removed rm) result))
        (require 'json)
        (json-encode (or (nreverse result) []))))))


(defun llm-project-root ()
  "Return the root directory of the current project."
  (if-let ((proj (project-current)))
      (project-root proj)
    default-directory))

(defun llm-directory-tree (&optional dir max-depth)
  "Return a simple nested list representing the directory tree under DIR up to MAX-DEPTH.
If MAX-DEPTH is nil, default to 2. Only directory names are returned, as JSON."
  (let* ((dir (or dir (llm-project-root)))
         (max-depth (or max-depth 2)))
    (with-temp-buffer
      (let ((default-directory dir))
        (call-process "find" nil t nil "." "-type" "d" "-print"))
      (let ((dirs (split-string (buffer-string) "\n" t)))
        (json-encode (llm--simple-dir-tree dirs max-depth))))))

(defun llm--simple-dir-tree (dirs max-depth)
  "Return a nested structure with only directory names, no files or metadata.
Example: '((src (lib)) (test)) for src/lib and test dirs (DEPTH=2)."
  (let ((tree ()))
    (dolist (d dirs)
      (let* ((parts (seq-take (split-string (string-remove-prefix "./" d) "/") max-depth))
             (node tree))
        (cl-labels
            ((insert-path (parts node)
               (if (null parts)
                   node
                 (let* ((head (car parts))
                        (child (assoc head node)))
                   (unless child
                     (setq child (cons head nil))
                     (push child node))
                   (setcdr child (insert-path (cdr parts) (cdr child)))
                   node))))
          (setq tree (insert-path parts tree)))))
    ;; clean output: turn '((a (b ()))) -> {"a": {"b": {}}}
    (cl-labels ((alist-to-hash (alist)
                  (let ((tbl (make-hash-table :test #'equal)))
                    (dolist (item alist)
                      (if (consp item)
                          (puthash (car item) (alist-to-hash (cdr item)) tbl)
                        (puthash item (make-hash-table) tbl)))
                    tbl)))
      (let ((ht (alist-to-hash tree)))
        (json-read-from-string (json-encode ht))))))


(defun llm-file-preview (file &optional n)
  "Return the first N lines of FILE as a string (default 15).
Warn if N exceeds 40. Trailing whitespace and newlines are stripped."
  (let* ((n (or n 15))
         (n (if (stringp n) (string-to-number n) n)))
    (when (> n 40)
      (unless (y-or-n-p (format "Preview %d lines? This may use many tokens. Continue? " n))
        (user-error "Aborted preview of large file segment")))
    (with-temp-buffer
      (insert-file-contents file nil 0 10000)
      (goto-char (point-min))
      (let ((content (buffer-substring-no-properties
                      (point-min)
                      (progn (forward-line n) (point)))))
        (string-trim-right content)))))

(defun llm-code-outline (file-or-buffer)
  "Return a code outline for FILE-OR-BUFFER using imenu only.

  If FILE-OR-BUFFER is a buffer, use it directly. Otherwise, open the file in a temporary buffer.

  Returns a processed alist representing the code outline suitable for LLM consumption."
  (let ((buf (cond
              ((bufferp file-or-buffer) file-or-buffer)
              ((stringp file-or-buffer) (find-file-noselect file-or-buffer))
              (t (error "Expected buffer or filename string"))))
        outline)
    (with-current-buffer buf
      ;; Refresh imenu index
      (condition-case err
          (imenu--make-index-alist)
        (error
         (message "Error building imenu indices: %s" err)
         (setq imenu--index-alist nil)))
      (setq outline (llm--flatten-imenu-alist imenu--index-alist)))
    outline))

(defun llm--flatten-imenu-alist (alist)
  "Flatten the nested imenu ALIST into a clean list of (NAME . POSITION)."
  (cl-labels ((flatten (list)
                (apply #'append
                       (mapcar (lambda (item)
                                 (cond
                                  ((imenu--subalist-p item)
                                   (flatten (cdr item)))
                                  ((and (consp item) (stringp (car item)))
                                   (list item))
                                  (t nil)))
                               list))))
    (flatten alist)))

(defun llm-symbol-search (symbol)
  "Search for the given SYMBOL references in the project using ripgrep.

  SYMBOL can be a string or a symbol. If it is a true symbol (not a string),
  it is converted to a string representation for the search.
  If it is a list or vector, the first string element is used as the search string.

  Returns the raw ripgrep output as a string suitable for LLM consumption."
  (let* ((query
          (cond
           ((and (or (listp symbol) (vectorp symbol)) (not (stringp symbol)))
            (let ((first-str (car (seq-filter #'stringp (if (vectorp symbol) (append symbol nil) symbol)))))
              (or first-str (error "List or vector did not contain a string element"))))
           ((stringp symbol) symbol)
           ((symbolp symbol) (symbol-name symbol))
           (t (error "Expected string, symbol, list, or vector, got: %s" symbol))))
         (project-root (llm-project-root))
         (default-directory project-root)
         (rg-cmd (format "rg --json --max-count 30 '%s'" query)))
    (shell-command-to-string rg-cmd)))

(provide 'llm-utils)
    ;;; llm-utils.el ends here

(use-package gptel
  :straight (:type git :host github :repo "karthink/gptel")
  :bind (("C-x g" . gptel-menu))
  :after direnv
  :config
  (require 'llm-utils)

  (setq gptel-display-buffer-action
      '((display-buffer-in-side-window)
        (side . right)))
  (defun read-file-as-string (file-path)
    (with-temp-buffer (insert-file-contents file-path)
                      (buffer-string)))

  (defun gptel-tool--open-file (file-name)
    "Open FILE-NAME using `find-file'."
    (if (file-exists-p file-name)
        (progn (find-file file-name) (gptel-add-file file-name))
      (format "File not found: %s" file-name)))

  (defvar gptel-tool-open-file-fn
    (gptel-make-tool
     :name "open_file"
     :function #'gptel-tool--open-file
     :description "Open a file by name using `find-file'."
     :args (list '(:name "file-name"
                         :type string
                         :description "Name of the file to open."))
     :category "file")
    "Tool for opening a file using `find-file'.")
  
  (add-to-list 'gptel-tools gptel-tool-open-file-fn)

  (defun gptel-tool--send-to-project-shell (command)
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
        (gptel-add)
        (goto-char (point-max))
        (comint-send-string shell-buf (concat command "\n")))))

  (defvar gptel-tool-shell-fn
    (gptel-make-tool
     :name "shell_command"
     :function (lambda (command)
                 (gptel-tool--send-to-project-shell command)
                 (format "Sent command to project shell: %s" command))
     :description "Run a shell command in the shell buffer for the current project."
     :args (list '(:name "command"
                         :type string
                         :description "Shell command to run in the project shell."))
     :category "project"))
  (add-to-list 'gptel-tools gptel-tool-shell-fn)

  (defun gptel-tool--save-buffer-or-file (target)
    "Save buffer named TARGET, or save buffer visiting TARGET if it’s a file path."
    (let ((buf (get-buffer target)))
      (cond
       ((buffer-live-p buf)
        (with-current-buffer buf (save-buffer))
        (format "Saved buffer: %s" target))
       ((file-exists-p target)
        (let ((b (find-buffer-visiting target)))
          (if b
              (with-current-buffer b (save-buffer))
            (with-current-buffer (find-file-noselect target)
              (save-buffer))))
        (format "Saved file: %s" target))
       (t (format "Target not found: %s" target)))))

  (defvar gptel-tool-save-buffer-fn
    (gptel-make-tool
     :name "save_buffer"
     :function #'gptel-tool--save-buffer-or-file
     :description "Save the specified buffer by name, or save the buffer visiting the given file."
     :args (list '(:name "target"
                         :type string
                         :description "Name of buffer to save, or file path."))
     :category "project"))
  (add-to-list 'gptel-tools gptel-tool-save-buffer-fn)

  (defvar gptel-elisp-function-whitelist
    '(message buffer-name list-buffers gptel-add-elisp-function-to-whitelist current-time-string)
    "List of symbols for whitelisted elisp function calls.")

     ;;; Utility to add a function to the gptel elisp whitelist
  (defun gptel-add-elisp-function-to-whitelist (fn-name)
    "Add FN-NAME (a string function name or symbol) to `gptel-elisp-function-whitelist' if not present. Prompts interactively if called with M-x. Always accepts a string or symbol, converts to symbol."
    (interactive
     (list (completing-read "Add function to whitelist: " obarray 'fboundp t)))
    (let ((sym (if (symbolp fn-name) fn-name (intern fn-name))))
      (unless (memq sym gptel-elisp-function-whitelist)
        (push sym gptel-elisp-function-whitelist)
        (message "Added %s to gptel-elisp-function-whitelist" sym)))
    (symbol-name (if (symbolp fn-name) fn-name (intern fn-name))))

  (defun gptel-tool--call-elisp (function-name args)
    "Call a whitelisted Emacs Lisp FUNCTION-NAME (string, symbol, vector, or list) with ARGS.
Unwraps nested 1-element vectors or lists for FUNCTION-NAME.
ARGS should be a list of strings; also unwrap nested vectors/lists in args elements.
If an argument is a character, convert it to a string."
    (cl-labels
        ((unwrap (x)
           (cond
            ((and (vectorp x) (= (length x) 1)) (unwrap (aref x 0)))
            ((and (listp x) (= (length x) 1)) (unwrap (car x)))
            (t x)))
         (unwrap-args (args-list)
           (mapcar (lambda (arg)
                     (let ((val (unwrap arg)))
                       (if (characterp val)
                           (char-to-string val)
                         (if (stringp val)
                             val
                           (prin1-to-string val)))))
                   args-list)))
      (let* ((sym (cond
                   ((symbolp function-name) function-name)
                   ((stringp function-name) (intern function-name))
                   ((vectorp function-name) (unwrap function-name))
                   ((listp function-name) (unwrap function-name))
                   (t (error "Unsupported function-name format: %S" function-name))))
             (unwrapped-args (unwrap-args (or args '()))))
        (if (memq sym gptel-elisp-function-whitelist)
            (condition-case err
                (prin1-to-string (apply sym unwrapped-args))
              (error (format "Function call failed: %s" err)))
          (when (yes-or-no-p (format "Add %s to elisp whitelist? " sym))
            (push sym gptel-elisp-function-whitelist)
            (prin1-to-string (apply sym unwrapped-args)))))))


  (defvar gptel-tool-call-elisp-fn
    (gptel-make-tool
     :name "call_elisp_function"
     :function (lambda (function-name &optional args)
                 (gptel-tool--call-elisp function-name (or args '())))
     :description "Call a whitelisted Emacs Lisp function with arguments and return result as string."
     :args (list '(:name "function-name"
                         :type string
                         :description "Name of Emacs Lisp function to call.")
                 '(:name "args"
                         :type array
                         :items '(:type string)
                         :optional t
                         :description "Arguments as a JSON array of strings or null."))
     :category "elisp"))
  (add-to-list 'gptel-tools gptel-tool-call-elisp-fn)

  (defun my/setup-gptel-buffer ()
    (olivetti-mode)
    (spell-fu-mode -1)
    (corfu-mode -1))

  (defun gptel-project-shell-tool (command)
    "Run COMMAND in the shell-mode buffer for the current project."
    (interactive "sShell command: ")
    (my/gptel-project-shell-send command))

  (defun my/setup-gptel ()
    (add-hook 'gptel-mode-hook #'my/setup-gptel-buffer)
    (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
    (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
    
    (setq gptel-backend (gptel-make-openai "LM-Studio"
                          :protocol "http"
                          ;; :host "localhost:1234"
                          :host "100.81.198.29:1234"
                          :stream t
                          :models '(lmstudio))
          gptel-model 'lmstudio)

    (setq gptel-backend (gptel-make-openai "OpenRouter"
                          :host "openrouter.ai"
                          :endpoint "/api/v1/chat/completions"
                          :stream nil
                          :key (getenv "OPENROUTER_API_KEY")
                          :models '(deepseek/deepseek-r1-0528:free)))

    (setq gptel-backend (gptel-make-openai "GitHub"
                          :host "models.inference.ai.azure.com"
                          :endpoint "/chat/completions"
                          :stream t
                          :key (getenv "GITHUB_MODELS_TOKEN")
                          :models '(gpt-4.1)))

    (setq gptel-default-mode 'org-mode)
    (setq gptel-prompt-prefix-alist '((markdown-mode . "### ") (org-mode . "* ") (text-mode . "### ")))
    (setq gptel--system-message (read-file-as-string "~/.emacs.d/config-org/gptel-system-message.md")))

  (my/setup-gptel))

(use-package llm-tool-collection
  :straight (:type git :host github :repo "skissue/llm-tool-collection")
  :after gptel
  :config
  (let ((tools (mapcar #'(lambda (tool)
                           (org-plist-delete tool :tags))
                       (llm-tool-collection-get-all))))
    (mapcar (apply-partially #'apply #'gptel-make-tool)
            tools)
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (push (gptel-get-tool path)
                        gptel-tools)))
            tools)))

(use-package mcp
  :straight (:type git :host github :repo "lizqwerscott/mcp.el")
  :config
  (defun gptel-mcp-register-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (apply #'gptel-make-tool
                         tool))
              tools)))
  (defun gptel-mcp-use-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (let ((path (list (plist-get tool :category)
                                    (plist-get tool :name))))
                    (push (gptel-get-tool path)
                          gptel-tools)))
              tools)))
  (defun mcp-hub-init ()
    (interactive)
    (gptel-mcp-register-tool)
    (gptel-mcp-use-tool))
  (setq mcp-hub-servers
        '(("tidewave" . (:url "http://localhost:4000/tidewave/mcp?include_fs_tools=true")))))
