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

(use-package gptel
  :straight (:type git :host github :repo "karthink/gptel")
  :bind (("C-x g" . gptel-menu))
  :after direnv
  :config
  (defun read-file-as-string (file-path)
    (with-temp-buffer (insert-file-contents file-path)
                      (buffer-string)))

  (defun gptel-tool--open-file (file-name)
    "Open FILE-NAME using `find-file'."
    (if (file-exists-p file-name)
        (progn (find-file file-name) (gptel-add))
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
    "Call a whitelisted Emacs Lisp FUNCTION-NAME (string, symbol, or 1-element vector) with ARGS. Prompts to whitelist new functions."
    (let* ((sym (cond
                 ((symbolp function-name) function-name)
                 ((and (vectorp function-name)
                       (= (length function-name) 1))
                  (aref function-name 0))
                 ((stringp function-name) (intern function-name))
                 ((listp function-name)
                  (intern (format "%s" (car function-name))))
                 (t (error "Unsupported function-name format: %S" function-name)))))
      (if (memq sym gptel-elisp-function-whitelist)
          (condition-case err
              (prin1-to-string (apply sym args))
            (error (format "Function call failed: %s" err)))
        (when (yes-or-no-p (format "Add %s to elisp whitelist? " sym))
          (push sym gptel-elisp-function-whitelist)
          (prin1-to-string (apply sym args))))))


  (defvar gptel-tool-call-elisp-fn
    (gptel-make-tool
     :name "call_elisp_function"
     :function (lambda (function-name &optional args)
                 (gptel-tool--call-elisp function-name (or args nil)))
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
                          :host "localhost:1234"
                          :stream t
                          :models '(lmstudio))
          gptel-model 'lmstudio)

    (setq gptel-model   'mistralai/mistral-small-3.2-24b-instruct:free
          gptel-backend (gptel-make-openai "OpenRouter"
                          :host "openrouter.ai"
                          :endpoint "/api/v1/chat/completions"
                          :stream nil
                          :key (getenv "OPENROUTER_API_KEY")
                          :models '(deepseek/deepseek-r1-0528:free
                                    mistralai/mistral-small-3.2-24b-instruct:free)))

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
