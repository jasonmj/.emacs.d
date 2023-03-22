(setq dired-listing-switches "-l -A -h -v --group-directories-first")
(defun dired-navigation-keys ()
  (define-key dired-mode-map "[" (lambda nil (interactive) (dired-single-buffer
"..")))
  (define-key dired-mode-map "]" 'dired-single-buffer))
(add-hook 'dired-mode-hook #'dired-navigation-keys)
(global-set-key (kbd "C-x d") 'dired-x-find-file)
(global-set-key (kbd "C-x C-d") 'dired-x-find-file)
(key-seq-define-global "xd" 'dired-x-find-file)

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
(exwm-input-set-key (kbd "C-x d") 'dired-recent-dirs)

(use-package direnv :ensure t :hook (after-init . direnv-mode))

(use-package dired-hide-dotfiles :ensure t)
(defun dired-toggle-dotfiles()
  (dired-hide-dotfiles-mode)
  (revert-buffer)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))
(add-hook 'dired-mode-hook #'dired-toggle-dotfiles)
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$"))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (bind-key "<tab>" (lambda()(interactive)(dired-subtree-toggle)(revert-buffer)) dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(defun hide-details-mode-hook ()
  (dired-hide-details-mode))
(add-hook 'dired-mode-hook #'hide-details-mode-hook)

(setq dired-auto-revert-buffer t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq dired-dwim-target t)

(use-package dired-single :ensure t)
(defun dired-single-init ()
  (set (make-local-variable 'mouse-1-click-follows-link) nil)
  (set (make-local-variable 'mouse-3-click-follows-link) nil)
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map [down-mouse-3] 'crux-open-with)
  (define-key dired-mode-map (kbd "<mouse-8>") (lambda nil (interactive) (dired-single-buffer "..")))
  (define-key dired-mode-map "^" (lambda nil (interactive) (dired-single-buffer ".."))))
(add-hook 'dired-mode-hook 'dired-single-init)

(key-chord-define-global "wd" 'wdired-change-to-wdired-mode)

(key-seq-define-global "1`" (lambda () (interactive) (dired "~/")))
