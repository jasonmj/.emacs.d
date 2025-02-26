;;; Visual
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(toggle-debug-on-error)

;;; Add Package and Archives
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Load Minimal Config
(load-file "~/.emacs.d/minimal-config.el")

;;; Start the Emacs server
(require 'server)
(or (server-running-p)
    (server-start))

;;; Maybe EXWM Set Key
(defun emacs-set-key (key cmd)
  (global-set-key key cmd))

;;; MacOS Customizations
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell :ensure t)
  (exec-path-from-shell-initialize)

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil)

  (setq mac-control-modifier 'super)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta)
  (setq mac-pass-command-to-system nil)

  (global-set-key (kbd "C-S-w") 'save-buffers-kill-terminal)

  (emacs-set-key (kbd "s-1") (lambda () (interactive) (call-process-shell-command "osascript -e 'tell Application \"BetterTouchTool\" to trigger_named \"Desktop 1\"'")))
  (emacs-set-key (kbd "s-2") (lambda () (interactive) (call-process-shell-command "osascript -e 'tell Application \"BetterTouchTool\" to trigger_named \"Desktop 2\"'")))
  (emacs-set-key (kbd "s-3") (lambda () (interactive) (call-process-shell-command "osascript -e 'tell Application \"BetterTouchTool\" to trigger_named \"Desktop 3\"'")))
  (emacs-set-key (kbd "s-4") (lambda () (interactive) (call-process-shell-command "osascript -e 'tell Application \"BetterTouchTool\" to trigger_named \"Desktop 4\"'")))
  (emacs-set-key (kbd "s-5") (lambda () (interactive) (call-process-shell-command "osascript -e 'tell Application \"BetterTouchTool\" to trigger_named \"Desktop 5\"'")))

  (emacs-set-key (kbd "C-c f") 'firefox)
  (key-seq-define-global "sf" 'firefox)
  (defun firefox ()
    (interactive)
    (start-process-shell-command "firefox fullscreen" nil "/Applications/Firefox.app/Contents/MacOS/firefox")))

;;; Straight
(defvar native-comp-deferred-compilation-deny-list ())
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

;;; Project
(straight-use-package 'project)

;;; Set the Config File
(defun load-directory (dir)
      (let ((load-it (lambda (f) (load-file (concat (file-name-as-directory dir) f)))))
	(mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/config-elisp/")
(toggle-debug-on-error)

;;; Emacs Generated Custom Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-default-style 'osx-notifier)
 '(dimmer-fraction 0.2)
 '(global-so-long-mode t)
 '(keycast-header-line-format "%1s%k%c%r ")
 '(safe-local-variable-values '((eval add-to-list 'vc-directory-exclusion-list "docs")))
 '(so-long-action 'so-long-minor-mode)
 '(so-long-threshold 1000))
 '(global-so-long-mode t)
 '(so-long-action 'so-long-minor-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "DarkGoldenrod1"))))
 '(eldoc-box-body ((t (:background "#fbf7f0" :foreground "#000000"))))
 '(eldoc-box-border ((t (:background "#000000"))))
 '(which-key-posframe-border ((t nil))))
