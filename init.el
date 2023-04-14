;;; Visual
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

(setq mouse-autoselect-window nil
      focus-follows-mouse nil)
(use-package exwm :ensure t)
(require 'exwm-config)
(exwm-config-example)
(exwm-enable)

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

;;; Set the Config File
(defun load-directory (dir)
      (let ((load-it (lambda (f) (load-file (concat (file-name-as-directory dir) f)))))
	(mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/config-elisp/")

;;; Emacs Generated Custom Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
