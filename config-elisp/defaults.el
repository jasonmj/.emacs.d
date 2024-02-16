(setq global-auto-revert-mode t)

(use-package bury-successful-compilation :ensure t)

(setq browse-url-generic-program "firefox"
      browse-url-browser-function 'browse-url-generic)

(setq delete-by-moving-to-trash t)

(ido-mode -1)

(add-hook 'focus-out-hook 'garbage-collect)

(setq mode-require-final-newline t)
(setq-default require-final-newline t)

(put 'narrow-to-region 'disabled nil)

(setq scroll-preserve-screen-position 'always)

(delete-selection-mode 1)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(use-package savekill
  :ensure t
  :custom (kill-ring-max 500))

(save-place-mode 1)

(setq-default history-length 1000)
(savehist-mode t)
(add-to-list 'savehist-additional-variables 'mark-ring)
(add-to-list 'savehist-additional-variables 'global-mark-ring)

(setq-default fill-column 80)

(setq-default scroll-margin 0)
(setq-default maximum-scroll-margin 0)

(setq-default create-lockfiles nil)

(setq warning-minimum-level :error)
(setq native-comp-async-report-warnings-errors 'silent)

(setq recentf-exclude '("~$" "/tmp" "/sudo:"))
(setq recentf-max-saved-items 300)
(add-hook 'find-file-hook 'recentf-save-list)
(recentf-mode 1)

(setq epg-gpg-program "gpg2")
(setq epa-pinentry-mode 'loopback)
(setq auth-sources '((:source "~/.authinfo.gpg")))
(pinentry-start)

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-inline-compress-start-size t)
(setq tramp-copy-size-limit nil)
(eval-after-load 'tramp '(setenv "SSH_AUTH_SOCK" "/run/user/1000/gnupg/S.gpg-agent.ssh"))
