(defun alacritty()
  (interactive)
  (start-process-shell-command "alacritty" nil "alacritty"))
(defun alacritty-toggle-theme()
  (interactive)
  (call-process-shell-command "bash ~/.scripts/alacritty-theme-toggle.sh"))

(defun chromium()
  (interactive)
  (start-process-shell-command "chromium" nil "chromium"))
(exwm-input-set-key (kbd "C-c c") 'chromium)
(defun chromium-incognito()
  (interactive)
  (start-process-shell-command "chromium" nil "chromium --incognito"))
(exwm-input-set-key (kbd "C-c C") 'chromium-incognito)

(exwm-input-set-key (kbd "C-c f") 'firefox)
(key-seq-define-global "sf" 'firefox)
(defun firefox()
  (interactive)
  (start-process-shell-command "firefox fullscreen" nil "firefox -P fullscreen"))
(defun firefox-private()
  (interactive)
  (start-process-shell-command "firefox private" nil "firefox --private-window -P fullscreen"))
(exwm-input-set-key (kbd "C-c F") 'firefox-private)
(key-chord-define-global "cF" 'firefox-private)

(defun shutter()
  "Open shutter in select mode"
  (interactive)
  (start-process-shell-command "tmp" nil "rm ~/tmp.jpg")
  (start-process-shell-command "shutter" nil "shutter -s"))
(exwm-input-set-key (kbd "C-%") 'shutter)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "HDMI-0"
                                            1 "HDMI-0"
                                            2 "HDMI-0"
                                            3 "HDMI-0"
                                            4 "HDMI-0"
                                            5 "DP-2"
                                            6 "HDMI-0"
                                            7 "HDMI-0"
                                            8 "HDMI-0"
                                            9 "HDMI-0"))
(exwm-randr-enable)
(defun display-vertical()
  (interactive)
  (start-process-shell-command
   "xrandr" nil "xrandr --output DP-0 --off --output DP-1 --primary --mode 3840x2160 --pos 0x0 --rotate left --output HDMI-0 --off --output DP-2 --off"))
(defun display-horizontal()
  (interactive)
  (start-process-shell-command
   "xrandr" nil "xrandr --output DP-0 --off --output DP-1 --off --output HDMI-0 --primary --mode 3840x2160 --pos 0x0 --rotate normal --output DP-2 --off"))
(defun display-tv ()
  (interactive)
  (start-process-shell-command
   "xrandr" nil "xrandr --output DP-0 --off --output DP-1 --off --output HDMI-0 --mode 1360x768 --pos 0x0 --rotate normal --output DP-2 --off"))
(defun display-laptop()
  (interactive)
  (start-process-shell-command
   "xrandr" nil "xrandr --output HDMI-0 --off --output DP-2 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1 --off --output DP-0 --off"))
(defun display-both()
  (interactive)
  (start-process-shell-command "xrandr" nil "xrandr --output DP-0 --off --output DP-1 --primary --mode 3840x2160 --pos 0x0 --rotate left --output HDMI-0 --off --output DP-2 --mode 1920x1080 --pos 2160x2139 --rotate normal"))

(defun open-lastpass()
  (interactive)
  (if (string= exwm-class-name "firefox")
      (progn
        (exwm-input--fake-key 'C-l)
        (run-with-idle-timer 0.05 nil (lambda()
                                        (interactive)
                                        (exwm-input--fake-key 'tab)
                                        (exwm-input--fake-key 'tab)
                                        (exwm-input--fake-key 'return))))
    (progn
      (exwm-input--fake-key 'C-L)
      (run-with-idle-timer 0.5 nil (lambda()
                                     (interactive)
                                     (exwm-input--fake-key 'down)
                                     (exwm-input--fake-key 'down)
                                     (exwm-input--fake-key 'down)
                                     (exwm-input--fake-key 'down)
                                     (exwm-input--fake-key 'down)
                                     (exwm-input--fake-key 'down)
                                     (exwm-input--fake-key 'right)
                                     )))))
(exwm-input-set-key (kbd "M-s-l") 'open-lastpass)
(exwm-input-set-key (kbd "C-S-l") 'open-lastpass)

(defun upcase-letter()
  (interactive)
  (if (not (eq major-mode 'exwm-mode))
      (upcase-char 1)
    (progn
      (exwm-input--fake-key 'C-S-right)
      (run-with-idle-timer 0.05 nil (lambda () (exwm-input--fake-key ?\C-c)))
      (run-with-idle-timer 0.75 nil (lambda ()
                                      (kill-new (format "%s" (concat
                                                              (string-to-list (upcase (char-to-string (car (string-to-list (car kill-ring))))))
                                                              (cdr (string-to-list (car kill-ring)))))
                                                nil)))
      (run-with-idle-timer 1.2 nil (lambda () (exwm-input--fake-key ?\C-v))))))
(exwm-input-set-key (kbd "M-u") 'upcase-letter)
(defun downcase-letter()
  (interactive)
  (if (not (eq major-mode 'exwm-mode))
      (upcase-char 1)
    (progn
      (exwm-input--fake-key 'C-S-right)
      (run-with-idle-timer 0.05 nil (lambda () (exwm-input--fake-key ?\C-c)))
      (run-with-idle-timer 0.75 nil (lambda ()
                                      (kill-new (format "%s" (concat
                                                              (string-to-list (downcase (char-to-string (car (string-to-list (car kill-ring))))))
                                                              (cdr (string-to-list (car kill-ring)))))
                                                nil)))
      (run-with-idle-timer 1.2 nil (lambda () (exwm-input--fake-key ?\C-v))))))
(exwm-input-set-key (kbd "M-l") 'downcase-dwim)

(exwm-input-set-key (kbd "<S-XF86PowerOff>") (lambda () (interactive) (shell-command "slock")))

(setq exwm-enable-ido-workaround t)

(defun my/exwm-rename-buffer ()
  (interactive)
  (unless (eq exwm-title nil) (exwm-workspace-rename-buffer (if (<= (length exwm-title) 96) exwm-title
                                                        (concat (substring exwm-title 0 95) "...")))))
(add-hook 'exwm-update-title-hook 'my/exwm-rename-buffer)

(setq exwm-input-global-simulation-keys
      '(("C-b" . left)
        ("C-S-b" . S-left)
        ("C-s-p" . C-p)
        ("C-s-a" . C-S-a)
        ("M-b" . C-left)
        ("M-B" . C-S-left)
        ("C-f" . right)
        ("C-S-f" . S-right)
        ("M-f" . C-right)
        ("M-F" . C-S-right)
        ("C-p" . up)
        ("C-S-p" . S-up)
        ("C-n" . down)
        ("C-S-n" . S-down)
        ("C-s-n" . C-n)
        ("C-a" . home)
        ("C-w" . ?\C-c)
        ("C-S-w" . ?\C-w)
        ("C-S-a" . S-home)
        ("C-e" . end)
        ("C-u" . ?\C-u)
        ("C-S-e" . S-end)
        ("C-k" . (home S-end delete backspace))
        ("C-m" . return)
        ("C-s-k" . C-k)
        ("C-l" . (?\C-l ?\C-c))
        ;;("C-'" . (\" home \" return))
        ("C-\"" . (\" end \" return))
        ("<S-return>" . (end return))
        ("<S-XF86AudioPause>" . S-home)
        ("<XF86Favorites>" . C-s-f)
        ("C-v" . next)
        ("M-v" . prior)
        ("M-p" . (home S-end ?\C-c backspace delete up home return up ?\C-v))
        ("M-P" . C-S-p)
        ("M-n" . (home S-end ?\C-c backspace delete end return ?\C-v))
        ("C-," . (home S-end ?\C-c))
        ("M-N" . C-S-n)
        ("C-d" . delete)
        ("M-d" . C-delete)
        ("C-g" . escape)
        ("s-g" . escape)
        ("C-s-x" . ?\C-x)
        ("C-s-b" . ?\C-b)
        ("C-s" . ?\C-s)
        ("s-c" . ?\C-c)
        ("s-v" . ?\C-v)
        ("s-[" . M-left)
        ("s-]" . M-right)
        ("s-a" . ?\C-a)
        ("M-s" . ?\C-f)
        ("C-y" . ?\C-v)
        ("M-w" . ?\C-c)
        ("C-S-u" . C-S-f)
        ("M-I" . C-S-i)
        ("M-K" . C-S-k)
        ("S-TAB" . (C-left C-S-right C-c))
        ("M-<" . C-home)
        ("M->" . C-end)))
  (defun exwm-manage-keys-hook ()
    (exwm-input-set-simulation-keys
     (mapcar (lambda (c) (cons (kbd (car c)) (cdr c))) exwm-input-global-simulation-keys)))
  (add-hook 'exwm-manage-finish-hook 'exwm-manage-keys-hook)

(add-hook 'exwm-manage-finish-hook
    (lambda () (use-local-map (copy-keymap (current-local-map)))
                (local-set-key (kbd "M-y") (lookup-key global-map (kbd "M-y")))
                (local-set-key (kbd "C-SPC") (lookup-key global-map (kbd "C-SPC")))
                (local-set-key (kbd "C-s-SPC") (lookup-key global-map (kbd "C-s-SPC")))
                (local-set-key (kbd "C-<tab>") (lookup-key global-map (kbd "C-<tab>")))
                (local-set-key (kbd "C-S-<iso-lefttab>") (lookup-key global-map (kbd "C-S-<iso-lefttab>")))
                (local-set-key (kbd "M-<tab>") (lookup-key global-map (kbd "M-<tab>")))
                (local-set-key (kbd "M-S-<iso-lefttab>") (lookup-key global-map (kbd "M-S-<iso-lefttab>")))
                (local-set-key (kbd "C-;") (lookup-key global-map (kbd "C-;")))
		(local-set-key (kbd "s-SPC") (lookup-key global-map (kbd "s-SPC")))))

(setq exwm-manage-configurations '(((equal exwm-class-name ".zoom ")
                                    floating t
                                    floating-mode-line nil
                                    border-width 0)
                                   ((equal exwm-class-name "Example App")
                                    floating t
                                    floating-mode-line nil
                                    border-width 0)))
(defun impression-fixer ()
  (if (equal exwm-class-name "Example App")
      (run-with-idle-timer 1.25 nil (lambda ()
        (switch-to-buffer "Example App")
        (exwm-float-resize 600 448)
        (exwm-float-move 1320 0 600 448)))))
(add-hook 'exwm-floating-setup-hook 'impression-fixer)
(defun zoom-fixer ()
  (if (or (equal exwm-title "Zoom - Free Account")
          (equal exwm-title "Zoom Meeting")
          (equal exwm-title "Zoom")) (exwm-floating--unset-floating exwm--id)))
(add-hook 'exwm-update-title-hook 'zoom-fixer)
(add-hook 'exwm-update-class-hook 'zoom-fixer)

(add-to-list 'after-init-hook 'exwm-init)

(defun exwm-layout--show (id &optional window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (exwm--log "Show #x%x in %s" id window)
  (let* ((edges (window-inside-absolute-pixel-edges window))
         (x (pop edges))
         (y (pop edges))
         (width (- (pop edges) x))
         (height (- (pop edges) y))
         frame-x frame-y frame-width frame-height)
    (with-current-buffer (exwm--id->buffer id)
      (when exwm--floating-frame
        (setq frame-width (frame-pixel-width exwm--floating-frame)
              frame-height (+ (frame-pixel-height exwm--floating-frame)
                              ;; Use `frame-outer-height' in the future.
                              exwm-workspace--frame-y-offset))
        (when exwm--floating-frame-position
          (setq frame-x (elt exwm--floating-frame-position 0)
                frame-y (elt exwm--floating-frame-position 1)
                x (+ x frame-x (- exwm-layout--floating-hidden-position))
                y (+ y frame-y (- exwm-layout--floating-hidden-position)))
          (setq exwm--floating-frame-position nil))
        (exwm--set-geometry (frame-parameter exwm--floating-frame
                                             'exwm-container)
                            frame-x frame-y frame-width frame-height))
      (when (exwm-layout--fullscreen-p)
        (with-slots ((x* x)
                     (y* y)
                     (width* width)
                     (height* height))
            (exwm-workspace--get-geometry exwm--frame)
          (setq x x*
                y y*
                width width*
                height height*)))
      (when (bound-and-true-p tab-line-mode)
	 (setq y (+ y (frame-char-height))))
      (exwm--set-geometry id x y width height)
      (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
      (exwm-layout--set-state id xcb:icccm:WM_STATE:NormalState)
      (setq exwm--ewmh-state
            (delq xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state))
      (exwm-layout--set-ewmh-state id)
      (exwm-layout--auto-iconify)))
  (xcb:flush exwm--connection))

(defun exwm-move-window-to-workspace(workspace-number)
  (interactive)
  (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
        (id (exwm--buffer->id (window-buffer))))
    (exwm-workspace-move-window frame id)))
(exwm-input-set-key (kbd "C-c 1") (lambda() (interactive) (exwm-move-window-to-workspace 1) (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 1)))))
(exwm-input-set-key (kbd "C-c 2") (lambda() (interactive) (exwm-move-window-to-workspace 2) (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 2)))))
(exwm-input-set-key (kbd "C-c 3") (lambda() (interactive) (exwm-move-window-to-workspace 3) (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 3)))))
(exwm-input-set-key (kbd "C-c 4") (lambda() (interactive) (exwm-move-window-to-workspace 4) (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 4)))))
(exwm-input-set-key (kbd "C-c 5") (lambda() (interactive) (exwm-move-window-to-workspace 5) (run-with-idle-timer 0.05 nil (lambda() (exwm-workspace-switch 5)))))

(setq exwm-workspace-number 10)
(exwm-input-set-key (kbd "s-0") (lambda() (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-1") (lambda() (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-2") (lambda() (interactive) (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "s-3") (lambda() (interactive) (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "s-4") (lambda() (interactive) (exwm-workspace-switch 4)))
(exwm-input-set-key (kbd "s-5") (lambda() (interactive) (exwm-workspace-switch 5)))
(exwm-input-set-key (kbd "s-6") (lambda() (interactive) (exwm-workspace-switch 6)))
(exwm-input-set-key (kbd "s-7") (lambda() (interactive) (exwm-workspace-switch 7)))
(exwm-input-set-key (kbd "s-8") (lambda() (interactive) (exwm-workspace-switch 8)))
(exwm-input-set-key (kbd "s-9") (lambda() (interactive) (exwm-workspace-switch 9)))
(key-seq-define-global "w0" (lambda () (interactive) (exwm-workspace-switch 0)))
(key-seq-define-global "w1" (lambda () (interactive) (exwm-workspace-switch 1)))
(key-seq-define-global "w2" (lambda () (interactive) (exwm-workspace-switch 2)))
(key-seq-define-global "w3" (lambda () (interactive) (exwm-workspace-switch 3)))
(key-seq-define-global "w4" (lambda () (interactive) (exwm-workspace-switch 4)))
(key-seq-define-global "w5" (lambda () (interactive) (exwm-workspace-switch 5)))
(key-seq-define-global "w6" (lambda () (interactive) (exwm-workspace-switch 6)))
(key-seq-define-global "w7" (lambda () (interactive) (exwm-workspace-switch 7)))
(key-seq-define-global "w8" (lambda () (interactive) (exwm-workspace-switch 8)))
(key-seq-define-global "w9" (lambda () (interactive) (exwm-workspace-switch 9)))
(add-to-list 'exwm-init-hook (lambda() (interactive) (exwm-workspace-switch 1)))

(defun my/exwm-workspace-switch ()
  (interactive)
  (setq exwm-next-workspace-frame (exwm-workspace--prompt-for-workspace "Workspace: "))
    (run-with-idle-timer 0.01 nil (lambda () (exwm-workspace-switch exwm-next-workspace-frame))))
(exwm-input-set-key (kbd "s-w") 'my/exwm-workspace-switch)
