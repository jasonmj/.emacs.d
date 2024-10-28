(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-word-org-subword-1))
  :config
  (key-chord-define-global "jj" 'avy-goto-word-or-subword-1)
  (key-chord-define-global "JJ" 'avy-goto-char-in-line))

(use-package centered-cursor-mode
  :ensure t
  :bind ("s--" . centered-cursor-mode))

(defun copy-keep-highlight (beg end)
  (interactive "r")
  (prog1 (clipboard-kill-ring-save beg end)
    (setq deactivate-mark nil)))
 (global-set-key (kbd "M-w") 'copy-keep-highlight)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first.  If
   point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(eval-after-load 'org #'(define-key org-mode-map (kbd "C-a") 'smarter-move-beginning-of-line))

(key-chord-define-global ".," 'other-window)

;;(key-seq-define-global "o0" 'pop-to-mark-command)
(key-seq-define-global "O)" 'pop-global-mark)

(use-package popper
  :ensure t
  :bind (("C-\\"   . popper-cycle))
  :custom
  (popper-mode-line t)
  (popper-window-height nil)
  (popper-reference-buffers '("^Shell:" "^Aweshell:" "-shell\\*$"))
  :init
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package related-files
  :ensure t
  :bind (("C-x j" . related-files-jump)))

(use-package related-files-recipe
  :demand t
  :after related-files)

(defun remove-directory-from-path (file remove-directory)
  "Return the paths to files looking like FILE but with REMOVE-DIRECTORY removed.

  The file-system is searched for existing directories but the
  returned paths don't have to exist."
  (when-let* ((path-segments (split-string file "/"))
	      (positions (related-files-recipe--seq-positions path-segments remove-directory)))
    (cl-loop
     for position in positions
     for candidate = (string-join (related-files-recipe--seq-remove-at-position path-segments position) "/")
     collect candidate)))

(defun replace-directory-in-path (file remove-directory add-directory)
  (let* ((without-removed (remove-directory-from-path file remove-directory))
	 (with-added (if without-removed
			 (related-files-recipe--add-directory-to-path (car without-removed) add-directory))))
    (if with-added
	(car with-added)
      "")))

(defun test-from-module (file)
  (concat (file-name-sans-extension (replace-directory-in-path file "lib" "test")) "_test.exs"))

(defun module-from-test (file)
  (let* ((replaced (replace-directory-in-path file "test" "lib"))
	 (without-suffix (if (> (length replaced) 0) (substring (file-name-sans-extension replaced) 0 -5))))
    (if without-suffix
	(concat without-suffix ".ex")
      file)))

(defun spec-from-module (file)
  (concat (file-name-sans-extension (replace-directory-in-path file "lib" "spec")) "_spec.exs"))

(defun module-from-spec (file)
  (let* ((replaced (replace-directory-in-path file "spec" "lib"))
	 (without-suffix (if (> (length replaced) 0) (substring (file-name-sans-extension replaced) 0 -5))))
    (if without-suffix
	(concat without-suffix ".ex")
      file)))

(defun my/related-files-jumper (file)
  (list
    (test-from-module file)
    (module-from-test file)
    (spec-from-module file)
    (module-from-spec file)))

(setq related-files-jumpers (list #'my/related-files-jumper))

(emacs-set-key (kbd "C-x x") 'scratch-buffer)
(key-chord-define-global "xx" 'scratch-buffer)

(global-set-key (kbd "s-a") 'mark-whole-buffer)

(defun forward-node ()
  (interactive)
  (let ((initial-pos (point)))
    (expreg-expand)
    (let ((new-pos (cdr (car (region-bounds)))))
      (if (eq initial-pos new-pos)
	  (progn (goto-char new-pos) (forward-char 1))
	(goto-char new-pos))))
    (deactivate-mark))
(global-set-key (kbd "C-s-f") 'forward-node)

(defun backward-node ()
  (interactive)
  (let ((initial-pos (point)))
    (expreg-expand)
    (let ((new-pos (car (car (region-bounds)))))
      (if (eq initial-pos new-pos)
	  (progn (backward-char 1) (backward-node))
	(goto-char new-pos))))
  (deactivate-mark))
(global-set-key (kbd "C-s-b") 'backward-node)

(defun up-node ()
  (interactive)
  (let ((initial-pos (point)))
    (expreg-expand)
    (let ((new-pos (car (car (region-bounds)))))
      (if (eq initial-pos new-pos)
	  (up-node)
	(goto-char new-pos))))
  (deactivate-mark))
(global-set-key (kbd "C-s-p") 'up-node)

(global-set-key (kbd "C-s-n") 'down-list)

(setq window-combination-resize t
      split-width-threshold 300)

(use-package tabgo
  :ensure t
  :bind ("C-S-SPC" . tabgo))

(defun visit-line ()
  (interactive)
  (let* ((args (split-string (selection-or-thing-at-point) ":"))
	 (filename (car args))
	 (fullpath (concat (project-root (project-current)) filename))
	 (line (car (cdr args))))
    (find-file fullpath)
    (goto-char (point-min))
    (forward-line (- (string-to-number line) 1))))
(global-set-key (kbd "C-x i") 'visit-line)

(use-package windmove :ensure t)
(emacs-set-key (kbd "s-b") 'windmove-left)
(emacs-set-key (kbd "s-f") 'windmove-right)
(emacs-set-key (kbd "s-p") 'windmove-up)
(emacs-set-key (kbd "s-n") 'windmove-down)
(emacs-set-key (kbd "C-1") 'delete-other-windows)
(emacs-set-key (kbd "C-2") (lambda () (interactive) (split-window-below)
				  (run-with-idle-timer 0.15 nil (lambda() (interactive) (windmove-down)))))
(emacs-set-key (kbd "C-3") (lambda () (interactive) (split-window-right) (windmove-right)))
(emacs-set-key (kbd "<C-escape>") 'delete-window)

(winner-mode 1)
(emacs-set-key (kbd "s-z") 'winner-undo)
(emacs-set-key (kbd "s-Z") 'winner-redo)

(use-package zoom
  :ensure t
  :commands zoom-mode
  :preface
  (setq zoom-size '(0.618 . 0.618))
  :config
  (defun my/work-around-zoom-issue ()
    (message "reloading zoom-mode")
    (load "zoom.el")
    (remove-hook 'zoom-mode-hook #'my/work-around-zoom-issue))
  :hook
  ((zoom-mode . my/work-around-zoom-issue)
   (after-init . zoom-mode)))
