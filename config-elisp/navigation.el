(use-package avy :ensure t
  :bind (("C-c j" . avy-goto-word-org-subword-1))
  :config
  (key-chord-define-global "jj" 'avy-goto-word-or-subword-1)
  (key-chord-define-global "JJ" 'avy-goto-char-in-line))

(use-package centered-cursor-mode
  :ensure t
  :config
  (global-set-key (kbd "s--") 'centered-cursor-mode)
  (global-centered-cursor-mode t))

(global-set-key (kbd "M-;") 'comment-line)

(defun copy-keep-highlight (beg end)
  (interactive "r")
  (prog1 (clipboard-kill-ring-save beg end)
    (setq deactivate-mark nil)))
 (global-set-key (kbd "M-w") 'copy-keep-highlight)

(exwm-input-set-key (kbd "s-d") (lambda()(interactive)(dired "~/downloads")))

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1)
  (yank)
  (yank)
  (previous-line)
  (backward-word))
(global-set-key (kbd "C-c d") 'duplicate-line)

(global-set-key (kbd "C-s-f") 'forward-sexp)
(global-set-key (kbd "C-s-b") 'backward-sexp)

(defun kill-ring-clear () (interactive) (setq kill-ring nil))

(global-set-key (kbd "C-k") 'kill-whole-line)

(defun delete-word-no-copy (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))
(global-set-key (kbd "M-d") 'delete-word-no-copy)

(defun backward-delete-word-no-copy (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-word-no-copy (- arg)))
(global-set-key (kbd "<C-backspace>") 'backward-delete-word-no-copy)
(global-set-key (kbd "<M-backspace>") 'backward-delete-word-no-copy)

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
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
(global-set-key [remap org-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun end-of-line-minus-one () (interactive) (move-end-of-line 1) (left-char))
(global-set-key (kbd "s-e") 'end-of-line-minus-one)

(defun open-line-below ()
  (interactive)
  (move-end-of-line 1)
  (newline))
(global-set-key [(shift return)] 'open-line-below)

(key-chord-define-global ".," 'other-window)

(key-seq-define-global "o0" 'pop-to-mark-command)
(key-seq-define-global "O)" 'pop-global-mark)

(exwm-input-set-key (kbd "C-x x") 'scratch-buffer)

(global-set-key (kbd "s-a") 'mark-whole-buffer)

(global-set-key (kbd "C-s-p") (lambda() (interactive) (up-list) (backward-sexp)))
(global-set-key (kbd "C-s-n") 'down-list)

(winner-mode 1)
(exwm-input-set-key (kbd "s-z") 'winner-undo)
(exwm-input-set-key (kbd "s-Z") 'winner-redo)

(use-package zoom
  :ensure t
  :commands zoom-mode
  :preface
  (setq zoom-size '(0.618 . 0.618))
  :init
  (zoom-mode))
