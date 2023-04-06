(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-quick
  :after vertico
  :bind (:map vertico-map
         ("M-i" . vertico-quick-insert)
         ("C-'" . vertico-quick-exit)
         ("C-o" . vertico-quick-embark))
  :config
  (defun vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg))))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

(use-package vertico-posframe
  :ensure t
  :config
  (setq vertico-posframe-border-width 20
	vertico-posframe-hide-minibuffer t
	vertico-posframe-min-width 80
	vertico-posframe-height nil
	vertico-posframe-min-height 10
	vertico-posframe-width 110
	vertico-posframe-poshandler #'posframe-poshandler-window-top-center-offset
	vertico-posframe-parameters '((alpha . 92)
				      (parent-frame . nil)
				      (cursor-type . 'bar)
				      (left-fringe . 0)
				      (right-fringe . 0)))
  (defun posframe-poshandler-window-top-center-offset (info)
    "Posframe's position handler.

       Get a position which let posframe stay onto current window's
       top center.  The structure of INFO can be found in docstring of
       `posframe-show'."
    (setq-local tab-line-format nil)
    (let* ((window-left (plist-get info :parent-window-left))
	   (window-top (plist-get info :parent-window-top))
	   (window-width (plist-get info :parent-window-width))
	   (posframe-width (plist-get info :posframe-width)))
      (cons (+ window-left (/ (- window-width posframe-width) 2))
	    (+ window-top 64))))
  (vertico-indexed-mode 1)
  (setq vertico-indexed-start 1)
  (defun vertico-indexed-insert (i)
    (setq vertico--index (- i 1))
    (call-interactively #'vertico-insert)
    (call-interactively #'vertico-exit))
  (loopy-iter
   (with (map vertico-map))
   (numbering i :from 1 :to 9)
   (define-key map (kbd (format "s-%d" i)) `(lambda () (interactive) (vertico-indexed-insert ,i))))
  (custom-set-faces '(vertico-posframe-border ((t nil))))
  (vertico-posframe-mode 1))
