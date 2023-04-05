(use-package cape
  :ensure t
  :bind (("C-c q" . completion-at-point))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(use-package corfu
  :ensure t
  :init
  (setq corfu-auto-prefix 2
	corfu-auto-delay 0.05
	corfu-auto t
	corfu-cycle t
	corfu-quit-no-match t
	corfu-preselect 'first
	corfu-scroll-margin 5)
  (corfu-indexed-mode 1)
  (corfu-history-mode 1)
  (savehist-mode t)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (setq corfu-indexed-start 1)

  ;; Customize corfu--affixate to exclude space after index
  (cl-defmethod corfu--affixate :around (cands &context (corfu-indexed-mode (eql t)))
    (setq cands (cdr (cl-call-next-method cands)))
    (let* ((space #(" " 0 1 (face (:height 0.5 :inherit corfu-indexed))))
           (width (if (length> cands (- 10 corfu-indexed-start)) 2 1))
           (fmt (concat space
			(propertize (format "%%%ds" width)
                                    'face 'corfu-indexed)
			space))
           (align
            (propertize (make-string width ?\s)
			'display
			`(space :align-to (+ left ,(1+ width))))))
      (cl-loop for cand in cands for index from corfu-indexed-start do
	       (setf (cadr cand)
		     (concat
		      (propertize " " 'display (format fmt index))
		      (cadr cand)
		      align)))
      (cons t cands)))

  ;; Completion in the minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  ;; Insert indexed candidate without needing to press enter
  (defun corfu-indexed-insert (i)
    (setq corfu--index (- i 1))
    (call-interactively #'corfu-insert))
  (loopy-iter
   (with (map corfu-map))
   (numbering i :from 1 :to 9)
   (define-key map (kbd (format "s-%d" i)) `(lambda () (interactive) (corfu-indexed-insert ,i))))
  (global-corfu-mode))

;; Customize Emacs for Corfu usage
(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon)
  (kind-icon-default-face 'corfu-default)
  :config
  (setq kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 0.6)
	kind-icon-extra-space t)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
