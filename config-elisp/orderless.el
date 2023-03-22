(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
