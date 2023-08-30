# configuration
```emacs-lisp
(use-package rich-text
  :load-path "/path/to/rich-text"
  :init
  (use-package selected :ensure t)
  (setq rich-text-selected-ignore-modes '(prog-mode))
  :config
  (rich-text-mode 1))
```
