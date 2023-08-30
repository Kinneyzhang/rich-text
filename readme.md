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

# usage
- `rich-text-render-headline-1`
- `rich-text-render-headline-2`
- `rich-text-render-headline-3`
- `rich-text-render-bold`
- `rich-text-render-italic`
- `rich-text-render-underline`
- `rich-text-render-fontcolor`
- `rich-text-render-highlight`

- `rich-text-render-bold-dwim`
- `rich-text-render-italic-dwim`
- `rich-text-render-underline-dwim`
- `rich-text-render-fontcolor-dwim`
- `rich-text-render-highlight-dwim`
