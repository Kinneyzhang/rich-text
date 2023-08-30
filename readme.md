# Configuration
```emacs-lisp
(use-package rich-text
  :load-path "/path/to/rich-text"
  :init
  (use-package selected :ensure t)
  (setq rich-text-selected-ignore-modes '(prog-mode))
  :config
  (rich-text-mode 1))
```

# Commands
| command functions                 | key  |   |
|-----------------------------------|------|---|
| `rich-text-render-headline-1`     | `h1` |   |
| `rich-text-render-headline-2`     | `h2` |   |
| `rich-text-render-headline-3`     | `h3` |   |
| `rich-text-render-bold`           | `bb` |   |
| `rich-text-render-italic`         | `ii` |   |
| `rich-text-render-underline`      | `uu` |   |
| `rich-text-render-fontcolor`      | `cc` |   |
| `rich-text-render-highlight`      | `vv` |   |
|                                   |      |   |
| `rich-text-render-bold-dwim`      | `b1` |   |
| `rich-text-render-italic-dwim`    | `i1` |   |
| `rich-text-render-underline-dwim` | `u1` |   |
| `rich-text-render-fontcolor-dwim` | `c1` |   |
| `rich-text-render-highlight-dwim` | `v1` |   |
