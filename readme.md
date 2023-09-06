Rich-text is an emacs package to render rich text in various types of files.

# Demo


# Configuration

```emacs-lisp
(use-package rich-text
  :load-path "/path/to/rich-text"
  :init
  (use-package selected :ensure t)
  (setq rich-text-selected-ignore-modes '(prog-mode))
  (setq rich-text-selected-key-alist
        '(("b." . rich-text-render-bold-dwim)
          ("i." . rich-text-render-italic-dwim)
          ("u." . rich-text-render-underline-dwim)
          ("c." . rich-text-render-fontcolor-dwim)
          ("v." . rich-text-render-highlight-dwim)))
  :config
  (rich-text-mode 1))
```

- `rich-text-selected-ignore-modes` is a list of major modes in which you want to ignore rich-text local keybindings when a region is active.

- `rich-text-selected-key-alist` is a alist consists of key and command to render rich text when region is active.

use `define-rich-text-face` macro to customize your own rich text face.

```emacs-lisp
(define-rich-text-face pretty-green-light-1
  :key "gl1"
  :props
  (face (:background "#5B9A8B" :foreground "white"
                     :box (:line-width (3 . 1) :color "#5B9A8B"))))

(define-rich-text-face pretty-green-dark-1
  :key "gd1"
  :props (face (:background "#016A70" :foreground "white"
                            :box (:line-width (3 . 1) :color "#016A70"))))

(define-rich-text-face pretty-purple-light-1
  :key "pl1"
  :props (face (:background "#8062D6" :foreground "white"
                            :box (:line-width (3 . 1) :color "#8062D6"))))

(define-rich-text-face pretty-orange-light-1
  :key "ol1"
  :props (face (:background "#FF8551" :foreground "white"
                            :box (:line-width (3 . 1) :color "#FF8551"))))
```

# Commands

The following keys are used when a region is active and key bindings are invalided when the current major mode are derived from modes in `rich-text-selected-ignore-modes`.

| command function                | default key | description                                                                                   |
|---------------------------------|:-----------:|-----------------------------------------------------------------------------------------------|
| rich-text-render-headline-1     | `h1`        | render headline with `rich-text-headline-1-height `                                           |
| rich-text-render-headline-2     | `h2`        | render headline with `rich-text-headline-2-height `                                           |
| rich-text-render-headline-3     | `h3`        | render headline with `rich-text-headline-3-height `                                           |
| rich-text-render-bold           | `bb`        | render bold with `rich-text-bold-type`                                                        |
| rich-text-render-italic         | `ii`        | render italic with `rich-text-italic-type`                                                    |
| rich-text-render-underline      | `uu`        | render underline with color `rich-text-underline-color` and style `rich-text-underline-style` |
| rich-text-render-fontcolor      | `cc`        | render font color with `rich-text-font-color`                                                 |
| rich-text-render-highlight      | `vv`        | render highlight color with `rich-text-highlight-color`                                       |
| rich-text-render-bold-dwim      | `b1`        | choose a bold type from `rich-text-bold-types`                                                |
| rich-text-render-italic-dwim    | `i1`        | choose a italic type from `rich-text-italic-types`                                            |
| rich-text-render-underline-dwim | `u1`        | choose a underline color according to `rich-text-underline-colors-by-theme`                   |
| rich-text-render-fontcolor-dwim | `c1`        | choose a font color according to `rich-text-font-colors-by-theme`                             |
| rich-text-render-highlight-dwim | `v1`        | choose a highlight color according to `rich-text-highlight-colors-by-theme`                   |

