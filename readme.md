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

