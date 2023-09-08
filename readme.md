Rich-text is an emacs package to render rich text in various types of files.

# Demo


# Configuration
```emacs-lisp
(use-package rich-text
  :load-path "/path/to/rich-text"
  :init (setq rich-text-selected-ignore-modes '(prog-mode))
  :config
  (rich-text-mode 1))
```

- `rich-text-selected-ignore-modes` is a list of major modes. Local keybindings will be ignored in major modes and the derived ones when a region is active.

# Usage

use `define-rich-text` and `define-rich-text-dwim` macros to customize your own rich text format.

## some built-in ones
There are some built-in rich-text formats:

| rich-text      | key  |                                                                        |
|----------------|------|------------------------------------------------------------------------|
| headline-1     | `h1` | set `rich-text-headline-1-height` to customize the default height.     |
| headline-2     | `h2` | set `rich-text-headline-2-height` to customize the default height.     |
| headline-3     | `h3` | set `rich-text-headline-3-height` to customize the default height.     |
| underline-line | `ul` | straight underline                                                     |
| underline-wave | `uw` | wave underline                                                         |
| bold           | `bb` | set `rich-text-bold-type` to customize the default type of `bold`.     |
| italic         | `ii` | set `rich-text-italic-type` to customize the default type of `italic`. |
| fontcolor      | `cc` | set `rich-text-font-color` to customize the default color.             |
| highlight      | `vv` | set `rich-text-highlight-color` to customize the default color.        |

- the type of `bold` should be one of the symbols `ultra-bold extra-bold bold semi-bold normal semi-light light extra-light ultra-light`.
- the type of `italic` should be one of the symbols `italic oblique normal reverse-italic reverse-oblique`.
    
## define-rich-text
`(define-rich-text NAME KEY PROPS)` could be used to define a simple rich-text format. NAME is the name of rich-text format. KEY is the keybinding. PROPS is elisp text properties.

For example, the elisp below define a rich-text format named `bold-underline` which is binded to two single keystrokes "bu". When you press key <bu> on a region, text in region will be rendered by text properties `'(face (:weight bold :underline t))`

```emacs-lisp
(define-rich-text bold-underline "bu"
  '(face (:weight bold :underline t)))
```

## define-rich-text-dwim
`(define-rich-text-dwim NAME KEY &KEY PROPS LIGHT DARK)` is an enhanced version of `define-rich-text`, which support setting specific properties for themes with light or dark background. And when the background of emacs theme is changed, the rich-text properties will also be changed adaptably.

This feature is useful when you want to render different colors separately in light or dark themes, since a color suitable for light themes may not for dark themes, vice versa.

```emacs-lisp
(define-rich-text-dwim highlight-1 "v1"
  :light (face (:background "#F7E987" :foreground "black"))
  :dark (face (:background "#C58940" :foreground "white")))
```
