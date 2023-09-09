(define-rich-text bold-italic "bi"
  '(face (:weight bold :slant italic)))

(define-rich-text bold-underline "bu"
  '(face (:weight bold :underline t)))

(define-rich-text italic-underline "iu"
  '(face (:slant italic :underline t)))

(define-rich-text-dwim blockquote "qt"
  :light `(wrap-prefix
           ,(propertize "┃ " 'face '(:foreground "#999"))
           line-prefix
           ,(propertize "┃ " 'face '(:foreground "#999"))
           face (:slant italic))
  :dark `(wrap-prefix
          ,(propertize "┃ " 'face '(:foreground "#ccc"))
          line-prefix
          ,(propertize "┃ " 'face '(:foreground "#ccc"))
          face (:slant italic)))

(define-rich-text plain-list "pl"
  '(line-prefix "• "))

(define-rich-text small-text "sm"
  '(face (:height 0.9)))

;; for test
(define-rich-text-dwim underline-dwim "uu"
  :props '(face (:underline (:style wave)))
  :light '(face (:underline (:style line))))

(provide 'rich-text-plus)
