(define-rich-text bold-italic "bi"
  '(face (:weight bold :slant italic)))

(define-rich-text-dwim underline-dwim "uu"
  :props '(face (:underline (:style wave)))
  :light '(face (:underline (:style line))))

(define-rich-text-dwim quote "qt"
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

;;; 设置默认的 color 和 highlight 为上面这种经过优化的颜色

(provide 'rich-text-plus)
