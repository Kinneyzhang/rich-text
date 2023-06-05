(defun rich-text-color-test ()
  (interactive)
  (let ((color (completing-read "Input a color: " nil)))
    (rich-text-ov-set-dwim `(face (:foreground ,color)))))

;; #FF9800
;; #00BCD4

;; find-file-hook
;; after-save-hook

;; chui93egchuiowecioweh
;; (rich-text-mode 1)
;; (setq rich-text-db-file "~/.config/emacs/rich-text")

;; (rich-text-db-crud [:select * :from ov])
(rich-text-db-crud
 [:select * :from ov :where (= id "c:/Users/26289/gknows/20230526193438.org")])
;; ;; (rich-text-db-crud [:delete :from ov])

;; (rich-text-db-crud
;;  `[:select [region face] :from ov
;;            :where (= filename ,(buffer-file-name))])

;; (cl-defstruct (rtm-face (:constructor rtm-face-create)
;;                         (:copier nil)
;;                         (:type vector))
;;   foreground-color background-color
;;   weight slant underline height)

;; (defun cl-struct-solts (struct)
;;   (mapcar #'car (cl-struct-slot-info struct)))

;; (defun cl-struct-solt-value-plist (struct inst)
;;   (let* ((slots (cl-struct-solts struct))
;;          (values (append inst nil))
;;          (len (length slots))
;;          plist)
;;     (cl-loop for slot in slots
;;              for val in values
;;              when (not (null val))
;;              do (setq plist (plist-put plist slot val)))
;;     plist))

;; (cl-struct-solts 'rtm-face)
;; (cl-struct-solt-value-plist 'rtm-face rtm-headline-1-face)

;; (cl-defstruct (rtm-ov (:constructor rtm-ov-create)
;;                       (:conc-name rtm-ov)
;;                       (:copier nil))
;;   before-string after-string
;;   line-prefix wrap-prefix keymap)

;; (defun ov-in-region ()
;;   (when (use-region-p)
;;     (ov-in 'face 'any (region-beginning) (region-end))))

;; (ov-spec (ov-in-region))

;; (defun ov-val-in-region (prop)
;;   (mapcar (lambda (ov)
;;             (ov-val ov 'face))
;;           (ov-in-region)))

