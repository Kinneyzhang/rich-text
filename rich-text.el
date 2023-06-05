;;; rich-text-mode
(require 'ov)
(require 'selected)
(require 'rich-text-db)

(defvar rich-text-headline-1-height 1.8)
(defvar rich-text-headline-2-height 1.5)
(defvar rich-text-headline-3-height 1.2)
(defvar rich-text-default-bold-type 'bold)
(defvar rich-text-default-italic-type 'italic)
(defvar rich-text-default-underline-color "grey")
(defvar rich-text-default-underline-style 'line
  "Default style of face attribute :underline,
should be one of symbols line and wave")
(defvar rich-text-default-font-color "blue")
(defvar rich-text-default-highlight-color "yellow")
(defvar rich-text-bold-types
  '(ultra-bold extra-bold bold semi-bold normal semi-light light extra-light ultra-light))
(defvar rich-text-italic-types
  '(italic oblique normal reverse-italic reverse-oblique))
(defvar rich-text-underline-styles '(line wave))
(defvar rich-text-light-colors '("#F44336" "#009688" "#FF9800" "#00BCD4"))
(defvar rich-text-dark-colors '("#F44336" "#009688" "#FF9800" "#00BCD4"))

;;; rich text properties

(defvar rich-text-headline-1-props
  `(face (:height ,rich-text-headline-1-height :weight bold)))

(defvar rich-text-headline-2-props
  `(face (:height ,rich-text-headline-2-height :weight bold)))

(defvar rich-text-headline-3-props
  `(face (:height ,rich-text-headline-3-height :weight bold)))

(defvar rich-text-bold-props
  `(face (:weight ,rich-text-default-bold-type)))

(defvar rich-text-italic-props
  `(face (:slant ,rich-text-default-italic-type)))

(defvar rich-text-color-props
  `(face (:foreground ,rich-text-default-font-color)))

(defvar rich-text-highlight-props
  `(face (:background ,rich-text-default-highlight-color)))

(defvar rich-text-underline-props
  `(face (:underline (:color ,rich-text-default-underline-color :style ,rich-text-default-underline-style))))

(defvar rich-text-mode-map (make-sparse-keymap))
(defvar selected-emacs-lisp-mode-map (make-sparse-keymap))

;;; utilities functions

(defun rich-text-theme-dark-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun rich-text-theme-light-p ()
  (eq (frame-parameter nil 'background-mode) 'light))

(defun rich-text-colors-by-theme ()
  "Return a list of color according to the type of theme."
  (when (rich-text-theme-light-p) rich-text-light-colors)
  (when (rich-text-theme-dark-p) rich-text-dark-colors))

(defun rich-text-propertized-colors-by-theme (face-type)
  (pcase face-type
    ('color (mapcar (lambda (color)
                      (propertize color 'face `(:foreground ,color)))
                    (rich-text-colors-by-theme)))
    ('highlight (mapcar (lambda (color)
                          (propertize color 'face `(:background ,color)))
                        (rich-text-colors-by-theme)))
    ('underline (mapcar (lambda (color)
                          (propertize color 'face `(:underline (:color ,color))))
                        (rich-text-colors-by-theme)))
    (_ (rich-text-colors-by-theme))))

;;; render functions

(defun rich-text-ov-set-dwim (props)
  (let (ov)
    (if (use-region-p)
        (progn
          (setq ov (ov-set (ov-region) props)))
      (setq ov (ov-set (ov-line) props)))
    (setq ov (ov-set ov '(evaporate t)))
    ;; indicate a overlay set by rich text.
    (setq ov (ov-set ov '(rich-text t)))
    (rich-text-store-curr-ov (car (ov-spec ov)))))

(defun rich-text-render-headline (arg)
  "HD-LEVEL should be one of the numbers 1, 2 or 3 which means
the different level of headline."
  (interactive "P")
  (let ((props (pcase arg
                 (1 rich-text-headline-1-props)
                 (2 rich-text-headline-2-props)
                 (3 rich-text-headline-3-props)
                 (_ rich-text-headline-3-props))))
    (rich-text-ov-set-dwim props)))

(defun rich-text-render-headline-1 ()
  (interactive)
  (rich-text-render-headline 1))

(defun rich-text-render-headline-2 ()
  (interactive)
  (rich-text-render-headline 2))

(defun rich-text-render-headline-3 ()
  (interactive)
  (rich-text-render-headline 3))

(defun rich-text-render-bold (arg)
  "Render current region with face attribute :weight.
Defaultly use `rich-text-default-bold-type'. If ARG is non-nil,
 choose a bold type from `rich-text-bold-types'."
  (interactive "P")
  (if arg
      (let ((type (completing-read "Choose a bold type" rich-text-bold-types nil t)))
        (rich-text-ov-set-dwim `(face (:weight ,type))))
    (rich-text-ov-set-dwim rich-text-bold-props)))

(defun rich-text-render-italic (arg)
  "Render current region with face attribute :slant.
Defaultly use `rich-text-default-italic-type'. If ARG is non-nil,
 choose a bold type from `rich-text-italic-types'."
  (interactive "P")
  (if arg
      (let ((type (completing-read "Choose a italic type" rich-text-italic-types nil t)))
        (rich-text-ov-set-dwim `(face (:slant ,type))))
    (rich-text-ov-set-dwim rich-text-italic-props)))

(defun rich-text-render-underline ()
  (interactive)
  (let ((color (completing-read "Choose a line underline color"
                                (rich-text-propertized-colors-by-theme 'underline))))
    (rich-text-ov-set-dwim `(face (:underline (:color ,color :style line))))))

(defun rich-text-render-color ()
  (interactive)
  (let ((color (completing-read "Choose a font color"
                                (rich-text-propertized-colors-by-theme 'color))))
    (rich-text-ov-set-dwim `(face (:foreground ,color)))))

(defun rich-text-render-highlight ()
  (interactive)
  (let ((color (completing-read "Choose a highlight color"
                                (rich-text-propertized-colors-by-theme 'highlight))))
    (rich-text-ov-set-dwim `(face (:background ,color)))))

;;; rich text mode

(defun rich-text-set-region-keymap ()
  (define-key selected-keymap "h1" #'rich-text-render-headline-1)
  (define-key selected-keymap "h2" #'rich-text-render-headline-2)
  (define-key selected-keymap "h3" #'rich-text-render-headline-3)
  (define-key selected-keymap "b" #'rich-text-render-bold)
  (define-key selected-keymap "i" #'rich-text-render-italic)
  (define-key selected-keymap "uu" #'rich-text-render-underline)
  (define-key selected-keymap "cc" #'rich-text-render-color)
  (define-key selected-keymap "hh" #'rich-text-render-highlight))

(defun rich-text-buffer-or-file-id (&optional buffer-or-name)
  "FIXME: repalce with unique id of file or buffer in system."
  (let ((buffer-or-name (or buffer-or-name (current-buffer))))
    (buffer-file-name (get-buffer buffer-or-name))))

(defun rich-text-all-id ()
  (when-let ((id (rich-text-buffer-or-file-id (current-buffer))))
    (mapcar #'car (rich-text-db-distinct-query 'ov [id]))))

(defun rich-text-buffer-stored-p (id)
  (cl-member id (rich-text-all-id) :test 'string=))

(defun rich-text-store-curr-ov (ov-spec)
  (when-let* ((beg (nth 0 ov-spec))
              (end (nth 1 ov-spec))
              (id (rich-text-buffer-or-file-id (nth 2 ov-spec)))
              (props (nth 3 ov-spec)))
    (rich-text-db-insert 'ov `([,id ,beg ,end ,props]))))

(defun rich-text-buffer-ov-specs ()
  "A list of overlay spec set by `rich-text-mode',
 exclude other already exist overlays."
  (seq-filter (lambda (spec)
                (cl-member 'rich-text (nth 3 spec) :test 'eq))
              (ov-spec (ov-in))))

(defun rich-text-store-buffer-ov ()
  (let ((count (rich-text-db-query-count 'ov `(= id ,(rich-text-buffer-or-file-id)))))
    (when (not (= count 0))
      (rich-text-db-delete 'ov `(= id ,(rich-text-buffer-or-file-id))))
    (when-let ((specs (rich-text-buffer-ov-specs)))
      (mapcar (lambda (spec)
                (rich-text-store-curr-ov spec))
              specs))))

(defun rich-text-restore-buffer-ov ()
  (interactive)
  (when-let* ((id (rich-text-buffer-or-file-id))
              (_ (rich-text-buffer-stored-p id))
              (specs (rich-text-db-query 'ov [beg end props]
                                         `(= id ,id))))
    (mapcar (lambda (spec)
              (ov (nth 0 spec) (nth 1 spec) (nth 2 spec)))
            specs)
    (message "%s rich-text overlays restored" (length specs))))

(defun rich-text-use-region-keyhint ()
  (when (use-region-p)
    (message "[h] headline [b] bold [i] italic")))

(define-minor-mode rich-text-mode
  "Minor mode for showing rich text in buffer."
  :lighter "RT"
  :global t
  :keymap rich-text-mode-map
  (if rich-text-mode
      (progn
        (selected-global-mode 1)
        (rich-text-set-region-keymap)
        (add-hook 'post-command-hook 'rich-text-use-region-keyhint)
        (add-hook 'find-file-hook #'rich-text-restore-buffer-ov)
        (add-hook 'after-save-hook #'rich-text-store-buffer-ov))
    (selected-global-mode -1)
    (remove-hook 'post-command-hook 'rich-text-use-region-keyhint)
    (remove-hook 'find-file-hook #'rich-text-restore-buffer-ov)
    (remove-hook 'after-save-hook #'rich-text-store-buffer-ov)))

(provide 'rich-text)
