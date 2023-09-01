(require 'cl-macs)
(require 'ov)
(require 'selected)
(require 'rich-text-db)

;;;; Variables

(defconst rich-text-bold-types
  '(ultra-bold extra-bold bold semi-bold normal semi-light light extra-light ultra-light)
  "A list of rich-text bold face types.")

(defconst rich-text-italic-types
  '(italic oblique normal reverse-italic reverse-oblique)
  "A list of rich-text italic face types.")

(defconst rich-text-underline-styles '(line wave)
  "A list of rich-text underline face styles.")

;;; headline

(defvar rich-text-headline-1-height 1.8
  "Default height of rich-text headline-1 face.")

(defvar rich-text-headline-2-height 1.4
  "Default height of rich-text headline-2 face.")

(defvar rich-text-headline-3-height 1.2
  "Default height of rich-text headline-2 face.")

;;; bold

(defvar rich-text-bold-type 'bold
  "Default type of rich-text bold face,
 should be one of the symbols in `rich-text-bold-types'.")

;;; italic

(defvar rich-text-italic-type 'italic
  "Default type of rich-text italic face,
 should be one of the symbols in `rich-text-italic-types'.")

;;; underline

(defvar rich-text-underline-style 'line
  "Default style of rich-text underline face,
 should be one of the symbols in `rich-text-underline-styles'.")

(defvar rich-text-underline-color "grey"
  "Default color of rich-text underline face.")

(defvar rich-text-underline-light-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
  "Preset a list of rich-text underline colors in light themes.")

(defvar rich-text-underline-dark-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
  "Preset a list of rich-text underline colors in dark themes.")

;;; font color

(defvar rich-text-font-color "blue"
  "Default color of rich-text font color face.")

(defvar rich-text-font-light-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
  "Preset a list of rich-text font colors in light themes.")

(defvar rich-text-font-dark-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
  "Preset a list of rich-text font colors in dark themes.")

;;; highlight

(defvar rich-text-highlight-color "yellow"
  "Default color of rich-text font highlight face.")

(defvar rich-text-highlight-light-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
  "Preset a list of rich-text highlight colors in light themes.")

(defvar rich-text-highlight-dark-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
  "Preset a list of rich-text highlight colors in dark themes.")

;;; mode map

(defvar rich-text-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for rich-text-mode.")

(defvar rich-text-selected-ignore-modes nil
  "List of major modes for which selected will not be turned on.")

(defvar rich-text-selected-key-alist
  '(("bj" . rich-text-render-bold-dwim))
  "Alist of key and the command which key binded to.
The key bindings are used when a region is active.")

;;;; Rich-text face properties

(defun rich-text-headline-1-props ()
  `(face (:height ,rich-text-headline-1-height :weight bold)))

(defun rich-text-headline-2-props ()
  `(face (:height ,rich-text-headline-2-height :weight bold)))

(defun rich-text-headline-3-props ()
  `(face (:height ,rich-text-headline-3-height :weight bold)))

(defun rich-text-bold-props ()
  `(face (:weight ,rich-text-bold-type)))

(defun rich-text-italic-props ()
  `(face (:slant ,rich-text-italic-type)))

(defun rich-text-underline-props ()
  `(face (:underline (:color ,rich-text-underline-color :style ,rich-text-underline-style))))

(defun rich-text-fontcolor-props ()
  `(face (:foreground ,rich-text-font-color)))

(defun rich-text-highlight-props ()
  `(face (:background ,rich-text-highlight-color)))

;;;; Specific for light/dark themes

(defun rich-text-theme-dark-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun rich-text-theme-light-p ()
  (eq (frame-parameter nil 'background-mode) 'light))

(defun rich-text-underline-colors-by-theme ()
  "Return a list of underline colors according to the type of theme."
  (cond
   ((rich-text-theme-light-p) rich-text-underline-light-colors)
   ((rich-text-theme-dark-p) rich-text-underline-dark-colors)))

(defun rich-text-font-colors-by-theme ()
  "Return a list of font colors according to the type of theme."
  (cond
   ((rich-text-theme-light-p) rich-text-font-light-colors)
   ((rich-text-theme-dark-p) rich-text-font-dark-colors)))

(defun rich-text-highlight-colors-by-theme ()
  "Return a list of highlight colors according to the type of theme."
  (cond
   ((rich-text-theme-light-p) rich-text-highlight-light-colors)
   ((rich-text-theme-dark-p) rich-text-highlight-dark-colors)))

(defun rich-text-propertize-colors-by-theme (face-type)
  (pcase face-type
    ('underline
     (mapcar (lambda (color)
               (propertize color 'face `(:underline (:color ,color))))
             (rich-text-underline-colors-by-theme)))
    ('fontcolor
     (mapcar (lambda (color)
               (propertize color 'face `(:foreground ,color)))
             (rich-text-font-colors-by-theme)))
    ('highlight
     (mapcar (lambda (color)
               (propertize color 'face `(:background ,color)))
             (rich-text-highlight-colors-by-theme)))
    (_ (rich-text-font-colors-by-theme))))

;;;; Rich-text render functions

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

;;;###autoload
(defun rich-text-render-headline-1 ()
  "Render region or line with rich-text headline-1 face."
  (interactive)
  (rich-text-ov-set-dwim (rich-text-headline-1-props)))

;;;###autoload
(defun rich-text-render-headline-2 ()
  "Render region or line with rich-text headline-2 face."
  (interactive)
  (rich-text-ov-set-dwim (rich-text-headline-2-props)))

;;;###autoload
(defun rich-text-render-headline-3 ()
  "Render region or line with rich-text headline-3 face."
  (interactive)
  (rich-text-ov-set-dwim (rich-text-headline-3-props)))

;;;###autoload
(defun rich-text-render-bold (arg)
  "Render region or line with face attribute :weight.
Defaultly use `rich-text-bold-type'. If ARG is non-nil,
 choose a bold type from `rich-text-bold-types'."
  (interactive "P")
  (if arg
      (let ((type (completing-read "Choose a bold type: " rich-text-bold-types nil t)))
        (rich-text-ov-set-dwim `(face (:weight ,(intern type)))))
    (rich-text-ov-set-dwim (rich-text-bold-props))))

;;;###autoload
(defun rich-text-render-bold-dwim ()
  (interactive)
  (rich-text-render-bold 1))

;;;###autoload
(defun rich-text-render-italic (arg)
  "Render region or line with face attribute :slant.
Defaultly use `rich-text-italic-type'. If ARG is non-nil,
 choose a bold type from `rich-text-italic-types'."
  (interactive "P")
  (if arg
      (let ((type (completing-read "Choose an italic type: " rich-text-italic-types nil t)))
        (rich-text-ov-set-dwim `(face (:slant ,(intern type)))))
    (rich-text-ov-set-dwim (rich-text-italic-props))))

;;;###autoload
(defun rich-text-render-italic-dwim ()
  (interactive)
  (rich-text-render-italic 1))

;;;###autoload
(defun rich-text-render-underline (arg)
  "Render region or line with face attribute :slant.
 Defaultly use `rich-text-underline-color' and `rich-text-underline-style'.
 If ARG is non-nil, choose a bold type from `rich-text-underline-styles'
 and choose a underline color from function `rich-text-underline-colors-by-theme'."
  (interactive "P")
  (if arg
      (let ((color (completing-read
                    "Choose an underline color: "
                    (rich-text-propertize-colors-by-theme 'underline)))
            (style (completing-read
                    "Choose an underline style: " '(line wave))))
        (rich-text-ov-set-dwim `(face (:underline (:color ,color :style ,(intern style))))))
    (rich-text-ov-set-dwim (rich-text-underline-props))))

;;;###autoload
(defun rich-text-render-underline-dwim ()
  (interactive)
  (rich-text-render-underline 1))

;;;###autoload
(defun rich-text-render-fontcolor (arg)
  "Render region or line font with color.
 Defaultly use `rich-text-font-color'. If ARG is non-nil,
 choose a font color from function `rich-text-font-colors-by-theme'."
  (interactive "P")
  (if arg
      (let ((color (completing-read "Choose a font color: "
                                    (rich-text-propertize-colors-by-theme 'fontcolor))))
        (rich-text-ov-set-dwim `(face (:foreground ,color))))
    (rich-text-ov-set-dwim (rich-text-fontcolor-props))))

;;;###autoload
(defun rich-text-render-fontcolor-dwim ()
  (interactive)
  (rich-text-render-fontcolor 1))

;;;###autoload
(defun rich-text-render-highlight (arg)
  "Render region or line with highlight color.
 Defaultly use `rich-text-highlight-color'. If ARG is non-nil,
 choose a highlight color from function `rich-text-highlight-colors-by-theme'."
  (interactive "P")
  (if arg
      (let ((color (completing-read "Choose a highlight color: "
                                    (rich-text-propertize-colors-by-theme 'highlight))))
        (rich-text-ov-set-dwim `(face (:background ,color))))
    (rich-text-ov-set-dwim (rich-text-highlight-props))))

;;;###autoload
(defun rich-text-render-highlight-dwim ()
  (interactive)
  (rich-text-render-highlight 1))

;;;; Rich text ov store and restore functons

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

;;;###autoload
(defun rich-text-restore-buffer-ov ()
  (interactive)
  (when-let* ((id (rich-text-buffer-or-file-id))
              (_ (rich-text-buffer-stored-p id))
              (specs (rich-text-db-query 'ov [beg end props]
                                         `(= id ,id))))
    (mapcar (lambda (spec)
              (ov (nth 0 spec) (nth 1 spec) (nth 2 spec)))
            specs)
    (message "%s rich-text overlays restored!" (length specs))))

;;;; Rich text mode

;;; when use region to render

(defun keys-of-command (command keymap)
  "Return all key bindins of COMMAND defined in KEYMAP."
  (mapcar (lambda (vector-keys)
            (kbd (key-description vector-keys)))
          (where-is-internal command keymap)))

(defun unset-command-keys (keymap command)
  (mapcar (lambda (key)
            (define-key keymap key nil))
          (keys-of-command command keymap)))

(defun override-keys (alist keymap)
  "Override keybindings in KEYMAP with cons-cell in ALIST.
ALIST consists with key and command."
  (mapcar (lambda (key-cmd)
            (let ((key (car key-cmd))
                  (cmd (cdr key-cmd)))
              (unset-command-keys keymap cmd)
              (define-key keymap key cmd)))
          alist))

(defun rich-text-set-region-keymap ()
  (define-key selected-keymap "h1" #'rich-text-render-headline-1)
  (define-key selected-keymap "h2" #'rich-text-render-headline-2)
  (define-key selected-keymap "h3" #'rich-text-render-headline-3)
  (define-key selected-keymap "bb" #'rich-text-render-bold)
  (define-key selected-keymap "ii" #'rich-text-render-italic)
  (define-key selected-keymap "uu" #'rich-text-render-underline)
  (define-key selected-keymap "cc" #'rich-text-render-fontcolor)
  (define-key selected-keymap "vv" #'rich-text-render-highlight)
  (define-key selected-keymap "b1" #'rich-text-render-bold-dwim)
  (define-key selected-keymap "i1" #'rich-text-render-italic-dwim)
  (define-key selected-keymap "u1" #'rich-text-render-underline-dwim)
  (define-key selected-keymap "c1" #'rich-text-render-fontcolor-dwim)
  (define-key selected-keymap "v1" #'rich-text-render-highlight-dwim)
  ;; remove all key bindings to command defined in
  ;; `rich-text-selected-key-alist' and bind to a new key.
  (override-keys rich-text-selected-key-alist selected-keymap))

(defun rich-text-use-region-keyhint ()
  "Rich text keybinding hint when a region active."
  (unless (or (minibufferp)
              (when selected-ignore-modes
                (apply #'derived-mode-p selected-ignore-modes)))
    (when (use-region-p)
      (message "[h]headline [b]bold [i]italic [u]underline [c]color [v]highlight"))))

;;;###autoload
(define-minor-mode rich-text-mode
  "Minor mode for showing rich text in buffer."
  :lighter "RT"
  :global t
  :keymap rich-text-mode-map
  (if rich-text-mode
      (progn
        (selected-global-mode 1)
        (setq selected-ignore-modes rich-text-selected-ignore-modes)
        (rich-text-set-region-keymap)
        (add-hook 'post-command-hook 'rich-text-use-region-keyhint)
        (add-hook 'find-file-hook #'rich-text-restore-buffer-ov)
        (add-hook 'after-save-hook #'rich-text-store-buffer-ov))
    (selected-global-mode -1)
    (remove-hook 'post-command-hook 'rich-text-use-region-keyhint)
    (remove-hook 'find-file-hook #'rich-text-restore-buffer-ov)
    (remove-hook 'after-save-hook #'rich-text-store-buffer-ov)))

(cl-defmacro define-rich-text-face (name &key key props)
  (let* ((func-prefix "rich-text-render-")
         (render-func (intern (concat "rich-text-render-"
                                      (symbol-name name)))))
    `(progn
       (defun ,render-func ()
         (interactive)
         (rich-text-ov-set-dwim ',props))
       (unset-command-keys selected-keymap ',render-func)
       (define-key selected-keymap ,key #',render-func))))

(provide 'rich-text)
