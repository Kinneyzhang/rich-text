(require 'cl-macs)
(require 'dash)
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

;; (defconst rich-text-underline-styles '(line wave)
;;   "A list of rich-text underline face styles.")

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

(defvar rich-text-underline-color "black"
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

(defvar rich-text-selected-key-alist nil
  "Alist of key and the command which key binded to.
The key bindings are used when a region is active.")

;;; theme background change hook

(defvar rich-text-theme-background-change-hook nil
  "Normal hook that is run after the background of theme changed.")

(defvar rich-text-alist nil)

;;;; Define rich text face

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

(cl-defmacro define-rich-text (name key props)
  (let* ((name-str (symbol-name name))
         (func-prefix "rich-text-render-")
         (render-func (intern (concat func-prefix name-str))))
    `(progn
       (defun ,render-func ()
         (interactive)
         (rich-text-ov-set-dwim ',name ,props))
       (unset-command-keys selected-keymap ',render-func)
       (define-key selected-keymap ,key #',render-func))))

(cl-defmacro define-rich-text-dwim (name key &key props light dark)
  (let* ((name-str (symbol-name name))
         (func-prefix "rich-text-render-")
         (render-func (intern (concat func-prefix name-str "-dwim"))))
    `(progn
       (when-let ((lst (assoc ',name rich-text-alist)))
         (setq rich-text-alist (remove lst rich-text-alist)))
       (add-to-list 'rich-text-alist
                    '(,name :key ,key :props ,props
                            :light ,light :dark ,dark))
       (defun ,render-func ()
         (interactive)
         (let (theme-props)
           (when (and ',light (rich-text-theme-light-p))
             (setq theme-props (or ',light ',props)))
           (when (and ',dark (rich-text-theme-dark-p))
             (setq theme-props (or ',dark ',props)))
           (if theme-props
               (progn
                 ;; at least one of light and dark props
                 (rich-text-ov-set-dwim ',name theme-props t))
             ;; no light or dark props
             (if ',props
                 (rich-text-ov-set-dwim ',name ',props)
               (pop-mark)
               (message "No rich-text to render.")))))
       (unset-command-keys selected-keymap ',render-func)
       (define-key selected-keymap ,key #',render-func))))

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

(defun rich-text-underline-line-props ()
  '(face (:underline (:style line))))

(defun rich-text-underline-wave-props ()
  '(face (:underline (:style wave))))

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

(defun rich-text-ov-set-dwim (name props &optional reverse-p)
  (let (ov)
    (if (use-region-p)
        (progn
          (setq ov (ov-set (ov-region) props))
          (setq ov (ov-set ov 'reverse-p reverse-p)))
      (setq ov (ov-set (ov-line) props))
      (setq ov (ov-set ov 'reverse-p reverse-p)))
    (setq ov (ov-set ov '(evaporate t)))
    ;; indicate a overlay set by rich text.
    (setq ov (ov-set ov `(rich-text ,name)))
    (rich-text-store-curr-ov (car (ov-spec ov)))))

(defun rich-text-get-props (rich-text-name)
  (let ((plist (cdr (assoc rich-text-name rich-text-alist))))
    (cond
     ((rich-text-theme-light-p) (or (plist-get plist :light)
                                    (plist-get plist :props)))
     ((rich-text-theme-dark-p) (or (plist-get plist :dark)
                                   (plist-get plist :props))))))

(defun rich-text-reverse ()
  (let ((ovs (ov-in 'reverse-p t)))
    (mapcar (lambda (ov)
              (let ((name (ov-val ov 'rich-text)))
                (ov-set ov (rich-text-get-props name))))
            ovs)))

(defun rich-text-reverse-all ()
  (save-window-excursion
    (mapcar (lambda (win)
              (select-window win)
              (rich-text-reverse))
            (window-list))))

(define-rich-text headline-1 "h1"
  (rich-text-headline-1-props))

(define-rich-text headline-2 "h2"
  (rich-text-headline-2-props))

(define-rich-text headline-3 "h3"
  (rich-text-headline-3-props))

(define-rich-text bold "bb"
  (rich-text-bold-props))

(define-rich-text italic "ii"
  (rich-text-italic-props))

(define-rich-text underline-line "ul"
  (rich-text-underline-line-props))

(define-rich-text underline-wave "uw"
  (rich-text-underline-wave-props))

(define-rich-text fontcolor "cc"
  (rich-text-fontcolor-props))

(define-rich-text highlight "vv"
  (rich-text-highlight-props))

(define-rich-text bold-italic "bi"
  '(face (:weight bold :slant italic)))

(define-rich-text bold-italic "bi"
  '(face (:weight bold :slant italic)))

(define-rich-text bold-underline "bu"
  '(face (:weight bold :underline t)))

(define-rich-text-dwim underline-line "uu"
  :props (face (:underline (:style wave)))
  :light (face (:underline (:style line))))

(define-rich-text-dwim highlight-test-1 "v1"
  :light (face (:background "#F7E987" :foreground "black"))
  :dark (face (:background "#C58940" :foreground "white")))

(defun rich-text-change-theme-background (orig-fun &rest args)
  "Advice functon when load a theme."
  (let ((before-bg (frame-parameter nil 'background-mode))
        after-bg)
    (apply orig-fun args)
    (setq after-bg (frame-parameter nil 'background-mode))
    (unless (eq before-bg after-bg)
      (run-hooks 'rich-text-theme-background-change-hook))))

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

(defun rich-text-set-region-keymap ()
  ;; remove all key bindings to command defined in
  ;; `rich-text-selected-key-alist' and bind to a new key.
  (override-keys rich-text-selected-key-alist selected-keymap))

(defun rich-text-use-region-keyhint (&rest _)
  "Rich text keybinding hint when a region active."
  (unless (or (minibufferp)
              (when selected-ignore-modes
                (apply #'derived-mode-p selected-ignore-modes)))
    (when (use-region-p)
      (message "[h]headline [b]bold [i]italic [u]underline \
[c]color [v]highlight"))))

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
        (advice-add #'handle-shift-selection :after #'rich-text-use-region-keyhint)
        (when (package-installed-p 'counsel)
          (advice-add #'counsel-load-theme :around #'rich-text-change-theme-background))
        (advice-add #'load-theme :around #'rich-text-change-theme-background)
        (rich-text-set-region-keymap)
        ;; when change the background of emacs theme.
        ;; reverse the ov
        (add-hook 'rich-text-theme-background-change-hook
                  #'rich-text-reverse-all)
        (add-hook 'find-file-hook #'rich-text-restore-buffer-ov)
        (add-hook 'after-save-hook #'rich-text-store-buffer-ov))
    (selected-global-mode -1)
    (advice-remove #'handle-shift-selection #'rich-text-use-region-keyhint)
    (advice-remove #'counsel-load-theme #'rich-text-change-theme-background)
    (advice-remove #'load-theme #'rich-text-change-theme-background)
    (remove-hook 'rich-text-theme-background-change-hook
                 #'rich-text-reverse-all)
    (remove-hook 'find-file-hook #'rich-text-restore-buffer-ov)
    (remove-hook 'after-save-hook #'rich-text-store-buffer-ov)))

(provide 'rich-text)
