(require 'emacsql)
(require 'emacsql-sqlite)

(defvar rich-text-db-file
  (expand-file-name "rich-text.db" user-emacs-directory))

;; FIXME: 将id替换为文件在系统中的唯一标识符
(defvar rich-text-db-models '((ov [id beg end props])))

(defvar rich-text-db--conn (make-hash-table :test #'equal)
  "Database connection to rich-text-db.")

(defun rich-text-db--get-conn ()
  "Return the rich-text database connection with key PATH."
  (gethash rich-text-db-file rich-text-db--conn))

(defun rich-text-db--init (db)
  "Initialize database DB with `rich-text-db--table-schemata'."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) rich-text-db-models)
      (emacsql db `[:create-table ,table ,schema]))))

(defun rich-text-db ()
  "Entrypoint to rich-text sqlite database."
  (unless (and (rich-text-db--get-conn)
               (emacsql-live-p (rich-text-db--get-conn)))
    (let ((init-db (not (file-exists-p rich-text-db-file))))
      (make-directory (file-name-directory rich-text-db-file) t)
      (let ((conn (emacsql-sqlite rich-text-db-file)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash rich-text-db-file conn rich-text-db--conn)
        (when init-db
          (rich-text-db--init conn)))))
  (rich-text-db--get-conn))

(defun rich-text-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for current rich-text db."
  (unless db
    (setq db (rich-text-db--get-conn)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun rich-text-db-clear ()
  "Clear all data in rich-text database."
  (interactive)
  (when (file-exists-p rich-text-db-file)
    (dolist (table (mapcar #'car rich-text-db--table-schemata))
      (rich-text-db-crud `[:delete :from ,table]))))

(defun rich-text-db-drop ()
  "Drop the whole rich-text database."
  (interactive)
  (rich-text-db--close)
  (delete-file rich-text-db-file))

(defun rich-text-db-crud (sql &rest args)
  "Return SQL query on rich-text database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if (stringp sql)
      (emacsql (rich-text-db) (apply #'format sql args))
    (apply #'emacsql (rich-text-db) sql args)))

(defun rich-text-db-query (table fields &optional conds)
  (if conds
      (rich-text-db-crud `[:select ,fields :from ,table :where ,conds ])
    (rich-text-db-crud `[:select ,fields :from ,table])))

(defun rich-text-db-query-count (table &optional conds)
  (if conds
      (caar (rich-text-db-crud `[:select (funcall count *) :from ,table :where ,conds]))
    (caar (rich-text-db-crud `[:select (funcall count *) :from ,table]))))

(defun rich-text-db-distinct-query (table fields &optional conds)
  (if conds
      (rich-text-db-crud `[:select :distinct ,fields :from ,table :where ,conds ])
    (rich-text-db-crud `[:select :distinct ,fields :from ,table])))

(defun rich-text-db-insert (table values)
  (rich-text-db-crud `[:insert :into ,table :values ,values]))

(defun rich-text-db-delete (table conds)
  (rich-text-db-crud `[:delete :from ,table :where ,conds]))

(defun rich-text-db-update (table conds)
  (rich-text-db-crud `[:delete :from ,table :where ,conds]))

(provide 'rich-text-db)
