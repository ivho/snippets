
(defvar sqlquery-command "mysql")

(defvar sqlquery-host nil "The database host")
(defvar sqlquery-db nil "The database")
(defvar sqlquery-user nil "The user to log in as")
(defvar sqlquery-password nil "The password to log in `sqlquery-user' with")
(defvar sqlquery-cmdline nil)
(defvar sqlquery-coding-system nil
  "If not nil, use this coding system for talking to the SQL server")

(defun sqlquery-init (host database user password &optional coding-system)
  "Set up parameters for talking to a database"
  (make-local-variable 'sqlquery-host)
  (make-local-variable 'sqlquery-db)
  (make-local-variable 'sqlquery-user)
  (make-local-variable 'sqlquery-password)
  (setq sqlquery-host host)
  (setq sqlquery-db database)
  (setq sqlquery-user user)
  (setq sqlquery-password password)
  (setq sqlquery-cmdline
        `("--batch" "-u" ,sqlquery-user
          "-h" ,sqlquery-host
          ,@(if password (list (concat "--password=" sqlquery-password)) '())
          ,sqlquery-db
	  "--default-character-set=UTF8"))
  (when coding-system
    (make-local-variable 'sqlquery-coding-system)
    (setq sqlquery-coding-system coding-system)))

(defun sqlquery-parse-reply (buf handler)
  (goto-char (point-min))
  (let ((continue t)
        (reply '()))
    ;; Skip header
    (forward-line 1)
    ;; Data
    (while (not (eobp))
      (setq reply (cons (funcall handler)
                        reply)))
    (nreverse reply)))

(defun sqlquery-sentinel (proc reason)
  ;; (message "sqlquery-sentinel: %s" reason)
  (when (string-match "exited abnormally with code .*" reason)
    (kill-buffer (process-buffer proc))
    (error "Lost contact with MySQL server.  Please try again.")))

(defun sqlquery-start (buf)
  (let ((proc (apply 'start-process "*sqlquery*"
                     buf sqlquery-command sqlquery-cmdline)))
    (when sqlquery-coding-system
      (set-process-coding-system proc
                                 sqlquery-coding-system
                                 sqlquery-coding-system))
    (set-process-sentinel proc 'sqlquery-sentinel)
    proc))

(defun sqlquery-query (query handler)
  ;; (message "%s" query)
  (save-excursion
    (let* ((buf (get-buffer-create " *sqlquery*"))
           (proc (sqlquery-start buf)))
      (set-buffer buf)
      (erase-buffer)
      (send-string proc (format "%s;\\q\n" query))
      (while (eq (process-status proc) 'run)
        (accept-process-output proc 1))
      (sqlquery-parse-reply buf handler))))

;;; High-level access functions

(defun sqlquery-compile-cols (columns)
  (if (null columns)
      '()
    (cons
     `(let ((str (buffer-substring (point) (1- (re-search-forward "[\t\n]")))))
        ,(if (and (consp (car columns)) (cdr (car columns)))
             (list (cdr (car columns)) 'str)
           'str))
     (sqlquery-compile-cols (cdr columns)))))

(defmacro sqlquery (table columns &optional condition order-by args callback)
  `(sqlquery-query
    (let* ((cond ,condition)
           (condstr (or cond "1")))
      (format (format ,(format "SELECT %s FROM %s WHERE %%s %s"
                               (mapconcat '(lambda (x)
                                             (symbol-name (if (consp x) (car x) x)))
                                          columns ",")
                               (symbol-name table)
                               (if order-by
                                   (format "ORDER BY %s" (eval order-by))
                                 ""))
                      condstr)
              ,@args ;; ,@(mapcar 'eval args)
              ))
    (lambda ()
      (let ((data (vector ,@(sqlquery-compile-cols columns))))
        ,(if callback (list callback 'data))
        data))))

(def-edebug-spec sqlquery
  (symbolp sexp &optional form form &rest sexp))

(provide 'sqlquery)
