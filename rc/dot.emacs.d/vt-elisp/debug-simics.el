;;; debug-simics.el --- debug a running Simics on the same machine

(require 'comint)

(defcustom cygwin-ps-program
  (if (bound-and-true-p cygwin-mount-activated)
      (cygwin-mount-substitute-longest-mount-name "/bin/ps")
    "c:\\cygwin\\bin\\ps.exe")
  "The path to Cygwin's /bin/ps program."
  :type 'string)

(defun for-all-windows-processes (func &rest ps-args)
  "Windows backend for `for-all-processes'.

Requires Cygwin's /bin/ps, as defined by `cygwin-ps-program'."

  (unless (zerop (apply 'call-process cygwin-ps-program nil "*ps-output*" nil
			"-W" ps-args))
    (error "Failed to get process list"))

  (save-current-buffer
    (set-buffer "*ps-output*")
    (beginning-of-buffer)

    (re-search-forward "COMMAND")
    (goto-char (match-beginning 0))
    (let ((command-column (current-column)))
      (while (re-search-forward "^I? *\\([0-9]+\\) +\\([0-9]+\\)" nil t)
	(let ((pid (match-string 1))
	      (ppid (match-string 2)))
	  (move-to-column command-column)
	  (and (= (current-column) command-column)
	       (looking-at "\\(.*[/\\\\]\\)?\\(.+\\)$")
	       (funcall func pid ppid (match-string 2))))))

    (kill-buffer nil)
    ))

(defun for-all-unix-processes (func &rest ps-args)
  "Unix-style backend for `for-all-processes'."

  (unless (zerop (apply 'call-process "/bin/ps" nil "*ps-output*" nil
			"-o" "pid,ppid,comm"
			ps-args))
    (error "Failed to get process list"))

  (save-current-buffer
    (set-buffer "*ps-output*")
    (beginning-of-buffer)
    (while (re-search-forward
	    "^ *\\([0-9]+\\) +\\([0-9]+\\) +\\([^ ]+/\\)?\\(.*\\)$"
	    nil t)
      (let ((pid (match-string 1))
	    (ppid (match-string 2))
	    (comm (match-string 4)))
	(funcall func pid ppid comm)))
    (kill-buffer nil)))

(defun for-all-processes (func &rest ps-args)
  "Calls (FUNC PID PPID COMM) for all current processes, where PID, PPID,
and COMM are strings.

PS-ARGS is a list of arguments that are passed on to /bin/ps."

  (apply (if (eq system-type 'windows-nt)
             'for-all-windows-processes
           'for-all-unix-processes)
         func ps-args))

(defun debug-application (exact pattern)
  "Debug an application by name or pattern.

When run interactively, it will ask for the name of the process to debug.

With a prefix argument, it will instead ask for a regular expression
that shall match the process name.

When run non-interactively, PATTERN holds the name (when EXACT is
non-nil) or the pattern (when EXACT is nil)."
  (interactive 
   (list
    (not current-prefix-arg)
    (if current-prefix-arg 
	(read-string "Command pattern: ")
      (completing-read 
       "Command name: "

       (let (names)
	 (for-all-processes
	  (lambda (pid ppid comm)
	    (unless (assoc comm names)
	      (setq names
		    (cons (cons comm nil)
			  names))))
	  "-u" (getenv "USER"))
	 names)
       nil t))))

  (require 'gud)

  (let (rpids pids)
    (for-all-processes
     (lambda (pid ppid comm)
       (when (funcall (if exact 'string-equal 'string-match) pattern comm)
	 (setq pids (cons (cons pid ppid) pids))))
     "-u" (getenv "USER"))

    (unless pids
      (error "No matching processes found"))

    (let ((p pids))
      (while p
	(unless (assoc (cdar p) pids)
	  (setq rpids (cons (caar p) rpids)))
	(setq p (cdr p))))

    (let ((pid
	   (if (null (cdr rpids))
	       (car rpids)
	     (completing-read "PID: "
			      (mapcar (lambda (pid) (cons pid nil)) rpids)
			      nil t nil nil
			      (apply #'min
				     (mapcar #'string-to-number rpids)))))
          (gdb-command (if (boundp 'gud-gdb-command-name)
                           gud-gdb-command-name
                         "gdb")))
      (if (stringp pid)
	  (setq pid (string-to-number pid)))
      (gdb (concat gdb-command (format " --pid=%s" pid))))
    ))

(defun debug-simics ()
  "Debug Simics running on the same machine by the same user."
  (interactive)
  (debug-application nil "^simics\\(-[^ ]+\\)?\\(\\.exe\\)?$"))
