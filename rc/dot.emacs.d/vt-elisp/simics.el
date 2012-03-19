;;; simics.el -- Simics interaction

;; Copyright (C) 2001 Virtutech AB

;; Author: David Kågedal <david@virtutech.se>
;; Maintainer: Virtutech
;; Keywords: processes

;; This file is not part of GNU Emacs

;;; Commentary:

;; This file lets users run Simics from inside Emacs.

;;; Code:

(defgroup simics nil
  "Running Simics from within Emacs buffers"
  :group 'processes)

(defcustom simics-prompt-idle "%P:simics> "
  "Simics prompt string when not running"
  :group 'simics)

(defcustom simics-prompt-run "%P:simics>> "
  "Simics prompt string when running"
  :group 'simics)

(defcustom simics-prompt-off "- "
  "Simics prompt string when it is inactive"
  :group 'simics)

(defface simics-prompt-face
  '((((background dark))
     (:foreground "light blue"
      :weight bold))
    (((background light))
     (:foreground "dark blue"
      :weight bold)))
  "Simics prompt face"
  :group 'simics)

(defface simics-command-face
  '((t
     (:weight bold)))
  "Simics command face"
  :group 'simics)

(defface simics-strange-output-face
  '((t
     (:foreground "gray" :slant italic)))
  "Simics strange output face"
  :group 'simics)

(defface simics-warning-face
  '((t
     (:foreground "white" :background "red" :weight bold)))
  "Simics warning output face"
  :group 'simics)

(defvar simics-running nil
  "A buffer-local flag indicating whether Simics is running")

(defvar simics-startup-complete nil
  "A buffer-local flag indicating whether Simics has completed startup")

(defvar simics-debug nil)
(defun simics-debug (format &rest args)
  (if simics-debug
      (with-current-buffer (get-buffer-create "*Simics debug*")
        (goto-char (point-max))
        (insert (apply 'format format args))
        (sit-for 0))))

;;(defun simics-makehash () (makehash 'eq))
;;(defalias 'simics-gethash 'gethash)
;;(defalias 'simics-puthash 'puthash)
(defun simics-makehash ()
  (list 'simics-hashtable))

(defun simics-gethash (key table)
  (cdr (assq key (cdr table))))

(defun simics-puthash (key value table)
  (let ((entry (assq key (cdr table))))
    (if entry
        (setcdr entry value)
      (setcdr table (cons (cons key value) (cdr table))))))

(defvar simics-clear-buffer nil
  "*Clear the Simics buffer before starting new processes in it")

(defvar simics-hidden-output nil)
(defvar simics-hiding-output nil)
(make-variable-buffer-local 'simics-hidden-output)
(make-variable-buffer-local 'simics-hiding-output)

(defvar simics-selected-processor nil)
(make-variable-buffer-local 'simics-selected-processor)

(defun simics-hidden-command (cmd)
  "Send a command to the simics process and return the output without
any output to the buffer."
  (error "not implemented"))


(defun simics-command-start ()
  "Return the start position of the current command"
  ;;(field-beginning)
  (let ((pos (previous-single-property-change (point) 'field)))
    (if (eq (get-text-property pos 'field) 'prompt)
        (point)
      pos)))

(defun simics-command-end ()
  "Return the end position of the current command"
  (or (next-single-property-change (point) 'field)
          (point-max)))

(defun strip-final-newline (str)
  (let ((len-1 (1- (length str))))
    (cond ((= len-1 -1) "")
          ((= (elt str len-1) ?\n)
           (substring str 0 len-1))
          (t str))))

(defun simics-command-line ()
  "Return the simics command around point"
  ;; (strip-final-newline (field-string-no-properties pos))
  (strip-final-newline
   (buffer-substring-no-properties
    (simics-command-start)
    (simics-command-end))))

(defun simics-set-command-line (cmd)
  "Replace the command line at POS with CMD"
  (delete-region (simics-command-start) (simics-command-end))
  (insert cmd))

(defun simics-dynamic-complete ()
  (interactive)
  (let* ((cmd (buffer-substring (simics-command-start) (point)))
         (reply (simics-sync-request 'complete cmd))
         (simics-filename-completion (not (zerop (car reply))))
         (completions (cadr reply)))
    (cond ((null completions)
           (ding))
          ((= (length completions) 1)
	   (let ((completion (car completions))
		 (old-undo-list (cons (point) buffer-undo-list)))
	     (setq buffer-undo-list nil)
	     ;; (simics-debug "FOO %s\n" (subseq buffer-undo-list 0 10))
	     (re-search-backward "[ \t\n%()]\\(.*\\)\\=")
	     (if (and simics-filename-completion
		      (file-directory-p completion))
		 (setq completion (concat completion "/"))
	       (setq completion (concat completion " ")))
	     (replace-match completion t t nil 1)
	     (setq buffer-undo-list
		   (append (cons nil (delete nil buffer-undo-list))
			   old-undo-list)))
	   t)
          (t
           (save-excursion (re-search-backward "[ \t\n%()]\\(.*\\)\\="))
           (let* ((old (match-string 1))
                  (new (try-completion old (mapcar 'list completions))))
             (if (string= old new)
                 (with-output-to-temp-buffer "*Completions*"
                   (display-completion-list completions))
               (replace-match new t t nil 1)))
           t))))
          

(defun simics-help-for-command ()
  (interactive)
  (let ((cmd (simics-command-line))
        help-cmd)
    (if (not (string-match "[^ ]+" cmd))
        (error "No command on line"))
    (setq help-cmd (concat "help " (match-string 0 cmd)))
    (simics-set-command-line help-cmd)
    (simics-send-current-command t)
    (simics-set-command-line cmd)))

(defvar simics-display-prompt-hook nil)

(defvar inhibit-simics-status nil)
(defun simics-output-update-status (data)
  (ding)
  (unless inhibit-simics-status
    (when (string-match "" data)
      (simics-debug "CR in output [%s]\n" data))
    (let ((inhibit-simics-status t))
      (save-excursion
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (if (re-search-backward "simics> \\=" nil t)
            (run-hooks 'simics-display-prompt-hook))))))

(defvar simics-buffer nil)

(defvar simics-output-mark nil)
(defvar simics-modscan-mark nil)
(defvar simics-license-mark nil)

(defvar simics-markup-state '(nil . nil))

(defvar simics-prompt-state 'off)
(defvar simics-prompt-marker nil)

(defvar simics-mode-hook nil
  "Hook run when entering simics-mode")

(defvar simics-mode-map nil)
(unless simics-mode-map
  (setq simics-mode-map (make-sparse-keymap))
  (define-key simics-mode-map "\C-m" 'simics-send-current-command)
  (define-key simics-mode-map "\C-a" 'simics-beginning-of-line)
  (define-key simics-mode-map "\C-d" 'simics-delete-or-send-eof)
  (define-key simics-mode-map "\C-c\C-v" 'simics-run)
  (define-key simics-mode-map "\C-c\C-c" 'simics-stop)
  (define-key simics-mode-map "\t" 'simics-dynamic-complete)
  (define-key simics-mode-map "\C-c\C-r" 'simics-restart)
  (define-key simics-mode-map "\C-c\C-k" 'simics-quit)
  (define-key simics-mode-map "\M-h" 'simics-help-for-command)
  (define-key simics-mode-map "\M-p" 'simics-previous-input)
  (define-key simics-mode-map "\M-n" 'simics-next-input)
  (define-key simics-mode-map "\M-r" 'simics-previous-matching-input)
  (define-key simics-mode-map "\C-c\C-p" 'simics-previous-prompt)
  (define-key simics-mode-map "\C-c\C-n" 'simics-next-prompt)
  )

(defun simics-mode ()
  "Major mode for interacting with Simics.

\\{simics-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'simics-mode)
  (setq mode-name "Simics")
  (use-local-map simics-mode-map)
  (make-local-variable 'simics-running)
  (setq simics-running nil)
  (make-local-variable 'simics-startup-complete)
  (setq simics-startup-complete nil)
  (make-local-variable 'simics-output-mark)
  (setq simics-output-mark (point-marker))
  (make-local-variable 'simics-prompt-marker)
  (setq simics-prompt-marker (make-marker))
  (set-marker-insertion-type simics-prompt-marker t)
  (make-local-variable 'list-buffers-directory)
  (setq list-buffers-directory (expand-file-name default-directory))
  (make-local-variable 'simics-buffer)
  (setq simics-buffer (current-buffer))
  (run-hooks 'simics-mode-hook))


(defvar simics-last-command-line nil)
(defvar simics-read-command-history nil)
(defvar simics-read-command-local-map nil
  "Keymap for minibuffer prompting of simics startup command.")
(require 'comint)
(if simics-read-command-local-map
    nil
  (setq simics-read-command-local-map (copy-keymap minibuffer-local-map))
  (define-key simics-read-command-local-map "\C-i" 'comint-dynamic-complete-filename))

(defun simics-read-command-line ()
  (read-from-minibuffer
   "Simics command: "
   (cond (simics-last-command-line)
         ((file-exists-p (concat default-directory "simics"))
          "./simics ")
         ((file-exists-p (concat default-directory "bin/simics"))
          "bin/simics ")
         (t
          "simics "))
   simics-read-command-local-map
   nil
   simics-read-command-history))

(defvar simics-process-buffer nil)
(make-variable-buffer-local 'simics-process-buffer)

(defvar simics-outstanding nil)
(make-variable-buffer-local 'simics-outstanding)

(defun simics-check-proc ()
  "Check if the processo connected to BUF is still alive"
  (and (buffer-live-p simics-process-buffer)
       (get-buffer-process simics-process-buffer)
       (eq (process-status (get-buffer-process simics-process-buffer))
           'run)))

(defun simics-kill-proc ()
  (interactive)
  (when (buffer-live-p simics-process-buffer)
    (let ((proc (get-buffer-process simics-process-buffer)))
      (when proc
        (condition-case e
            (simics-async-request 'quit)
          (t nil))
        (kill-process proc)))
    (kill-buffer simics-process-buffer)
    (setq simics-process-buffer nil)))

(defun simics-start-proc (interaction-buf name prog args)
  "Start the simics process in its own buffer"
  (unless simics-process-buffer
    (setq simics-process-buffer
          (get-buffer-create (concat " " name " process"))))
  (with-current-buffer simics-process-buffer
    (set-buffer-file-coding-system 'no-conversion)
    (set-buffer-multibyte nil)
    (setq simics-buffer interaction-buf))
  (simics-debug "simics buffer is <%s>\n" simics-buffer)
  (simics-debug "process buffer is <%s>\n" simics-process-buffer)
  (let ((process-environment (cons "TERM=vt100" process-environment))
        (process-connection-type nil))
    (let ((proc (apply 'start-process-shell-command
                       name simics-process-buffer prog
                       (append args '("-control-pipes" "3,4"
                                      "3<&0"
                                      "4>&1"
                                      "1>.simics.stdout"
                                      "2>.simics.stderr"
                                      )))))
      (unless proc
        (error "Could not start Simics"))
      (set-process-coding-system proc 'iso-latin-1 'iso-latin-1)
      ;;(set-process-coding-system proc 'no-conversion 'no-conversion)
      (set-process-filter proc 'simics-output-filter)
      (set-process-sentinel proc 'simics-sentinel)
      )))

;;;###autoload
(defun simics (&optional command-line)
  "Run Simics"
  (interactive (list (simics-read-command-line)))
  (when (or (not (simics-check-proc))
            (y-or-n-p "Really restart? "))
    (simics-kill-proc)
    (setq simics-last-command-line command-line)
    (let* ( ;;(name (format "simics: %s" command-line))
           (name "simics")
           (bufname (concat "*" name "*"))
           (argv (split-string command-line))
           (progfile (expand-file-name (car argv)))
           (cwd default-directory)
           (buf (get-buffer-create bufname)))
      (set-window-buffer (selected-window) buf)
      (when (or (not (simics-check-proc))
                (if (yes-or-no-p "Already running. Restart? ")
                    (progn (simics-kill-proc) t)
                  nil))
        (set-buffer buf)
        (erase-buffer)
        (cd cwd)
        (insert "Running in " cwd "\n" command-line "\n\n")
        (simics-mode)
        (setq mode-line-process ":stop"
              simics-prompt-state 'off
              simics-outstanding (simics-makehash))
        (simics-insert-prompt 'off)
        (add-hook 'kill-buffer-hook 'simics-kill-proc)
        (simics-start-proc buf name progfile (cdr argv))
        (simics-async-request-with-callback
         'pselect
         '(lambda (cpu)
            (setq simics-selected-processor (if (string= cpu "") nil cpu))))
        ))))

(defun simics-restart ()
  "Restart Simics"
  (interactive)
  (simics simics-last-command-line))

(defun simics-insert-prompt (state)
  (let ((pos (point))
        (prompt (symbol-value (intern
                               (concat "simics-prompt-"
                                       (symbol-name state))))))
    (insert prompt)
    (save-excursion
      (let ((end (point-marker))
            (cpu-string (or simics-selected-processor "-")))
        (goto-char pos)
        (while (search-forward "%P" end t)
          (replace-match cpu-string t t))))
    (set-marker simics-prompt-marker pos)
    (add-text-properties pos (point) '(face simics-prompt-face
                                       field prompt
                                       rear-nonsticky t))))

(defun simics-set-prompt-state (state &optional force)
  "Redraw the current prompt"
  (when (or force (not (eq state simics-prompt-state)))
    (save-excursion
      (goto-char simics-prompt-marker)
      (setq simics-prompt-state state)
      (simics-insert-prompt state)
      (delete-region (point) (or (next-single-property-change (point) 'field)
                                 (point-max))))))

(defun simics-beginning-of-line ()
  (interactive)
  (let ((pos (previous-single-property-change (point) 'field)))
    (beginning-of-line)
    (if (and pos (< (point) pos))
        (goto-char pos))))

(defun simics-send-eof ()
  "Send EOF to the simics process, thereby terminating it."
  (interactive)
  (process-send-eof (get-buffer-process simics-process-buffer)))

(defun simics-delete-or-send-eof ()
  "If at end of buffer, call `simics-send-eof', otherwise, call `delete-char'."
  (interactive)
  (if (eobp)
      (simics-send-eof)
    (call-interactively 'delete-char)))

(defun simics-run ()
  "Run simulation"
  (interactive)
  (simics-async-request 'run))

(defun simics-stop ()
  "Stop simulation"
  (interactive)
  (simics-async-request 'break))

(defun simics-quit (&optional force)
  "Exit Simics.
If prefix argument FORCE is non-NIL, do it forcefully."
  (interactive "P")
  (if force
      (simics-kill-proc)
    (simics-async-request 'quit)))

(defun simics-gdb ()
  "Start gdb and attach it to the current Simics process"
  (interactive)
  (let ((pid (car (simics-sync-request 'pid))))
    (gdb (if (boundp 'gud-gdb-command-name)
             gud-gdb-command-name
           "gdb"))
    (insert (format "attach %d" pid))
    (comint-send-input)))
  

(defconst simics-byte-order
  (if (string-match "\\`\\(i[3-6]86\\|x86_64\\)-" system-configuration)
      'little-endian
    'big-endian))

(defun simics-encode-int32 (n)
  (let* ((b1 (mod n 256))  (n1 (/ n 256))
         (b2 (mod n1 256)) (n2 (/ n1 256))
         (b3 (mod n2 256))
         (b4 (/ n2 256)))
    (concat (if (eq simics-byte-order 'little-endian)
                 (vector b1 b2 b3 b4)
              (vector b4 b3 b2 b1)))))

(defun simics-encode-int64 (n)
  (let* ((b1 (mod n 256))  (n1 (/ n 256))
         (b2 (mod n1 256)) (n2 (/ n1 256))
         (b3 (mod n2 256)) (n3 (/ n2 256))
         (b4 (mod n3 256)) (n4 (/ n3 256))
         (b5 (mod n4 256)) (n5 (/ n4 256))
         (b6 (mod n5 256)) (n6 (/ n5 256))
         (b7 (mod n6 256))
         (b8 (/ n6 256)))
    (concat (if (eq simics-byte-order 'little-endian)
                 (vector b1 b2 b3 b4 b5 b6 b7 b8)
              (vector b8 b7 b6 b5 b4 b3 b2 b1)))))

(defun simics-encode (data)
  (cond ((numberp data)
         (concat [0]
                 (simics-encode-int64 data)))
        ((listp data)
         (concat [1]
                 (simics-encode-int32 (length data))
                 (mapconcat 'simics-encode data "")))
        ((stringp data)
         (concat [2]
                 (simics-encode-int32 (length data))
                 data))
        (t
         (error "Can't encode" data))))

(defsubst get-simics-process ()
  (or (get-buffer-process simics-process-buffer)
      (error "No simics process")))

(defun simics-send-request (req)
  (let ((msg (simics-encode req)))
    (setq msg (concat (simics-encode-int32 (length msg)) msg))
    (simics-debug ">> %S [%s]\n" req msg)
    (process-send-string (get-simics-process) msg)))

(defun simics-sync-request (request-type &rest args)
  "Send a synchronous request and wait for the response"
  (simics-add-outstanding 1)
  (simics-send-request `(,(symbol-name request-type) 1 ,@args))
  (cdr (simics-wait-for-reply 1)))

(defvar simics-last-async-id 1)
(make-variable-buffer-local 'simics-last-async-id)
(defun simics-async-request (request-type &rest args)
  "Send an asynchronous request and return the request id"
  (setq simics-last-async-id (1+ simics-last-async-id))
  (simics-send-request `(,(symbol-name request-type)
                         ,simics-last-async-id
                         ,@args))
  simics-last-async-id)

(defun simics-async-request-with-callback (request-type callback &rest args)
  "Send an asynchronous request and return the request id.
The callback function will be called when the reply is received."
  (let ((id (apply 'simics-async-request request-type args)))
    (simics-add-outstanding id callback)
    id))


(defun simics-send-command (cmd)
  "Send a cli command"
  (interactive "sCommand: ")
  ;; (simics-async-request 'set-screen-size (window-height) (window-width))
  (simics-async-request 'cli cmd))

(defvar simics-input-history '())
(defvar simics-input-history-scanner '(nil . nil))

(defun simics-send-current-command (&optional no-history)
  "Send the command after the prompt Simics"
  (interactive)
  (if (not simics-startup-complete)
      (error "Wait until Simics has started"))
  (let ((cmd (simics-command-line)))
    (end-of-line)
    (unless (eobp)
      (goto-char (point-max))
      (simics-set-command-line cmd))
    (insert "\n")
    (add-text-properties (simics-command-start) (point)
                         '(field command face simics-command-face
                           front-sticky t))
    (set-marker simics-output-mark (point))
    (simics-insert-prompt 'off)
    (simics-set-prompt-state 'off)
    (unless no-history
      (when (and (not (string= cmd ""))
                 (not (string= cmd (car simics-input-history))))
        (setq simics-input-history (cons cmd simics-input-history)))
      (setq simics-input-history-scanner (cons simics-input-history nil)))
    (simics-send-command cmd)))

(defun simics-previous-input ()
  "Move back one step in the command history"
  (interactive)
  (if (not (car simics-input-history-scanner))
      (error "No previous input")
    (let ((current (simics-command-line)))
      (simics-set-command-line (car (car simics-input-history-scanner)))
      (setq simics-input-history-scanner
            (cons (cdr (car simics-input-history-scanner))
                  (cons current (cdr simics-input-history-scanner)))))))

(defun simics-next-input ()
  "Move forward one step in the command history"
  (interactive)
  (if (not (cdr simics-input-history-scanner))
      (error "No next input")
    (let ((current (simics-command-line)))
      (simics-set-command-line (car (cdr simics-input-history-scanner)))
      (setq simics-input-history-scanner
            (cons (cons current (car simics-input-history-scanner))
                  (cdr (cdr simics-input-history-scanner)))))))

(defun simics-previous-matching-input (re)
  "Find a command in the command history"
  (interactive "sPrevious input matching (regexp): ")
    (let ((scanner (cons (car simics-input-history-scanner)
                         (cons (simics-command-line)
                               (cdr simics-input-history-scanner))))
          (match nil))
      (while (and (car scanner) (null match))
        (if (string-match re (caar scanner))
            (setq match (caar scanner))
          (setq scanner (cons (cdar scanner)
                              (cons (caar scanner) (cdr scanner))))))
      (if (null match)
          (error "No match")
      (simics-set-command-line match)
      (setq simics-input-history-scanner
            (cons (cdar scanner)
                  (cdr scanner))))))

(defun simics-previous-prompt (&optional arg)
  "Jump to the previous prompt"
  (interactive "p")
  (let ((inhibit-field-text-motion t))
    (while (> arg 0)
      (let ((pos (1- (line-beginning-position))))
        (while (not (eq (get-text-property pos 'field) 'prompt))
          (setq pos (previous-single-property-change pos 'field))
          (if (not pos) (error "No previous prompt")))
        (setq arg (1- arg))
        (goto-char (next-single-property-change pos 'field))))))

(defun simics-next-prompt (&optional arg)
  "Jump to the next prompt"
  (interactive "p")
  (let ((inhibit-field-text-motion t))
    (while (> arg 0)
      (let ((pos (point)))
        (while (not (eq (get-text-property pos 'field) 'prompt))
          (setq pos (next-single-property-change pos 'field))
          (if (not pos) (error "No next prompt")))
        (setq arg (1- arg))
        (goto-char (or (next-single-property-change pos 'field)
                       (point-max)))))))

;;;
;;; BSFP decoder
;;;

(defun simics-int (int64)
  "Convert internal integer representation to an Emacs integer"
  (if (numberp int64)
      int64
    (or (cdr int64)
        (let ((bytes (car int64)))
          (if (not (and (zerop (aref bytes 0))
                        (zerop (aref bytes 1))
                        (zerop (aref bytes 2))
                        (zerop (aref bytes 3))))
              (error "Too large integer")
            (setcdr int64 (+ (* (aref bytes 4) 16777216)
                             (* (aref bytes 5) 65536)
                             (* (aref bytes 6) 256)
                             (* (aref bytes 7)))))))))

(defun simics-decode-int64 ()
  (let ((bytes (vconcat (buffer-substring-no-properties
                         (point) (+ (point) 8)))))
    (forward-char 8)
    (if (eq simics-byte-order 'little-endian)
        (setq bytes (vector (aref bytes 7)
                            (aref bytes 6)
                            (aref bytes 5)
                            (aref bytes 4)
                            (aref bytes 3)
                            (aref bytes 2)
                            (aref bytes 1)
                            (aref bytes 0))))
    (if (and (zerop (aref bytes 0))
             (zerop (aref bytes 1))
             (zerop (aref bytes 2))
             (zerop (aref bytes 3))
             (zerop (aref bytes 4)))
        (simics-int (cons bytes nil))
      (cons bytes nil))))

(defun simics-decode-int32 ()
  (let ((bytes (vconcat (buffer-substring-no-properties
                         (point) (+ (point) 4)))))
    (forward-char 4)
    (if (eq simics-byte-order 'little-endian)
        (setq bytes (vector 0 0 0 0
                            (aref bytes 3)
                            (aref bytes 2)
                            (aref bytes 1)
                            (aref bytes 0)))
      (setq bytes (vconcat [0 0 0 0] bytes)))
    (simics-int
     (cons bytes nil))))

(defun simics-decode-list ()
  (let ((len (simics-int (simics-decode-int32)))
        l)
    (while (> len 0)
      (setq l (cons (simics-decode-object) l)
            len (1- len)))
    (reverse l)))

(defun simics-decode-string ()
  (let ((len (simics-int (simics-decode-int32))))
    (prog1 (buffer-substring-no-properties (point) (+ (point) len))
      (forward-char len))))

(defun simics-decode-object ()
  (let ((type (char-after)))
    (forward-char 1)
    (cond ((= type 0)
           (simics-decode-int64))
          ((= type 1)
           (simics-decode-list))
          ((= type 2)
           (simics-decode-string)))))

(defun simics-decode-msg ()
  (if (< (point-max) (+ (point) 4))
      nil
    (let* ((pos (point))
           (len (simics-int (simics-decode-int32))))
      (if (< (point-max) (+ (point) len))
          (progn
            ;; incomplete
            (goto-char pos)
            nil)
        (if (/= (char-after) 1)
            (error "parse error (not a list)")
          (forward-char 1)
          (simics-decode-list))))))

;;;
;;; Filter functinos
;;;

(defvar simics-displays '())
(make-variable-buffer-local 'simics-displays)


(defvar simics-output-filter-buf nil)
(defvar simics-output-filter-buf-incomplete "")
(defvar in-simics-output-filter nil)

(defun simics-sentinel (proc msg)
  (save-excursion
    ;; (simics-insert-output (concat "SENTINEL: " msg) 'simics-strange-output-face)
    (let ((status (process-status proc))
          (exit-status (process-exit-status proc)))
      ;; (simics-insert-output (format "Status: %S\n" status) 'simics-strange-output-face)
      (cond ((memq status '(exit signal))
             (setq mode-line-process ":exit")
             (simics-set-prompt-state 'off)
             (accept-process-output proc 10 500)
             (goto-char (or simics-output-mark (point-max)))
             (set-buffer (process-buffer proc))
             (with-current-buffer simics-buffer
               (if (zerop exit-status)
                   (insert "done\n")
                 (insert (format "exit status %d\n" exit-status)))
               ;;(delete-region (point) (point-max))
               ))))))

(defun simics-output-filter (proc str)
  ;; (simics-debug "simics-output-filter [%s]\n" str)

  (setq simics-output-filter-buf
        (concat simics-output-filter-buf str))

  ;; This magic allows us to singlestep through simics-output-filter-2
  ;; without getting recursive debugging, or reordered output.
  (unless in-simics-output-filter
    (let ((in-simics-output-filter t))
      (while simics-output-filter-buf
        (let ((str (concat simics-output-filter-buf-incomplete
                           simics-output-filter-buf)))
          (setq simics-output-filter-buf nil
                simics-output-filter-buf-incomplete "")
          (simics-output-filter-2 proc str))))))

(defvar simics-parser-mark nil)
(make-variable-buffer-local 'simics-parser-mark)

(defun simics-output-filter-2 (proc str)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-quit nil)
              data)
          (goto-char (point-max))
          (insert str)
          (unless simics-parser-mark
            (setq simics-parser-mark (make-marker))
            (set-marker simics-parser-mark 0))
          (goto-char simics-parser-mark)
          (while (setq data (simics-decode-msg))
            (with-current-buffer simics-buffer
              (simics-debug "<< %S\n" data)
              (simics-recv data)))
          (set-marker simics-parser-mark (point)))))))

(defun simics-parse-output ()
  "Parse one line of output at point"
  (cond ((looking-at "^(.*)\n")
         (let ((end (match-end 0)))
           (prog1
               (condition-case e
                   (simics-parse-data)
                 (t (end-of-line)
                    (insert "PARSE ERROR HERE")
                    (forward-line 1)))
             (goto-char end))))
        ((looking-at "^.*\n")
         (goto-char (match-end 0))
         (list "strange" 0 (match-string-no-properties 0)))
        (t
         nil)))
        
(defun simics-parse-string ()
  (let ((q (char-after))
        (re (concat "[^\\\\" (char-to-string (char-after)) "]+"))
        (s ""))
    (forward-char 1)
    (while (not (= (char-after) q))
      (cond ((looking-at "\\\\b") (setq s (concat s "\b")))
            ((looking-at "\\\\n") (setq s (concat s "\n")))
            ((looking-at "\\\\r") (setq s (concat s "\r")))
            ((looking-at "\\\\t") (setq s (concat s "\t")))
            ((looking-at "\\\\['\"\\\\]")
             (setq s (concat s (buffer-substring (1+ (point)) (match-end 0)))))
            ((looking-at "\\\\'")
             (setq s (concat s "'")))
            ((looking-at "\\\\\\([0-9][0-9][0-9]\\)")
             (setq s (concat s (char-to-string (string-to-number (match-string 1) 8)))))
            ((looking-at re)
             (setq s (concat s (match-string 0))))
            (t
             (error "Error in parser")))
      (goto-char (match-end 0)))
    (forward-char 1)
    s))

(defun simics-parse-number ()
  (prog1
      (string-to-number (match-string 0))
    (goto-char (match-end 0))))

(defun simics-parse-data ()
  (cond ((= (char-after) ?\()
         (let ((l '()))
           (forward-char 1)
           (while (not (= (char-after) ?\)))
             (setq l (cons (simics-parse-data) l))
             (simics-parse-skip "\\(, *\\)?"))
           (forward-char 1)
           (nreverse l)))
        ((or (= (char-after) ?\')
             (= (char-after) ?\"))
         (simics-parse-string))
        ((looking-at "[0-9]+")
         (simics-parse-number))
        (t
         (error "Parse error"))))

(defun simics-parse-skip (re)
  (if (looking-at re)
      (goto-char (match-end 0))
    (error "Parse error")))

(defun simics-fix-markup (start end)
  "Convert terminal codes to faces"
  (let ((end-marker (make-marker)))
    (set-marker end-marker end)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\[\\(.?\\)m" end t)
        (let ((s (match-string 1))
              state)
          (cond ((string= s "")
                 (setq state nil))
                ((string= s "1")
                 (setq state 'bold))
                ((string= s "4")
                 (setq state 'italic)))
          (delete-region (match-beginning 0) (match-end 0))
          (if (cdr simics-markup-state)
              (put-text-property (car simics-markup-state)
                                 (point)
                                 'face (cdr simics-markup-state)))
          (setq simics-markup-state
                (cons (point-marker)
                      state))))
      ;; Convert CRLF to LF
      (goto-char (1- start))
      (while (re-search-forward "\r\n" end-marker t)
        (replace-match "\n" nil t))
      ;; Treat backspace
      (goto-char (1- start))
      (while (re-search-forward "\b." end-marker t)
        (forward-char -1)
        (delete-char -2)))))
  

(defun simics-insert-output (str &optional face)
  (goto-char simics-output-mark)
  (insert str)
  (simics-fix-markup simics-output-mark (point))
  (if face
      (put-text-property simics-output-mark (point)
                         'face face))
  (set-marker simics-output-mark (point))
  ;; Make sure the prompt is on a line of its own
  (unless (eq simics-prompt-state 'off)
    (if (bolp)
        (if (< (point) simics-prompt-marker)
            (delete-char 1))
      (if (= (point) simics-prompt-marker)
          (insert "\n")))))

(defun simics-hap (hap args)
  (cond ((eq hap 'Core_Continuation)
         (setq simics-running t
               mode-line-process ":run")
         (simics-set-prompt-state 'run))
        ((eq hap 'Core_Simulation_Stopped)
         (setq simics-running nil
               mode-line-process ":stop")
         ;; (simics-set-prompt-state 'idle)
         )
        ((eq hap 'Core_At_Exit)
         ;; We do stuff in the sentinel instead
         nil)
        ((memq hap '(Core_Conf_Object_Create
                     Core_Configuration_Loaded
                     Core_Initial_Configuration))
         nil)
        (t
         (simics-insert-output (format "# Got hap %s\n" hap)
                               'simics-strange-output-face))))

(defvar simics-pc-functions nil)
(defvar simics-asm-functions nil)

(defun simics-recv (data)
  (save-excursion
    (let ((msg (intern (car data)))
          (id (car (cdr data)))
          (args (cdr (cdr data))))
      (cond ((eq msg 'stdout)
             (simics-insert-output (car args)))
            ((eq msg 'strange)
             (simics-insert-output (concat "# " (car args))
                                   'simics-strange-output-face))
            ((eq msg 'reply)
             (let ((outstanding (simics-gethash id simics-outstanding)))
               (cond ((not outstanding))
                     ((functionp outstanding)
                      (apply outstanding args))
                     ((consp outstanding)
                      (setcar outstanding t)
                      (setcdr outstanding args))
                     (t
                      (error "weird")))))
            ((eq msg 'ready)
             ;; (simics-insert-output "# ready\n" 'simics-strange-output-face)
             (if simics-startup-complete
                 (simics-set-prompt-state (if simics-running 'run 'idle))))
            ((eq msg 'modscan)
             (unless simics-modscan-mark
               (simics-insert-output "Scanning modules: ")
               (make-local-variable 'simics-modscan-mark)
               (setq simics-modscan-mark (copy-marker simics-output-mark))
               (simics-insert-output "\n"))
             (goto-char simics-modscan-mark)
             (end-of-line)
             (delete-region simics-modscan-mark (point))
             (insert (if (string= (car args) "")
                         "done"
                       (car args))))
            ((eq msg 'load-checkpoint-progress)
             (message (car args)))
            ((eq msg 'license)
             (unless simics-license-mark
               (simics-insert-output "License: ")
               (make-local-variable 'simics-license-mark)
               (setq simics-license-mark (copy-marker simics-output-mark))
               (simics-insert-output "\n"))
             (goto-char simics-license-mark)
             (end-of-line)
             (delete-region simics-license-mark (point))
             (insert (car args)))
            ((eq msg 'hap)
             (simics-hap (intern (car args)) (cdr args)))
            ;;((eq msg 'stopped)
            ;;  (delete-region simics-output-mark (point-max)))
            ((eq msg 'pc)
             (let ((f simics-pc-functions))
               (while f
                 (apply (car f) args)
                 (setq f (cdr f)))))
            ((eq msg 'asm)
             (let ((f simics-asm-functions))
               (while f
                 (apply (car f) args)
                 (setq f (cdr f)))))
            ((eq msg 'pselect)
             (setq simics-selected-processor (car args))
             (simics-set-prompt-state simics-prompt-state t))
            ((eq msg 'startup-complete)
             (setq simics-startup-complete t)
             (simics-set-prompt-state 'idle))
            ((eq msg 'startup-failure)
             (simics-insert-output (format "%s\n" (car args))
                                   'simics-warning-face))
            ((memq msg '(console-write console-cursor-left
                         console-delete-line))
             nil)
            (t
             (simics-insert-output (format "%S\n" data)
                                   'simics-strange-output-face))))))

(defun simics-add-outstanding (id &optional callback)
  (simics-puthash id (or callback (cons nil nil)) simics-outstanding))
  

(defun simics-get-reply (id)
  (simics-gethash id simics-outstanding))

(defun simics-wait-for-reply (id)
  (let ((outstanding (simics-gethash id simics-outstanding)))
    (while (not (car outstanding))
      (accept-process-output))
    outstanding))

;; Display buffers

(defvar simics-buffer nil)
(make-variable-buffer-local 'simics-buffer)

;;(defvar simics-display-command nil)

(define-derived-mode simics-display-mode fundamental-mode "Simics Display"
  "Mode for displaying Simics output

\\{simics-display-mode-map}"
  ;; (make-local-variable 'simics-display-command)
  )

;; (define-key simics-display-mode-map " " 'simics-display-update)


(defun simics-open-display (id)
  (let ((simics-buf (current-buffer))
        (buf (get-buffer-create (concat "*Simics display [" id "]*"))))
    (setq simics-displays (cons (cons id buf) simics-displays))
    (with-current-buffer buf
      (simics-display-mode)
      (setq simics-buffer simics-buf))
    (display-buffer buf)
    buf))

(defun simics-display-buffer (id)
  (let ((buf (cdr-safe (assoc id simics-displays))))
    (if (buffer-live-p buf)
        buf
      (simics-open-display id))))

(defun simics-display (command)
  (interactive "sCommand to display: ")
  (let* ((simics-buf (current-buffer))
        (id (substring (simics-hidden-command
                        (format "display '%s'" command))
                       0 4))
        (buf (simics-open-display id)))
    (simics-hidden-command "p 1")))

(add-to-list 'special-display-regexps
             '("\\*Simics display \\[.*\\]\\*" (minibuffer . nil)))

(defvar simics-asm-buffers nil)
(defvar simics-asm-pc nil)
(defvar simics-asm-cpu nil)

(defun create-simics-asm-buffer (cpu)
  (interactive "sCPU: ")
  (save-selected-window
    (let ((buf (get-buffer-create (concat "*" cpu "*")))
          (simics-buf simics-buffer))
      (select-window (display-buffer buf t))
      (erase-buffer)
      (simics-asm-mode)
      (setq simics-buffer simics-buf)
      (setq simics-asm-cpu cpu)
      (unless (memq 'simics-asm-pc simics-pc-functions)
        (setq simics-pc-functions (cons 'simics-asm-pc
                                        simics-pc-functions)))
      (unless (memq 'simics-asm-asm simics-asm-functions)
        (setq simics-asm-functions (cons 'simics-asm-asm
                                        simics-asm-functions)))
      (setq simics-asm-buffers (cons buf simics-asm-buffers))
      (simics-asm-update))))

(defun simics-asm-mode ()
  "Simics disassembly mode"
  (interactive)
  (fundamental-mode)
  (setq major-mode 'simics-asm-mode)
  (setq major-mode "Simics asm")
  (make-local-variable 'simics-asm-cpu)
  (make-local-variable 'simics-asm-pc))

(defun simics-asm-update ()
  (let ((cpu simics-asm-cpu))
    (save-excursion
      (set-buffer simics-buffer)
      (simics-send-request (list "getpc" cpu)))))

(defun simics-for-each-asm-buffer (cpu fun &rest args)
  (let ((bufs simics-asm-buffers))
    (while bufs
      (if (buffer-live-p (car bufs))
          (save-excursion
            (set-buffer (car bufs))
            (when (string= cpu simics-asm-cpu)
              (apply fun args)))
        (setq simics-asm-buffers (delete (car bufs)
                                         simics-asm-buffers)))
      (setq bufs (cdr bufs)))))  

(defun simics-asm-pc (cpu pc)
  (message "%s: %d" cpu pc)
  (simics-for-each-asm-buffer cpu 'simics-asm-update-pc pc))

(defun simics-asm-update-pc (pc)
  (setq simics-asm-pc pc)
  (let ((cpu simics-asm-cpu))
    (save-excursion
      (set-buffer simics-buffer)
      (simics-send-request (list "disassemble" cpu pc 4 3 "")))))

(defun simics-asm-asm (cpu &rest lines)
  (simics-for-each-asm-buffer cpu 'simics-asm-redraw lines))

(defun simics-asm-redraw (lines)
  (erase-buffer)
  (while lines
    (apply 'simics-insert-asm-line (car lines))
    (setq lines (cdr lines))))

(defun simics-insert-asm-line (va asm)
  (insert (format "%x: %s\n" va asm)))
