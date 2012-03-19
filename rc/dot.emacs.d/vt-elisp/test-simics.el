(defconst simics-test-result-column 30)

(defvar last-simics-test nil)

(defgroup test-simics nil
  "Settings for the test-simics package")

(defcustom test-simics-python
  (cond ((eq system-type 'windows-nt) "C:/Python24/python")
        ((eq system-type 'cygwin) "/cygdrive/c/python24/python")
	((eq system-type 'darwin) "python")
        (t "python2.4"))
  "The Python binary to invoke dotest with."
  :group 'test-simics
  :type 'string)

(defvar simics-base-dir nil)
(defvar simics-log-dir nil)
(defvar simics-test-name nil)
(defvar simics-test-host nil)
(defvar simics-test-target nil)

(defun guess-simics-host-type ()
  (cond ((string-match "x86_64-.*-linux" system-configuration)
         "amd64-linux")
        ((string-match "i[3-6]86-.*-linux-gnu" system-configuration)
         "x86-linux")
        ((string-match "i386-mingw-.*" system-configuration)
         "x86-win32")
        ((string-match "i686-pc-cygwin" system-configuration)
         "x86-win32")
        ((string-match "sparc-sun-solaris.*" system-configuration)
         "sparc64-solaris")
	((string-match "mac-apple-darwin" system-configuration)
	 "x86-osx")
        (t
         (error "unknown host type; system-configuration = %s"
                system-configuration))))

(defvar last-simics-test-dir nil)

(defun set-simics-test-local-vars (simics-dir test-name host-type
                                              target log-dir)
  (setq simics-base-dir simics-dir)
  (setq simics-test-name test-name)
  (setq simics-test-host host-type)
  (setq simics-test-target target)
  (setq simics-log-dir log-dir))

(define-derived-mode test-simics-mode fundamental-mode "Simics Test"
  (mapc 'make-local-variable
        '(simics-base-dir simics-test-name simics-test-host
          simics-test-target simics-log-dir expected-failures test-proc
          output-mark)))

(defun quit-simics-test ()
  "Hide the test buffer."
  (interactive)
  (bury-buffer))

(define-key test-simics-mode-map " " 'abort-simics-test)
(define-key test-simics-mode-map "k" 'kill-simics-test)
(define-key test-simics-mode-map "\C-c\C-k" 'kill-simics-test)
(define-key test-simics-mode-map "g" 'restart-simics-test)
(define-key test-simics-mode-map "l" 'open-simics-test-log)
(define-key test-simics-mode-map "\C-m" 'find-simics-subtest-log)
(define-key test-simics-mode-map "n" 'next-unexpected-simics-test-result)
(define-key test-simics-mode-map "p" 'previous-unexpected-simics-test-result)
(define-key test-simics-mode-map "q" 'quit-simics-test)

(defun next-unexpected-simics-test-result ()
  "Move to the next unexpected test result."
  (interactive)
  (end-of-line)
  (let ((p (next-single-property-change (point)
                                        'unexpected-simics-test-result)))
    (unless p
      (beginning-of-line)
      (error "No more unexpected results"))
    (goto-char p)))

(defun previous-unexpected-simics-test-result ()
  "Move to the previous unexpected test result."
  (interactive)
  (beginning-of-line)
  (let ((p (previous-single-property-change (point)
                                            'unexpected-simics-test-result)))
    (unless p (error "No more unexpected results"))
    (goto-char p)
    (beginning-of-line)))

(defun test-simics (test-dir test-name target &optional log-dir host-type)
  (interactive
   (let* ((dir (file-name-as-directory
                (read-file-name "Test directory: "
                                last-simics-test-dir
				last-simics-test-dir nil)))
          (test (read-test-name dir))
          (default-host-type (guess-simics-host-type))
          (target (read-test-target dir test)))
     (list dir test target
           (if current-prefix-arg
               (read-file-name "Log directory: ")
             nil)
           (if current-prefix-arg
               (read-string "Host type: " default-host-type)
             default-host-type))))
  (setq test-dir (file-name-as-directory (expand-file-name test-dir)))
  (setq last-simics-test-dir test-dir)
  (let ((buf (get-buffer-create "*simics test*"))
        (inhibit-read-only t))
    (display-buffer buf)
    (with-current-buffer buf
      (test-simics-mode)
      (let* ((simics-dir (file-name-directory (directory-file-name test-dir)))
             (host-dir (concat simics-dir host-type)))
        (setq default-directory test-dir)
        (setq test-dir (directory-file-name test-dir))
        (unless log-dir
          (setq log-dir (concat simics-dir "logs/test/")))
        (setq log-dir (directory-file-name (expand-file-name log-dir)))
        (erase-buffer)
        (setq buffer-read-only t)

        (set-simics-test-local-vars simics-dir test-name host-type
                                    target log-dir)

        (setq expected-failures
              (parse-test-failures
               (concat simics-dir "test/common/mp-failures")
               test-name host-type target))

        (insert (format "Test: %s %s/%s"
                        test-name host-type target))
        (newline)

        (setq test-proc
              (let ((process-environment (cons "DOTEST_VERBOSE=t"
                                               process-environment)))
                (start-process "simics test" buf
                               test-simics-python
                               "scripts/dotest.py"
                               host-type target host-dir test-dir
                               log-dir test-name)))
        (setq output-mark (point-marker))
        (set-process-filter test-proc 'test-simics-initial-filter)
        (set-process-sentinel test-proc 'test-simics-sentinel)
        (setq mode-line-process ": run")))))

(defun simics-deviations ()
  (if (looking-at "======= Running tests from .* ===========")
      (forward-line))
  (while (looking-at "\\([^ ]+\\) \\([-\\*][-\\*][-\\*].*\\)\n")
    (let* ((subtest (intern (match-string 1)))
           (result (match-string 2))
           (expected (gethash subtest expected-failures))
           face
           has-error)
      (goto-char (match-beginning 2))
      (if (< (current-column) simics-test-result-column)
          (insert-char ?\  (- simics-test-result-column (current-column))))
      (cond ((or (string= result "*** failed (aborted) ***")
                 (string= result "*** failed (ignored) ***"))
             ;; Ignore these
             (setq expected nil)
             (setq face (list :foreground "gray")))
            ((and expected (string= result "--- passed ---"))
             (setq has-error t
                   face (list :background "dark green"
                              :foreground "white")))
            ((and (stringp expected) (not (string= expected result)))
             (setq has-error t
                   face (list :background "tomato"
                              :foreground "white")))
            (expected
             (setq expected nil))
            ((= (elt result 0) ?*)
             (setq has-error t
                   face (list :background "dark red"
                              :foreground "white")))
            (t
             (setq expected nil)))
      (when expected
        (end-of-line)
        (newline)
        (insert-before-markers "           expected "  expected))
      (forward-line)
      (when face
        (put-text-property (match-beginning 0)
                           (point)
                           'face face))
      (when has-error
        (put-text-property (match-beginning 0)
                           (1+ (match-beginning 0))
                           'unexpected-simics-test-result has-error))
      (forward-line -1))
    (forward-line 1)))

(defun mark-test-deviations (simics-dir test-name host-type target)
  (interactive
   (let* ((path (reverse (split-string default-directory "/")))
          (guessed-dir
           (if (string-match "\\(.*/\\)logs/test/[^/]+/[^/]+/[^/]+/"
                             default-directory)
               (match-string 1 default-directory)
             default-directory))
          (dir (file-name-as-directory
                (read-file-name "Simics directory: "
                                guessed-dir nil nil "")))
          (test-name (read-test-name dir (elt path 1)))
          (host-type (read-string "Host type: " (elt path 2)))
          (target (read-string "Target: " (elt path 0))))
     (list dir test-name host-type target)))
  (let ((was-modified (buffer-modified-p))
        (inhibit-read-only t))
    (test-simics-mode)

    (set-simics-test-local-vars simics-dir test-name host-type target
                                (format "%s/logs/test" simics-base-dir))

    (setq expected-failures
          (parse-test-failures
           (concat simics-dir "test/common/mp-failures")
           test-name host-type target))
    (save-excursion
      (goto-char (point-min))
      (simics-deviations))
    (set-buffer-modified-p was-modified)))  

(defun test-simics-initial-filter (proc str)
  (let ((inhibit-read-only t))
    (save-excursion
      (set-buffer (process-buffer proc))
      (goto-char (process-mark proc))
      (insert-before-markers str)
      (goto-char (point-min))
      (forward-line 1)
      (set-marker output-mark (point))
      (cond ((looking-at "DOTEST .*\n")
             (message "New-style output")
             (delete-region (match-beginning 0) (match-end 0))
             (test-simics-verbose-filter proc "")
             (set-process-filter proc 'test-simics-verbose-filter))
            ((looking-at ".*\n")
             (message "Old-style output")
             (set-process-filter proc 'test-simics-log-filter))
            (t
             nil)))))
           
(defun test-simics-verbose-one ()
  (cond
   ((looking-at "\\(BEGIN \\)\\(.*\\)\n")
    (delete-region (match-beginning 1) (match-end 1))
    (end-of-line)
    (insert " ")
    (if (< (current-column) simics-test-result-column)
        (insert-char ?\  (- simics-test-result-column (current-column))))
    (insert "... running ...")
    (forward-line 1)
    t)

   ((looking-at "RESULT \\(\\S-*\\) \\(.*\\)\n")
    (let* ((subtest (intern (match-string 1)))
           (result (match-string 2))
           (expected (gethash subtest expected-failures))
           face
           has-error)
      
      (if (and (save-excursion
                 (forward-line -1)
                 (looking-at
                  "\\(\\S-*\\)\\s-+\\(\\.\\.\\. running \\.\\.\\.\\)"))
               (string= (match-string 1) subtest))
          (progn
            (replace-match result nil nil nil 2)
            (delete-region (point) (line-end-position 2))))

      (cond ((or (string= result "*** failed (aborted) ***")
                 (string= result "*** failed (ignored) ***"))
             ;; Ignore these
             (setq expected nil)
             (setq face (list :foreground "gray")))
            ((and expected (string= result "--- passed ---"))
             (setq has-error t
                   face (list :background "dark green"
                              :foreground "white"))
             (if (not (stringp expected))
                 (setq expected nil)))
            ((and (stringp expected) (not (string= expected result)))
             (setq has-error t
                   face (list :background "tomato"
                              :foreground "white")))
            (expected
             (setq expected nil))
            ((= (elt result 0) ?*)
             (setq has-error t
                   face (list :background "dark red"
                              :foreground "white")))
            (t
             (setq expected nil)))

      (let ((start (line-beginning-position)))
        (when expected
          (newline)
          (insert-before-markers "           expected "  expected))
        (forward-line)
        (when face
          (put-text-property start
                             (point)
                             'face face))
        (when has-error
          (put-text-property (match-beginning 0)
                             (1+ (match-beginning 0))
                             'unexpected-simics-test-result has-error))))
    t)

   ((looking-at "END .*\n")
    (delete-region (match-beginning 0) (match-end 0))
    t)

   ((looking-at ".*\n")
    (forward-line 1))

   (t
    nil)))

(defvar test-simics-in-verbose nil)
(defun test-simics-verbose-filter (proc str)
  (let ((inhibit-read-only t))
    (save-excursion
      (set-buffer (process-buffer proc))
      (goto-char (process-mark proc))
      (let ((pos (marker-position output-mark)))
        ;; We want to use insert-before markers here to e.g. keep the
        ;; visible point moving, but we don't want to move output-mark
        ;; just yet.
        (insert-before-markers str)
        (goto-char pos))
      (unless test-simics-in-verbose
        (let ((test-simics-in-verbose t))
          (while (test-simics-verbose-one)
            )))
      (set-marker output-mark (point))
      (set-marker (process-mark proc) (point-max)))))

(defun test-simics-log-filter (proc str)
  (let ((inhibit-read-only t))
    (save-excursion
      (set-buffer (process-buffer proc))
      (goto-char (process-mark proc))
      (let ((pos (marker-position output-mark)))
        ;; We want to use insert-before markers here to e.g. keep the
        ;; visible point moving, but we don't want to move output-mark
        ;; just yet.
        (insert-before-markers str)
        (goto-char pos))
      (simics-deviations)
      (set-marker output-mark (point))
      (set-marker (process-mark proc) (point-max)))))

(defun test-simics-sentinel (proc reason)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    ;; (insert-before-markers "Test " reason)
    (let ((status (process-status proc)))
      (setq mode-line-process (format ": %s" status)))))                   

(defun abort-simics-test ()
  (interactive)
  (interrupt-process test-proc))

(defun kill-simics-test ()
  (interactive)
  (kill-process test-proc))

(defun restart-simics-test ()
  (interactive)
  (if (eq (process-status test-proc) 'run)
      (kill-process test-proc))
  (while (eq (process-status test-proc) 'run)
    (accept-process-output test-proc 1))
  (sit-for 0)
  (goto-char (point-min))
  (test-simics default-directory simics-test-name simics-test-target
               simics-log-dir simics-test-host))

(defun read-test-name (test-dir &optional default-test)
  (unless default-test
    (setq default-test (car last-simics-test)))
  (completing-read (if default-test
                       (format "Test (%s): " default-test)
                     "Test: ")
                   'test-names nil t nil 'last-simics-test default-test))
    

(defun test-names (string predicate all)
  (let ((tests (mapcar 'list (directory-files test-dir nil
					      "^t[0-9]+.*\\|target-test"))))
    (if all
        (all-completions string tests predicate)
      (try-completion string tests predicate))))

(defun unique-elements (list)
  "LIST with duplicates removed, using `equal' for comparisons."
  (let (set)
    (mapc #'(lambda (elem)
	      (unless (member elem set)
		(setq set (cons elem set))))
	  list)
    set))

(defvar test-target-history '())
(defvar last-test-target nil)

(defun read-test-target (test-dir test-name)
  (let* ((testinfo (parse-testfiles (concat test-dir "common/testfiles")))
         (targets (unique-elements (gethash test-name testinfo)))
         (default (or last-test-target (car targets))))
    (if (= (length targets) 1)
        default
      (completing-read (format "Target (%s): " default)
                       (mapcar 'list targets) nil nil
                       nil 'test-target-history default))))
                           

(defun parse-test-failures (failures-file test-dir host-type target)
  (with-current-buffer (find-file-noselect failures-file)
    (save-excursion
      (goto-char (point-min))
      (let ((failures (make-hash-table))
            (match-oneline
             (concat "test\\s-+" test-dir
                     "\\(\\s-+host\\s-+\\([a-z0-9_-]+\\)\\)?"
                     "\\(\\s-+target\\s-+\\([a-z0-9_-]+\\)\\)?"
                     "\\s-+subtest\\s-+\\([a-zA-Z0-9_/.-]+\\)"
                     "\\s-*\\(#.*\\)?$"))
            (match-test-dir (format "Testdir: %s" test-dir))
            (match-host (format "Host: %s" host-type))
            (match-target (format "Target: %s" target))
            in-dir in-host in-target
            (active nil))
        (while (not (eobp))
          (cond ((looking-at match-oneline)
                 (let ((f-host (match-string 2))
                       (f-target (match-string 4))
                       (subtest (match-string 5)))
                   (if (and (or (null f-host)
                                (string= f-host host-type))
                            (or (null f-target)
                                (string= f-target target)))
                       (puthash (intern subtest)
                                t
                                failures))))
                ((looking-at match-test-dir)
                 (setq in-dir t
                       active t))
                ((looking-at "End testdir:")
                 (setq in-dir nil
                       active nil))

                ((looking-at match-host)
                 (setq in-host t))
                ((looking-at "Host:")
                 (setq in-host 'other
                       active nil))
                ((looking-at "End host:")
                 (setq in-host nil
                       active (and in-dir
                                   (not (eq in-target 'other)))))

                ((looking-at match-target)
                 (setq in-target t))
                ((looking-at "Target:")
                 (setq in-target 'other
                       active nil))
                ((looking-at "End target:")
                 (setq in-target nil
                       active (and in-dir
                                   (not (eq in-host 'other)))))
                ((not active)
                 )
                ((looking-at "\\([^ ]+\\) \\(\\(---\\|\\*\\*\\*\\) .*\\)")
                 (puthash (intern (match-string 1))
                          (match-string 2)
                          failures)))
          (forward-line 1))
        failures))))

(defun open-simics-test-log ()
  (interactive)
  (let ((simics-dir simics-base-dir)
        (test-name simics-test-name)
        (host-type simics-test-host)
        (target simics-test-target)
        (log-dir simics-log-dir))
    (switch-to-buffer-other-window
     (find-file-noselect (format "%s/%s/%s/%s/test.log"
                                 simics-log-dir
                                 simics-test-host
                                 simics-test-name
                                 simics-test-target)
                         t))
    (let ((inhibit-read-only t))
      (revert-buffer nil t))
    (test-log-mode)
    (set-simics-test-local-vars simics-dir test-name host-type
                                target log-dir)))

(define-derived-mode test-log-mode fundamental-mode "testlog"
  (toggle-read-only t)
  (compilation-minor-mode 1))
  
(define-key test-log-mode-map "c" 'debug-simics-core)

(defun find-simics-subtest-log ()
  (interactive)
  (let ((subtest (save-excursion
                   (beginning-of-line)
                   (if (looking-at "\\([^ ]+\\) +[-\\*][-\\*][-\\*].*\n")
                       (match-string 1)
                     (error "No test on this line")))))
    (open-simics-test-log)
    (goto-char (point-min))
    (re-search-forward (concat "^=BEGIN " (regexp-quote subtest) " -"))
    (beginning-of-line)
    (recenter 1)))

(defun parse-testfiles (file-name)
  (with-current-buffer (find-file-noselect file-name)
    (save-excursion
      (goto-char (point-min))
      (let ((targets (makehash 'equal))
            (tests (makehash 'equal)))
        ;; Parse targets
        (while (looking-at "\\([a-z0-9+-]+\\)\\s-+=\\s-+\\([A-Z0-9]+\\)\\s-+\\([a-z]+\\)")
          (let ((target (match-string 1))
                (abbrev (match-string 2))
                (owner (match-string 3)))
            (puthash abbrev target targets))
          (forward-line 1))

        ;; Parse test specs
        (while (not (eobp))
          (cond ((looking-at "\\s-*#")
                 nil)
                ((looking-at "\\s-*$")
                 nil)
                ((looking-at "\\([0-9]+\\)\\s-+\\(t[0-9]+_[a-z0-9_]*\\)\\s-+\\([a-z-]+\\)\\s-+\\([^|:\n]*\\)")
                 (let ((estimated-time (match-string 1))
                       (test-name (match-string 2))
                       (owner (match-string 3))
                       (test-targets (split-string (match-string 4))))
                   (puthash test-name
                            (append
                             (mapcar #'(lambda (abbr)
                                         (gethash abbr targets))
                                     test-targets)
                             (gethash test-name tests))
                            tests))))
          (forward-line 1))
        tests))))

(defun debug-simics-core ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "Core dump moved to \\(.*\\)")
        (let ((file (format "%s/%s/bin/simics-common"
                            simics-base-dir
                            simics-test-host))
              (core (match-string 1)))
          (gdb gud-gdb-command-name)
          (insert (format "file %s" file))
          (comint-send-input)
          (insert (format "core %s" core))
          (comint-send-input))
      (error "No core dump reference on current line"))))
