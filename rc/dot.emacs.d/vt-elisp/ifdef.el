;;; ifdef.el --- Count ifdefs and stuff

(require 'cl)

(defcustom ifdef-do-highlight t
  "Non-nil if the ifdef hightlighting should be enabled.
When this is nil, `ifdef-highlight-mode' will only add key bindings."
  :type 'boolean
  :group 'ifdef)

(defvar ifdef-highlight-mode nil)
(make-variable-buffer-local 'ifdef-highlight-mode)
(push (list 'ifdef-highlight-mode " #if") minor-mode-alist)

(setplist 'ifdef-parse-error
          '(error-conditions (parse-error error) error-message "parse error"))

(defun cpp-tokenize (str)
  (if (string-match "\\`\\s-+" str)
      (setq str (substring str (match-end 0))))
  (let (tokens)
    (while (not (zerop (length str)))
      (cond ((string-match "\\`defined" str)
             (setq tokens (cons 'defined tokens)))
            ((string-match "\\`||" str)
             (setq tokens (cons 'or tokens)))
            ((string-match "\\`&&" str)
             (setq tokens (cons 'and tokens)))
            ((string-match "\\`==" str)
             (setq tokens (cons '= tokens)))
            ((string-match "\\`!=" str)
             (setq tokens (cons '/= tokens)))
            ((string-match "\\`\\(\\+\\|-\\|>=?\\|<=?\\)" str)
             (setq tokens (cons (intern (match-string 0 str)) tokens)))
            ((string-match "\\`!" str)
             (setq tokens (cons 'not tokens)))
            ((string-match "\\`(" str)
             (setq tokens (cons 'lpar tokens)))
            ((string-match "\\`)" str)
             (setq tokens (cons 'rpar tokens)))
            ((string-match "\\`," str)
             (setq tokens (cons 'comma tokens)))
            ((string-match "\\`[-+<>=_a-zA-Z][_a-zA-Z0-9]*" str)
             (setq tokens (cons (match-string 0 str) tokens)))
            ((string-match "\\`0x\\([0-9a-fA-F]+\\)[uU]?[lL]?[lL]?" str)
             (setq tokens (cons (string-to-number (match-string 1 str) 16)
				tokens)))
            ((string-match "\\`0[0-7]+[uU]?[lL]?[lL]?" str)
             (setq tokens (cons (string-to-number (match-string 0 str) 8)
				tokens)))
            ((string-match "\\`[0-9]+[uU]?[lL]?[lL]?" str)
             (setq tokens (cons (string-to-number (match-string 0 str))
				tokens)))
            ((string-match "\\`/\\*\\([^*]\\|\\*[^/]\\)*\\(\\*/\\|$\\)" str)
             ;; Ignore C comment
             nil)
            ((string-match "\\`//.*$" str)
             ;; Ignore C++ comment
             nil)
            (t
             (signal 'ifdef-parse-error (list str))))
      (setq str (substring str (match-end 0)))
      (if (string-match "\\`\\s-+" str)
          (setq str (substring str (match-end 0)))))
    (nreverse tokens)))

(defun parse-cpp-skip (tokens skip-token)
  (if (eq (car tokens) skip-token)
      (cdr tokens)
    (signal 'ifdef-parse-error tokens)))

(defun parse-cpp-constant (tokens)
  (cond ((stringp (car tokens))
         ;; This is a macro name, check for arguments
         (if (eq (cadr tokens) 'lpar)
             (let ((name (car tokens))
                   args)
               ;; Scan tokens for comma-separated expressions
               (setq tokens (cddr tokens))
               (while (not (eq (car tokens) 'rpar))
                 (let ((e (parse-cpp-expression tokens)))
                   (setq args (cons (car e) args))
                   (setq tokens (cdr e)))
                 (if (eq (car tokens) 'comma)
                     (setq tokens (cdr tokens))))
               (cons `(apply ,name . ,(nreverse args))
                     tokens))
           ;; No arguments
           (cons (list 'verbatim (car tokens))
                 (cdr tokens))))
        ((numberp (car tokens))
         (cons (list 'verbatim (number-to-string (car tokens)))
               (cdr tokens)))
        (t
         (signal 'ifdef-parse-error tokens))))

(defun parse-cpp-defined (tokens)
  (let ((tokens (parse-cpp-skip tokens 'defined)))
    (cond ((eq (car tokens) 'lpar)
           ;; defined(FOOBAR)
           (setq tokens (cdr tokens))
           (if (not (stringp (car tokens)))
               (signal 'ifdef-parse-error tokens))
           (cons `(defined ,(car tokens))
                 (parse-cpp-skip (cdr tokens) 'rpar)))
          ((stringp (car tokens))
           ;; defined FOOBAR
           (cons `(defined ,(car tokens))
                 (cdr tokens)))
          (t
           (signal 'ifdef-parse-error tokens)))))

(defun parse-cpp-atom (tokens)
  (cond ((eq (car tokens) 'defined)
         (parse-cpp-defined tokens))
        ((eq (car tokens) 'lpar)
         (let ((e (parse-cpp-expression (cdr tokens))))
           (cons (car e) (parse-cpp-skip (cdr e) 'rpar))))
        (t
         (parse-cpp-constant tokens))))
           
(defun parse-cpp-not (tokens)
  (let ((e (parse-cpp-atom (cdr tokens))))
    (cons (list 'not (car e))
          (cdr e))))
           
(defun parse-cpp-aexpression (tokens)
  (cond ((memq (car tokens) '(rpar or and))
         nil)
        (t
         (let ((e (parse-cpp-atom tokens)))
           (setq tokens (cdr e))
           (while (memq (car tokens) '(= /= < > <= >= + -))
               (let ((e2 (parse-cpp-atom (cdr tokens))))
                 (setq e (cons (list (car tokens) (car e) (car e2))
                               (cdr e2)))
                 (setq tokens (cdr e2))))
           e))))
           
(defun parse-cpp-and (tokens)
  (cond ((null tokens)
         nil)
        ((eq (car tokens) 'rpar)
         nil)
        (t
         (let ((next 'and)
               l subexps)
           (while (eq next 'and)
             (cond ((eq (car tokens) 'not)
                    (setq l (parse-cpp-not tokens))
                    (setq tokens (cdr l)))
                   (t
                    (setq l (parse-cpp-aexpression tokens))
                    (setq tokens (cdr l))))
             (setq subexps (cons (car l) subexps))
             (if tokens
                 (setq next (car tokens)
                       tokens (cdr tokens))
               (setq next nil)))
           (if next
               (setq tokens (cons next tokens)))
           (if (cdr subexps)
               (cons `(and ,@(nreverse subexps))
                     tokens)
             (cons (car subexps) tokens))))))
           
(defun parse-cpp-or (tokens)
  (cond ((null tokens)
         nil)
        ((eq (car tokens) 'rpar)
         nil)
        (t
         (let ((next 'or)
               l subexps)
           (while (eq next 'or)
             (setq l (parse-cpp-and tokens))
             (unless l
               (signal 'ifdef-parse-error tokens))
             (setq tokens (cdr l))
             (setq subexps (cons (car l) subexps))
             (if tokens
                 (setq next (car tokens)
                       tokens (cdr tokens))
               (setq next nil)))
           (if next
               (setq tokens (cons next tokens)))
           (if (cdr subexps)
               (cons `(or ,@(nreverse subexps))
                     tokens)
             (cons (car subexps) tokens))))))

(defun parse-cpp-expression (tokens)
  (parse-cpp-or tokens))

(defun parse-cpp-condition (cond-str)
  (let* ((tokens (cpp-tokenize cond-str))
         (p (parse-cpp-expression tokens)))
    (if (not p)
        `(verbatim ,(concat "(" cond-str ")"))
      (car p))))

(defun format-cpp-condition (condition &optional par)
  (let* ((kind (car condition))
         (data (cdr condition))
         (lp (if par "(" ""))
         (rp (if par ")" "")))
    (cond ((eq kind 'verbatim)
           (car data))
          ((eq kind 'apply)
           (concat (car data) "("
                   (mapconcat #'format-cpp-condition (cdr data) ",")
                   ")"))
          ((eq kind 'defined)
           (concat "defined(" (car data) ")"))
          ((memq kind '(or and))
           (let ((str (mapconcat '(lambda (x) (format-cpp-condition x t))
                                 data
                                 (if (eq kind 'or) " || " " && "))))
             (if par
                 (concat "(" str ")")
               str)))
          ((eq kind 'elif)
           (format-cpp-condition (list 'and (car data) (cons 'not (cdr data)))))
          ((eq kind 'not)
           ;; Handle !(A == B) and !(A != B) to make it more readable
           (cond ((eq (caar data) '=)
                  (format-cpp-condition (cons '/= (cdar data))))
                 ((eq (caar data) '/=)
                  (format-cpp-condition (cons '= (cdar data))))
                 (t
                  (concat "!" (format-cpp-condition (car data) t)))))
          ((eq kind '=)
           (concat lp (format-cpp-condition (nth 0 data) t) " == "
                   (format-cpp-condition (nth 1 data) t) rp))
          ((eq kind '/=)
           (concat lp (format-cpp-condition (nth 0 data) t) " != "
                   (format-cpp-condition (nth 1 data) t) rp))
          ((memq kind '(+ - < > <= >=))
           (concat lp (format-cpp-condition (nth 0 data) t) " "
                   (symbol-name kind) " "
                   (format-cpp-condition (nth 1 data) t) rp))
          ((eq kind 'expr)
           (mapconcat '(lambda (x) (format-cpp-condition x t))
                      data
                      " "))
          (t
           (error "Strange")))))
          
(defvar cpp-definitions nil
  "An assoc list of preprocessor definitions.
A nil value means that it is known to be undefined.")
(defvar cpp-fuzzy-definitions
  '(("HAVE_.*_H$" 1)
    ("_H\\(__\\)?$" nil)))


(defun cpp-is-defined (symbol)
  "Check if a preprocessor symbol is defined.
Returns a cons (symbol . value) where value can be nil for undefined
symbols, and an anything else for defined symbols.  If it is unknown,
nil is returned."
  (or (assoc symbol cpp-definitions)
      (assoc-default symbol cpp-fuzzy-definitions 'string-match)))


(defun cpp-eval-verbatim (val)
  (cond ((numberp val)
         val)
        ((stringp val)
         ;; Cleanup heuristics
         (if (string-match "\\`\\(.+\\)/\\*.*\\*/\\s-*\\'" val)
             (setq val (match-string 1 val)))
         (if (string-match "\\`\\(\\S-+\\)\\s-+\\'" val)
             (setq val (match-string 1 val)))
         (if (string-match "\\`(\\(.+\\))\\'" val)
             (setq val (match-string 1 val)))
         
         (if (string-match "\\`[0-9]+\\'" val)
             (string-to-number val)
           (let ((sym (cpp-is-defined val)))
             (if sym
                 (cpp-eval-verbatim (cdr sym))
               nil))))
        (t
         nil)))

(defun cpp-eval-and (clauses)
  (if (null clauses)
      1
    (let ((val1 (cpp-eval (car clauses)))
          (val2 (cpp-eval-and (cdr clauses))))
      (cond ((or (eq val1 0) (eq val2 0)) 0)
            ((or (not val1) (not val2)) nil)
            (t 1)))))

(defun cpp-eval-or (clauses)
  (if (null clauses)
      0
    (let ((val1 (cpp-eval (car clauses)))
          (val2 (cpp-eval-or (cdr clauses))))
      (cond ((or (eq val1 1) (eq val2 1)) 1)
            ((or (not val1) (not val2)) nil)
            (t 0)))))

(defun cpp-eval (condition)
  "Evaluate the CPP condition in CONDITION.
The returned value is an integer, or nil for unknown."
  (let* ((kind (car condition))
         (data (cdr condition)))
    (cond ((eq kind 'verbatim)
           (cpp-eval-verbatim (car data)))
          ((eq kind 'defined)
           (let ((sym (cpp-is-defined (car data))))
             (if sym (if (cdr sym) 1 0) nil)))

          ((eq kind 'and)
           (cpp-eval-and data))

          ((eq kind 'or)
           (cpp-eval-or data))

          ((eq kind 'elif)
           (cpp-eval-and (list (car data) (cons 'not (cdr data)))))

          ((eq kind 'not)
           (let ((val (cpp-eval (car data))))
             (if val
                 (if (zerop val) 1 0)
               nil)))

          ((memq kind '(+ -))
           (let ((val1 (cpp-eval (car data)))
                 (val2 (cpp-eval (car (cdr data)))))
             (if (or (not val1) (not val2))
                 nil
               (apply kind val1 val2 nil))))

          ((memq kind '(= /= < > <= >=))
           (let ((val1 (cpp-eval (car data)))
                 (val2 (cpp-eval (car (cdr data)))))
             (if (or (not val1) (not val2))
                 nil
               (if (apply kind val1 val2 nil) 1 0))))

          ((eq kind 'expr)
           nil)
          (t
           (error "Strange")))))


(defun get-cpp-line (pos)
  (let ((s ""))
    (save-excursion
      (goto-char pos)
      (while (looking-at "\\(.*\\)\\\\$")
        (setq s (concat s (match-string-no-properties 1)))
        (forward-line 1))
      (setq pos (point))
      (end-of-line)
      (setq s (concat s (buffer-substring-no-properties pos (point)))))
    s))

(defun cpp-conjunction (conditions)
  (if (cdr conditions)
      `(and ,@conditions)
    (car conditions)))

(defun cpp-elif-reduce (condition)
  (let ((kind (car condition)))
    (if (eq kind 'elif)
        (let ((last (nth 1 condition))
              (prev (nth 2 condition)))
          (if (eq (car prev) 'or)
              `(or ,last ,@(cdr prev))
            `(or ,last ,prev)))
      condition)))

(defun next-cpp-conditional ()
  (if (re-search-forward "^ *# *\\(\\(el\\|end\\)?if\\|ifn?def\\|else\\)\\> *" nil t)
      (intern (match-string-no-properties 1))
    nil))

(defun get-one-cpp-macro-name ()
  (unless (looking-at "\\=\\s-*\\([_a-zA-Z0-9]+\\)")
    (signal 'ifdef-parse-error (list "strange #if(n)def")))
  (match-string-no-properties 1))
    

(defun update-current-condition (directive conditions)
  (cond ((eq directive 'ifdef)
         (cons `(defined ,(get-one-cpp-macro-name))
               conditions))
        ((eq directive 'ifndef)
         (cons `(not (defined ,(get-one-cpp-macro-name)))
               conditions))
        ((eq directive 'if)
         (cons (parse-cpp-condition (get-cpp-line (point)))
               conditions))
        ((eq directive 'elif)
         (cons `(elif ,(parse-cpp-condition (get-cpp-line (match-end 0)))
                      ,(cpp-elif-reduce (car conditions)))
               (cdr conditions)))
        ((eq directive 'else)
         (let ((first (cpp-elif-reduce (car conditions))))
           (cons (cond ((null first)
                        (signal 'ifdef-parse-error (list "#else")))
                       ((eq (car first) 'not)
                        (car (cdr first)))
                       (t
                        `(not ,first)))
                 (cdr conditions))))
        ((eq directive 'endif)
         (cdr conditions))))

(defun current-cpp-condition (pos &optional insert)
  "Show the preprocessor conditions that need to hold for a line."
  (interactive "d\nP")
  (let (directive conditions)
    (save-excursion
      (goto-char pos)
      ;; This is to make the right comments on #else lines
      (beginning-of-line)
      (if (looking-at "#else")
          (forward-line))
      (setq pos (point))
      (goto-char (point-min))
      (while (and (setq directive (next-cpp-conditional))
                  (<= (point) pos))
        (setq conditions (update-current-condition directive conditions))))
    (if (not conditions)
        (message "no conditions")
      (if insert
          (insert (format "/* %s */" (format-cpp-condition (car conditions))))
        (message "%s" (format-cpp-condition (cpp-conjunction conditions)))))))

(defun rescan-cpp-conditions ()
  "Show the preprocessor conditions that need to hold for a line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((block-start (point))
          (modified (buffer-modified-p))
          (inhibit-read-only t)
          conditions directive)
      (remove-text-properties (point-min) (point-max) '(cpp-conditions nil))
      (set-buffer-modified-p modified)
      (condition-case e
          (unwind-protect
              (while (setq directive (next-cpp-conditional))
                (put-text-property block-start (line-beginning-position)
                                   'cpp-conditions (cpp-conjunction conditions))
                (setq conditions (update-current-condition directive conditions))
                (forward-line)
                (setq block-start (point)))
            (set-buffer-modified-p modified))
        (parse-error
         (message "Parse error at %s" (car (cdr e))))
        (error
         (message "Error when scanning #ifs: %s" (car (cdr e)))))))
  (maybe-highlight-cpp-blocks))

(let ((bgmode (cdr (assoc 'background-mode (frame-parameters)))))
  (cond
   ((and (eq bgmode 'dark) (>= emacs-major-version 21))
    (defconst cpp-unknown-face '(:background "gray20"))
    (defconst cpp-true-face '())
    (defconst cpp-false-face '(:background "gray10" :foreground "gray50")))
   ((and (eq bgmode 'dark) (eq emacs-major-version 20))
    (defconst cpp-unknown-face '(background-color . "gray20"))
    (defconst cpp-true-face '())
    (defconst cpp-false-face '(foreground-color . "gray50")))
   ((and (eq bgmode 'light) (>= emacs-major-version 21))
    (defconst cpp-unknown-face '(:background "gray70"))
    (defconst cpp-true-face '())
    (defconst cpp-false-face '(:background "white" :foreground "gray80")))
   ((and (eq bgmode 'light) (eq emacs-major-version 20))
    (defconst cpp-unknown-face '(background-color . "gray70"))
    (defconst cpp-true-face '())
    (defconst cpp-false-face '(foreground-color . "gray80")))
   (t
    (error "Uknown emacs"))))

;;(eval-when-compile
;;  (defconst cpp-unknown-face '())
;;  (defconst cpp-true-face '())
;;  (defconst cpp-false-face '()))

(defun cpp-condition-face (val)
  (cond ((not val)  cpp-unknown-face)
        ((eq val 0) cpp-false-face)
        (t          cpp-true-face)))

(defvar ifdef-highlighted-overlays nil)
(make-variable-buffer-local 'ifdef-highlighted-overlays)

(defun unhighlight-cpp-blocks ()
  (interactive)
  (mapcar 'delete-overlay ifdef-highlighted-overlays))

(defun highlight-cpp-blocks ()
  (interactive)
  (let ((pos1 (point-min))
        pos2 conditions)
    (unhighlight-cpp-blocks)
    (while (setq pos2 (next-single-property-change pos1 'cpp-conditions))
      (if conditions
          (let ((ovl (make-overlay pos1 pos2)))
            (push ovl ifdef-highlighted-overlays)
            (overlay-put ovl 'face (cpp-condition-face (cpp-eval conditions)))))
      (setq conditions (get-text-property pos2 'cpp-conditions))
      (setq pos1 pos2))))

(defun maybe-highlight-cpp-blocks ()
  (when ifdef-do-highlight
    (highlight-cpp-blocks)))

(defun highlight-cpp-blocks-all-buffers ()
  (save-excursion
  (let ((buffers (buffer-list)))
    (while buffers
      (set-buffer (car buffers))
      (if ifdef-highlight-mode
          (maybe-highlight-cpp-blocks))
      (setq buffers (cdr buffers))))))

(defun define-cpp-symbol (symbol &optional expansion)
  (interactive (list (read-string "Symbol to define: "
                                  (cons (or (thing-at-point 'symbol) "") 0))
                     (if current-prefix-arg
                         (read-string "Expansion: ")
                       "")))
  (let ((val (assoc symbol cpp-definitions)))
    (if val (setcdr val (or expansion ""))
      (setq cpp-definitions (cons (cons symbol (or expansion ""))
                                  cpp-definitions))))
  (highlight-cpp-blocks-all-buffers))

(defun define-cpp-symbol-from-source (pos)
  (interactive "d")
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (cond ((looking-at "\\s-*#define\\s-+")
           (let ((ln (get-cpp-line (match-end 0))))
             (string-match "\\([a-zA-Z0-9_]+\\)\\s-*\\(.*\\)" ln)
             (define-cpp-symbol (match-string 1 ln) (match-string 2 ln))))
          ((looking-at "\\s-*#undef\\s-+")
           (let ((ln (get-cpp-line (match-end 0))))
             (string-match "\\([a-zA-Z0-9_]+\\)\\s-*" ln)
             (undefine-cpp-symbol (match-string 1 ln)))))))

(defun define-cpp-symbol-from-region (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (define-cpp-symbol-from-source (point))
      (forward-line 1))))

(defun undefine-cpp-symbol (symbol)
  (interactive (list (read-string "Symbol to undefine: "
                                  (cons (or (thing-at-point 'symbol) "") 0))))
  (let ((val (assoc symbol cpp-definitions)))
    (if val (setcdr val nil)
      (setq cpp-definitions (cons (cons symbol nil) cpp-definitions))))
  (highlight-cpp-blocks-all-buffers))

(defun forget-cpp-symbol (symbol)
  (interactive (list (read-string "Symbol to forget: "
                                  (cons (or (thing-at-point 'symbol) "") 0))))
  (let ((val (assoc symbol cpp-definitions)))
    (if val
        (setq cpp-definitions (delete val cpp-definitions))))
  (highlight-cpp-blocks-all-buffers))

(defun toggle-ifdef-do-highlight ()
  (interactive)
  (unhighlight-cpp-blocks)
  (setq ifdef-do-highlight (not ifdef-do-highlight))
  (message "#if highlighting is %s" (if ifdef-do-highlight "on" "off"))
  (maybe-highlight-cpp-blocks))

;; ifdef-hightlight-mode

(defvar ifdef-highlight-mode-map nil)
(unless ifdef-highlight-mode-map
  (setq ifdef-highlight-mode-map (make-sparse-keymap))
  (define-key ifdef-highlight-mode-map "\C-c##" 'current-cpp-condition)
  (define-key ifdef-highlight-mode-map "\C-c#d" 'define-cpp-symbol)
  (define-key ifdef-highlight-mode-map "\C-c#D" 'define-cpp-symbol-from-source)
  (define-key ifdef-highlight-mode-map "\C-c#r" 'define-cpp-symbol-from-region)
  (define-key ifdef-highlight-mode-map "\C-c#u" 'undefine-cpp-symbol)
  (define-key ifdef-highlight-mode-map "\C-c#f" 'forget-cpp-symbol)
  (define-key ifdef-highlight-mode-map "\C-c#r" 'rescan-cpp-conditions)
  (define-key ifdef-highlight-mode-map "\C-c#e" 'edit-cpp-definitions)
  (define-key ifdef-highlight-mode-map "\C-c#h" 'toggle-ifdef-do-highlight)
  (push (cons 'ifdef-highlight-mode ifdef-highlight-mode-map) minor-mode-map-alist))

(defvar cpp-rescan-timer nil)

(defun ifdef-highlight-mode (&optional arg)
  "ifdef highlighting mode"
  (interactive "P")
  (if arg
      (setq ifdef-highlight-mode (eq arg 1))
    (setq ifdef-highlight-mode (not ifdef-highlight-mode)))
  (make-local-variable 'cpp-rescan-timer)
  (if ifdef-highlight-mode
      (progn
        (rescan-cpp-conditions)
        ;;(setq cpp-rescan-timer (run-with-idle-timer 2 t 'rescan-cpp-conditions))
        )
    (unhighlight-cpp-blocks)
    ;;(cancel-timer cpp-rescan-timer)
    ))
  
;; edit mode

(defun edit-cpp-toggle (pos)
  (interactive "d")
  (save-excursion
    (beginning-of-line)
    (unless (looking-at " *\\([^ ]+\\) *\\(TRUE\\|FALSE\\)$")
      (error "fepp"))
    (let* ((symbol (match-string 1))
           (state (string= (match-string 2) "TRUE"))
           (val (assoc symbol cpp-definitions)))
      (if val
          (setcdr val (not state))
        (error "fupp"))
      (replace-match (format "  %-30s  %s"
                             symbol
                             (if (not state) "TRUE" "FALSE")))))
  (highlight-cpp-blocks-all-buffers))
      


(defun edit-cpp-definitions ()
  (interactive)
  (let ((buf (get-buffer-create (format "*known cpp definitions*"
                                         (buffer-name))))
        (conditions cpp-definitions))
    (set-buffer buf)
    (local-set-key "\r" 'edit-cpp-toggle)
    (erase-buffer)
    (while conditions
      (insert (format "  %-30s  %s\n"
                      (caar conditions)
                      (if (cdar conditions) "TRUE" "FALSE")))
      (setq conditions (cdr conditions)))
    (display-buffer buf t)))

