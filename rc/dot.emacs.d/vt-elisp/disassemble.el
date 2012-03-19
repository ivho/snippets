;;; disassemble.el --- Disassembly and source correlation

(put 'disasm-file-name 'invisible t)

(defvar objdump-program "objdump"
  "*objdump program to run")

(defvar disasm-obj-src-buffers nil
  "A list of all source buffers associated with this object buffer.")
(make-variable-buffer-local 'disasm-obj-src-buffers)

(defvar disasm-obj-orig-buffer nil
  "The buffer that was active when `disasm-file' was invoked.")
(make-variable-buffer-local 'disasm-obj-orig-buffer)

(defvar disasm-obj-symtable nil)
(make-variable-buffer-local 'disasm-obj-symtable)

(defvar disasm-src-overlay nil)
(make-variable-buffer-local 'disasm-src-overlay)

(defvar disasm-obj-overlays nil)
(make-variable-buffer-local 'disasm-obj-overlays)

(defvar disasm-obj-buffer nil)
(make-variable-buffer-local 'disasm-obj-buffer)

(defsubst disasm-current-line ()
  (if (eobp)
      (1+ (count-lines (point-min) (point)))
    (count-lines (point-min) (1+ (point)))))

(defun disasm-file (obj)
  "Disassemble a file"
  (interactive (list (let ((completion-ignored-extensions-not
                            (append '(".c" ".C" ".cc" ".h")
                                    (remove ".o" completion-ignored-extensions))))
                       (read-file-name "Object file: "
                                       nil nil t nil))))
  (let ((obj (expand-file-name obj))
        (orig-file (buffer-file-name))
        (orig-buf (current-buffer))
        (buf (get-buffer-create (format "*disassembly of %s*"
                                        (file-name-nondirectory obj))))
        (inhibit-read-only t)
        (line (disasm-current-line)))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (call-process objdump-program nil buf nil "-dlr" "--no-show-raw-insn" obj)
      (cd (file-name-directory obj))
      (disasm-obj-mode)
      (set-window-buffer (selected-window) buf)
      (disasm-scan)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (setq disasm-obj-orig-buffer orig-buf)
      (goto-char (point-min))
      ;; If orig-file is not a source, forget it
      (if (not (disasm-obj-find-src orig-file nil))
          (setq orig-file nil))
      ;(if (disasm-obj-find-src orig-file line)
      ;;    (disasm-obj-select (point) orig-file line)
      ;;  (message "Line %d in %s not found" line (file-name-nondirectory orig-file)))
      ;;(switch-to-buffer-other-window buf)
      (if orig-file
          (disasm-obj-get-src-buffer orig-file)))
    (if orig-file
        (disasm-src-select (point)))))

(defun disasm-scan ()
  "Scan the objdump output and fix stuff"
  (message "Scanning output...")
  (goto-char (point-min))
  (setq disasm-obj-symtable nil)
  
  ;; (search-forward "SYMBOL TABLE")
  ;; (forward-line 1)
  ;; (while (looking-at "^\\(........\\).......\\(.\\).*\t........ \\(.*\\)")
  ;;   (if (string= (match-string 2) "F")
  ;;       (setq disasm-obj-symtable
  ;;             (cons (cons (match-string-no-properties 3)
  ;;                         (string-to-number (match-string-no-properties 1) 16))
  ;;                   disasm-obj-symtable)))
  ;;   (forward-line 1))
  ;; (message "Scanning symbol table...done")

  ;; While we can find the next function
  (while (re-search-forward "^0*\\([0-9a-f]+\\) <\\(.*\\)>:$" nil t)
    (forward-line 2)
    (let ((function-address (string-to-number (match-string 1) 16))
          (function-name (match-string 2))
          (function-pos (point))
          src-file src-line)
      (setq disasm-obj-symtable (cons (list function-name
                                            function-address
                                            function-pos)
                                      disasm-obj-symtable))
      ;; Check for source line indicator
      (when (looking-at "\\(/.*\\):\\([0-9]+\\)$")
        (setq src-file (match-string 1)
              src-line (string-to-number (match-string 2)))
        (put-text-property (match-beginning 0) (1+ (match-end 0))
                           'category 'disasm-file-name)
        (forward-line 1))
      ;; While we can find the next instruction
      (while (looking-at " *\\([0-9a-f]+\\):\t.*\\(\\$?\\([0-9a-f]+\\) <[^>]+>\\)?")
        (forward-line 1)
        (let ((insn-address (match-string 1))
              (insn-relocation-address (match-string 3))
              (insn-relocation-start (match-beginning 2))
              (insn-relocation-end (match-end 2)))
          (while (looking-at "^\t\t\t\\([0-9a-f]+\\): \\([A-Z_0-9]+\\)\t\\(.*\\)$")
            (forward-line 1)
            (let ((relocation-address (match-string 1))
                  (relocation-type (match-string 2))
                  (relocation-symbol (match-string 3)))
              (delete-region (match-beginning 0) (1+ (match-end 0)))
              (if (string= insn-relocation-address
                           relocation-address)
                  (progn
                    (goto-char insn-relocation-start)
                    (delete-region insn-relocation-start insn-relocation-end)
                    (insert "<<" relocation-symbol ">>"))
                (backward-char)
                (insert " // " relocation-symbol)
                (forward-char)))))
        ;; Check for source line indicator
        (when (looking-at "\\(/.*\\):\\([0-9]+\\)$")
          (setq src-file (match-string 1)
                src-line (string-to-number (match-string 2)))
          (put-text-property (match-beginning 0) (1+ (match-end 0))
                             'category 'disasm-file-name)
          (forward-line 1)))))
    (message "Scanning output...done"))

;;   (message "Fixing relocations...")
;;   (while (re-search-forward
;;           "^\t\t\t\\([0-9a-f]+\\): \\([A-Z_0-9]+\\)\t\\(.*\\)$" nil 1)
;;     (let ((addr (match-string-no-properties 1))
;;           (type (match-string-no-properties 2))
;;           (value (match-string-no-properties 3)))
;;       (put-text-property 0 (length value) 'relocation (cons addr type)
;;                          value)
;;       (delete-region (match-beginning 0) (1+ (match-end 0)))
;;       (forward-line -1)
;;       (search-forward ":")
;;       (if (re-search-forward (concat "\\$?" addr "\\( <[^>]*>\\)?")
;;                              (line-end-position) t)
;;           (replace-match (concat "<<" value ">>") t t)
;;         (end-of-line)
;;         (insert "\t// " value))))
;;   (message "Fixing relocations...done")
;; 
;;   (disasm-mark-jump-sites)
;; 
;;   (message "Scanning source file references...")
;;   (goto-char (point-min))
;;   (while (disasm-obj-forward-src 1)
;;     (put-text-property (line-beginning-position) (1+ (line-end-position))
;;                        'category 'disasm-file-name))
;;   (message "Scanning source file references...done")
;;   )

(defun disasm-mark-jump-sites ()
  (message "Marking jumps...")
  (let ((buffer-lines (count-lines (point-min) (point-max)))
        (lines 0)
        (pos (point-min))
        (count 0))
    (while (re-search-forward "^ *\\([0-9a-f]+\\):\t.* \\$?\\([0-9a-f]+\\) <.*>"
                              nil t)
      (setq lines (+ lines (count-lines pos (point)))
            pos (point)
            count (1+ count))
      (message "Marking jumps...%d%%" (/ (* 100 lines) buffer-lines))
      (disasm-mark-jump (match-string-no-properties 1)
                        (match-string-no-properties 2)
                        pos))
    (message "Marking jumps...done (%d jumps)" count)))

(defun disasm-mark-jump (site target &optional site-pos target-pos)
  (unless site-pos
    (setq site-pos (disasm-obj-addr-pos site)))
  (unless target-pos
    (setq target-pos (disasm-obj-addr-pos target)))
  (save-excursion
    (goto-char site-pos)
    (put-text-property (line-beginning-position)
                       (line-end-position)
                       'jump-to
                       target)
    (goto-char target-pos)
    (let ((sites (get-text-property (point) 'jumps-to)))
      (put-text-property (line-beginning-position)
                         (line-end-position)
                         'jump-from
                         (cons site sites)))))
    
;;; Keymap for disasm-obj-mode

(defvar disasm-obj-mode-map nil
  "Keymap for disasm-obj-mode")

(if disasm-obj-mode-map
    nil
  (setq disasm-obj-mode-map (make-sparse-keymap))
  (define-key disasm-obj-mode-map "p" 'disasm-obj-previous-line)
  (define-key disasm-obj-mode-map "n" 'disasm-obj-next-line)
  (define-key disasm-obj-mode-map "P" 'disasm-obj-previous-function)
  (define-key disasm-obj-mode-map "N" 'disasm-obj-next-function)
  (define-key disasm-obj-mode-map "q" 'disasm-obj-quit)
  (define-key disasm-obj-mode-map "j" 'disasm-obj-show-jump)
  (define-key disasm-obj-mode-map "J" 'disasm-obj-show-jumps-to)
  (define-key disasm-obj-mode-map "f" 'disasm-obj-toggle-files))
          
;;; syntax table

;; (defvar disasm-obj-mode-syntax-table nil "The syntax table used in disasm-obj-mode.")
;; (if disasm-obj-mode-syntax-table
;;     nil
;;   (setq disasm-obj-mode-syntax-table (make-syntax-table)))

;;; font-lock

(require 'font-lock)

(defvar disasm-obj-mode-font-lock-keywords
  '(("^\\(/.*\\):\\([0-9]+\\)"
     (1 font-lock-comment-face t)
     (2 font-lock-constant-face t))
    ("^\\([_a-zA-Z][_a-zA-Z0-9]*\\)():"
     (1 font-lock-function-name-face t))
    ("<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\(\\+.*\\|\\)>"
     (1 font-lock-function-name-face t))
    ))

;;; Disassembly major mode

(defun disasm-obj-mode ()
  "Major mode for examining disassembly generated by the command `disasm-file'.

Use `\\[disasm-obj-previous-line]' and `\\[disasm-obj-next-line]' to
go to the previous or next block and highlight the corresponding
source line."
  (interactive)

  (kill-all-local-variables)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^$")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(disasm-obj-mode-font-lock-keywords nil nil nil nil))

  (setq major-mode 'disasm-obj-mode)
  (setq mode-name "Disassembly")
  (use-local-map disasm-obj-mode-map)
  ;; (set-syntax-table disasm-obj-mode-syntax-table)
  (run-hooks 'disasm-obj-mode-hook))

(defun disasm-obj-next-line (arg)
  "Go to the next block of assembly and highlight the corresponding source line."
  (interactive "p")
  (if (disasm-obj-forward-src arg)
      (disasm-obj-select (point))
    (error "No next source line")))

(defun disasm-obj-previous-line (arg)
  "Go to the previous block of assembly and highlight the corresponding source line."
  (interactive "p")
  (if (disasm-obj-backward-src arg)
      (disasm-obj-select (point))
    (error "No next source line")))

(defun disasm-obj-next-function (arg)
  "Go to the start of the next function."
  (interactive "p")
  (re-search-forward "^[0-9]+ <.*>:$" nil t arg)
  (disasm-obj-forward-src 1)
  (disasm-obj-select (point)))

(defun disasm-obj-previous-function (arg)
  "Go to the start of the previous function."
  (interactive "p")
  (disasm-obj-backward-src 1)
  (re-search-backward "^[0-9]+ <.*>:$" nil t arg)
  (disasm-obj-forward-src 1)
  (disasm-obj-select (point)))
  

(defun disasm-obj-select (pos &optional file line)
  "Select the current block of assembly and highlight the corresponding source line."
  (interactive "d")
  (goto-char pos)
  (beginning-of-line)
  (unless (and file line)
    (looking-at "^\\(/.*\\):\\([0-9]*\\)$")
    (setq file (match-string-no-properties 1)
          line (string-to-number (match-string 2))))
  (forward-line 1)
  (search-forward ":")
  (disasm-mark-line (disasm-obj-get-src-buffer file) (current-buffer) file line))


;;; (defun disasm-jump-target (target-string)
;;;   (unless (string-match "\\([_a-zA-Z0-9]*\\)\\(+0x\\([0-9a-f]+\\)\\)?" target-string)
;;;     (error "Not a jump link"))
;;;   (let* ((symbol (match-string-no-properties 1 target-string))
;;;          (offset (if (zerop (length (match-string 2 target-string)))
;;;                      0
;;;                    (string-to-number (match-string-no-properties 3 target-string) 16)))
;;;          (address (cdr (assoc symbol disasm-obj-symtable))))
;;;     (+ address offset)))

(defun disasm-obj-addr-pos (address-str)
  "Find the buffer position of the line for ADDRESS-STR"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^ *%s:" address-str))))

(defun disasm-obj-show-jump (arg)
  (interactive "P")
  (let ((target (get-text-property (point) 'jump-to)))
    (if (not target)
        (error "No target")
      (message "jump to %x" target)
      (let ((pos (disasm-obj-addr-pos target)))
        (if arg
            (goto-char pos)
          (save-excursion
            (goto-char pos)
            (sit-for 1)))))))

(defun disasm-obj-show-jumps-to (arg)
  (interactive "P")
  (let ((sites (get-text-property (point) 'jump-from)))
    (if (not sites)
        (error "No jumps")
      (if arg
          (goto-char (disasm-obj-addr-pos (car sites)))
        (while sites
          (let* ((site (car sites))
                 (pos (disasm-obj-addr-pos site)))
            (message "jump from %x" site)
            (save-excursion
              (goto-char pos)
              (sit-for 1)))
          (setq sites (cdr sites)))))))

(defun disasm-obj-quit ()
  "Kill the disassembly object buffer and disable disasm-src-mode in
the associated source buffers."
  (interactive)
  (save-excursion
    (while disasm-obj-src-buffers
      (set-buffer (car disasm-obj-src-buffers))
      (disasm-src-mode 0)
      (setq disasm-obj-src-buffers (cdr disasm-obj-src-buffers))))
  (let ((obj-buf (current-buffer)))
    (select-window (display-buffer disasm-obj-orig-buffer))
    (kill-buffer obj-buf)))

(defun disasm-obj-toggle-files ()
  "Kill the disassembly object buffer and disable disasm-src-mode in
the associated source buffers."
  (interactive)
  (put 'disasm-file-name 'invisible
       (not (get 'disasm-file-name 'invisible)))
  (redraw-display))

(defun disasm-obj-forward-src (arg)
  (re-search-forward "^\\(/.*\\):\\([0-9]*\\)$" nil t arg))
  
(defun disasm-obj-backward-src (arg)
  (forward-line -1)
  (re-search-backward "^\\(/.*\\):\\([0-9]*\\)$" nil t arg))
  
(defun disasm-obj-find-src (file line)
  (forward-line 1)
  (if (re-search-forward (if line
                             (format "%s:%d$" (regexp-quote file) line)
                           (format "%s:[0-9]+$" (regexp-quote file)))
                         nil t)
      (progn
        (beginning-of-line)
        t)
    ;; (error "Line %d in %s not found" line (file-name-nondirectory file))
    nil))
  
(defun disasm-obj-get-src-buffer (file)
  (let ((obj-buf (current-buffer))
        (src-buf (find-file-noselect file)))
    (unless (memq src-buf disasm-obj-src-buffers)
      (save-excursion
        (set-buffer src-buf)
        (disasm-src-mode 1)
        (setq disasm-obj-buffer obj-buf))
      (setq disasm-obj-src-buffers (cons src-buf disasm-obj-src-buffers)))
    src-buf))

;;; (defun disasm-obj-show-source-line (file line)
;;;   (let* ((obj-buf (current-buffer))
;;;          (buf (disasm-obj-get-src-buffer file))
;;;          (window (or (get-buffer-window buf)
;;;                      (display-buffer buf t))))
;;;     (set-buffer buf)
;;;     (disasm-src-mode 1)
;;;     (setq disasm-obj-buffer obj-buf)
;;;     (set-window-point window (disasm-src-mark-line line))))

(defun disasm-obj-mark-block ()
  "Mark the block beginning after the line where (point) is"
  (save-excursion
    (forward-line)
    (let ((start (point)))
      (if (not (re-search-forward "^[^ \t]" nil t))
          (goto-char (point-max)))
      (disasm-obj-add-overlay start (1- (point)))
      start)))

;;; disasm minor mode

(defvar disasm-src-minor-mode nil)
(make-variable-buffer-local 'disasm-src-minor-mode)

(defvar disasm-src-mode-map nil)
(unless disasm-src-mode-map
  (setq disasm-src-mode-map (make-keymap))
  (define-key disasm-src-mode-map "\r" 'disasm-src-select)
  (define-key disasm-src-mode-map "p" 'disasm-src-previous-line)
  (define-key disasm-src-mode-map "n" 'disasm-src-next-line)
  (define-key disasm-src-mode-map "q" 'disasm-src-quit)
  )

(unless (assq 'disasm-src-minor-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(disasm-src-minor-mode " dis") minor-mode-alist)))
(unless (assq 'disasm-src-mode-map minor-mode-map-alist)
  (setq minor-mode-map-alist (cons (cons 'disasm-src-minor-mode disasm-src-mode-map)
                                   minor-mode-map-alist)))

(defun disasm-src-mode (&optional arg)
  (interactive "P")
  (if arg
      (setq disasm-src-minor-mode (> arg 0))
    (setq disasm-src-minor-mode (not disasm-src-minor-mode)))
  (if (not disasm-src-minor-mode)
      (delete-overlay disasm-src-overlay)))

(defun disasm-src-next-line (arg)
  "Go to the next source line and highlight the corresponding disassembly line."
  (interactive "p")
  (next-line arg)
  (disasm-src-select (point)))

(defun disasm-src-previous-line (arg)
  "Go to the previous source line and highlight the corresponding disassembly line."
  (interactive "p")
  (previous-line arg)
  (disasm-src-select (point)))

(defun disasm-src-select (pos)
  (interactive "d")
  (let* ((file (buffer-file-name))
         (line (save-restriction
                 (widen)
                 (disasm-current-line))))
    (disasm-mark-line (current-buffer) disasm-obj-buffer file line)))

(defun disasm-src-quit ()
  (interactive)
  (condition-case e
      (progn
        (set-buffer disasm-obj-buffer)
        (disasm-obj-quit))
    (error (disasm-src-mode 0))))

;;; Internals

(defun disasm-mark-line (src-buffer obj-buffer file line)
  (let ((src-window (or (get-buffer-window src-buffer)
                        (display-buffer src-buffer t)))
        (obj-window (or (get-buffer-window obj-buffer)
                        (display-buffer obj-buffer t)))
        start)
    (save-excursion
      (set-buffer obj-buffer)
      (disasm-obj-delete-overlays)
      (goto-char (point-min))
      (while (disasm-obj-find-src file line)
        (if start
            (disasm-obj-mark-block)
          (setq start (disasm-obj-mark-block)))))
    (if (eq (current-buffer) src-buffer)
        (if start (set-window-point obj-window start)))
    (save-excursion
      (set-buffer src-buffer)
      (setq start (disasm-src-mark-line line)))
    (if (eq (current-buffer) obj-buffer)
        (set-window-point src-window start))))

(unless (facep 'disasm-overlay-face)
  (make-face 'disasm-overlay-face)
  (if (eq (cdr (assoc 'background-mode (frame-parameters))) 'dark)
      (set-face-background 'disasm-overlay-face "dark slate blue")
    (set-face-background 'disasm-overlay-face "light blue")))

(defun disasm-src-mark-line (line)
  (goto-line line)
  (disasm-src-set-overlay (point) (1+ (line-end-position)))
  (point))
  
(defun disasm-src-set-overlay (start stop)
  (if disasm-src-overlay
      (move-overlay disasm-src-overlay start stop)
    (setq disasm-src-overlay (make-overlay start stop))
    (if window-system
        (overlay-put disasm-src-overlay
                     'face '(disasm-overlay-face))
      (overlay-put disasm-src-overlay
                   'before-string ">"))))

(defun disasm-obj-delete-overlays ()
  (while disasm-obj-overlays
    (delete-overlay (car disasm-obj-overlays))
    (setq disasm-obj-overlays (cdr disasm-obj-overlays))))

(defun disasm-obj-add-overlay (start stop)
  (let ((ovl (make-overlay start stop)))
    (if window-system
        (overlay-put ovl 'face '(disasm-overlay-face))
      (overlay-put ovl 'before-string ">"))    
    (setq disasm-obj-overlays (cons ovl disasm-obj-overlays))))
