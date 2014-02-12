;;; simgen.el --- SimGen mode

(require 'font-lock)
(require 'cc-mode)

;;; Keymap

(defvar simgen-mode-map nil
  "Keymap for simgen-mode")

(if simgen-mode-map
    nil
  (setq simgen-mode-map (make-sparse-keymap))
  (define-key simgen-mode-map "\M-\C-i" 'simgen-indent-c-line)
  (define-key simgen-mode-map "\C-c\C-\\" 'simgen-backslash-region))
          
;;; syntax table

(defvar simgen-mode-syntax-table nil "The syntax table used in simgen-mode.")
(if simgen-mode-syntax-table
    nil
  (setq simgen-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" simgen-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" simgen-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" simgen-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" simgen-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" simgen-mode-syntax-table)
  (modify-syntax-entry ?\} "){" simgen-mode-syntax-table)
  (if (string-match "Lucid" emacs-version)
      (modify-syntax-entry ?/ ". 1456" simgen-mode-syntax-table)
    (modify-syntax-entry ?/ ". 124b" simgen-mode-syntax-table))
  (modify-syntax-entry ?\* ". 23" simgen-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" simgen-mode-syntax-table)
  (modify-syntax-entry 32 " " simgen-mode-syntax-table)
  (modify-syntax-entry ?\t " " simgen-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" simgen-mode-syntax-table)
  (modify-syntax-entry ?\f " " simgen-mode-syntax-table)
  (modify-syntax-entry ?\' "w" simgen-mode-syntax-table)
  (modify-syntax-entry ?\. "_" simgen-mode-syntax-table)
  (modify-syntax-entry ?\_ "_" simgen-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" simgen-mode-syntax-table)
  )

;;; font-lock

;; This is ugly...
(if (boundp 'font-lock-preprocessor-face)
    nil
  (copy-face 'font-lock-builtin-face 'font-lock-preprocessor-face)
  (setq font-lock-preprocessor-face 'font-lock-preprocessor-face))

(defvar simgen-mode-font-lock-keywords
  '(("\\<\\(opcode\\|pattern\\|syntax\\|semantics\\|name\\|attrs\\|attributes\\|header\\|usercode\\|interpreter\\|imfields\\|intermediate form\\|in\\|not\\|reg\\|local\\|end\\)\\>"
     (1 font-lock-keyword-face))
    ("\\<\\(fields\\)"
     (1 font-lock-keyword-face))
    ("\\<\\(instruction\\|virtual\\|define\\)\\s-+\\([a-zA-Z0-9_{}]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ("\\(case\\)\\s-+\\(.*\\)\\(->\\)"
     (1 font-lock-keyword-face)
     (3 font-lock-keyword-face))
    ("^\\(%\\w*\\)"
     (1 font-lock-preprocessor-face))
    ("^\\s-+\\(%.*\\)"
     (1 font-lock-warning-face t))
    ))

;;; imenu

(defconst simgen-imenu-generic-function
  '(("Instructions" "^\\s-*\\<instruction\\>\\s-*\\([a-zA-Z0-9{}_-]*\\)" 1)
    ("CCS Macros" "^\\s-*\\<define\\>\\s-*\\([a-zA-Z0-9_-]*\\)" 1)))

;;; Code

(defun simgen-mode ()
  (interactive)

  (kill-all-local-variables)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "// *")
  ;;(make-local-variable 'comment-end)
  ;;(setq comment-end "*/")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'simgen-indent-line)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'c-fill-paragraph)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(simgen-mode-font-lock-keywords nil nil nil nil))
  (setq imenu-generic-expression simgen-imenu-generic-function)

  (setq major-mode 'simgen-mode)
  (setq mode-name "SimGen")
  (use-local-map simgen-mode-map)
  (set-syntax-table simgen-mode-syntax-table)
  (run-hooks 'simgen-mode-hook))

(defvar simgen-offset 4
  "*Indentation offset used in SimGen mode")

(defun simgen-indent-line ()
  (save-excursion
    (back-to-indentation)
    (let (col)
      (cond ((looking-at "%")
             (indent-line-to 0))
            ((looking-at "#}")
             (indent-line-to (save-excursion
                               (backward-up-list 1)
                               (if (and (= (char-before) ?\#) (= (char-after) ?\{))
                                   (current-indentation)
                                 (error "No matching #{")))))
            ((looking-at "}")
             (indent-line-to (save-excursion
                               (backward-up-list 1)
                               (if (and (/= (char-before) ?\#) (= (char-after) ?\{))
                                   (current-indentation)
                                 (error "No matching {")))))
            ;; Probable CPP thingee
            ((looking-at "#[^{]")       
             (indent-line-to 0))
            ;; Inside () [] or {}
            ((save-excursion
               (condition-case e
                   (progn
                     (backward-up-list 1)
                     (if (= (char-after) ?\{)
                         (setq col (+ (current-indentation) simgen-offset))
                       (setq col (1+ (current-column)))))
                 (error nil)))
             (indent-line-to col))

            ((looking-at "\\(instruction\\|virtual\\|header\\|usercode\\|interpreter\\)\\>")
             (indent-line-to 0))

            ;; This is a combination of two rules:
            ;; 1) In a case, 'syntax', 'semantics' and 'name' are aligned
            ;;    with 'fields'
            ;; 2) In an instruction, 'syntax', 'sematics', and 'attrs' are
            ;;    aligned with 'pattern'
            ((looking-at "\\(pattern\\|syntax\\|semantics\\|attr\\(ibute\\)?s\\|name\\)\\>")
             (indent-line-to
              (save-excursion
                (if (re-search-backward "\\<\\(fields\\|opcode\\)" nil t)
                    (current-indentation)
                  0))))

            ;; 'fields' is indented to 0 unless the previous line
            ;; starts with 'define' or 'case'
            ((and (looking-at "fields")
                  (not (save-excursion
                         (re-search-backward "\\<\\(define\\|case\\)\\>.*\n\\s-*\\=" nil t))))
             (indent-line-to 0))

            ;; 'case' is a little tricky
            ((looking-at "case")
             (indent-line-to
              (save-excursion
                (if (re-search-backward "^\\s-*\\(case\\|define\\)" nil t)
                    (if (looking-at "\\s-*case")
                        (current-indentation)
                      (+ (current-indentation) simgen-offset))
                  0))))                         
            ;; After keyword
            ((save-excursion
               (beginning-of-line 0)
               (if (looking-at "\\s-*\\(opcode\\|fields\\|imfields\\|intermediate form\\|instruction\\|virtual\\|pattern\\|syntax\\|semantics\\|attr\\(ibute\\)?s\\|header\\|usercode\\|interpreter\\|define\\|case\\)\\>")
                   (setq col (+ (current-indentation) simgen-offset))))
             (indent-line-to col))
            (t
             (indent-line-to (save-excursion (forward-line -1)
                                             (current-indentation)))))))
  (if (re-search-backward "^[ \t]*\\=" nil t)
      (back-to-indentation)))

(defun simgen-indent-c-line ()
  (interactive)
  (c-indent-line))

(defun simgen-backslash-region (start stop delete-flag)
  "Backslashify region."
  (interactive "*r\nP")
  (save-excursion
    (let ((col 0)
          (stop-marker (make-marker)))
      (set-marker stop-marker stop)
      (goto-char start)
      (while (< (point) stop-marker)
        (end-of-line)
        (if (eq (char-before) ?\\)
            (delete-backward-char 1))
        (delete-horizontal-space)
        (if (> (current-column) col)
            (setq col (current-column)))
        (forward-line 1))
      (forward-line -1)
      (set-marker stop-marker (point))
      (setq col (* (1+ (/ col 8)) 8))
      (goto-char start)
      (unless delete-flag
        (while (< (point) stop-marker)
          (end-of-line)
          (move-to-column col t)
          (insert "\\")
          (forward-line 1))))))
      
