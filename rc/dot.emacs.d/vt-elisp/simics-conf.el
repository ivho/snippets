;;; simics-conf.el --- Simics configuration file editing mode

(require 'font-lock)

;;; Keymap

(defvar conf-mode-map nil
  "Keymap for conf-mode")

(if conf-mode-map
    nil
  (setq conf-mode-map (make-sparse-keymap)))
          
;;; syntax table

(defvar conf-mode-syntax-table nil "The syntax table used in conf-mode.")
(if conf-mode-syntax-table
    nil
  (setq conf-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" conf-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" conf-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" conf-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" conf-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" conf-mode-syntax-table)
  (modify-syntax-entry ?\} "){" conf-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" conf-mode-syntax-table)
  (if (string-match "Lucid" emacs-version)
      (modify-syntax-entry ?\# ". 5" conf-mode-syntax-table)
    (modify-syntax-entry ?\# "<b" conf-mode-syntax-table))
  (modify-syntax-entry ?\* ". 23" conf-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" conf-mode-syntax-table)
  (modify-syntax-entry 32 " " conf-mode-syntax-table)
  (modify-syntax-entry ?\t " " conf-mode-syntax-table)
  (modify-syntax-entry ?\n ">b" conf-mode-syntax-table)
  (modify-syntax-entry ?\f " " conf-mode-syntax-table)
  (modify-syntax-entry ?\' "w" conf-mode-syntax-table)
  (modify-syntax-entry ?\_ "w" conf-mode-syntax-table)
  (modify-syntax-entry ?\. "_" conf-mode-syntax-table)
  (modify-syntax-entry ?\_ "_" conf-mode-syntax-table)
  (modify-syntax-entry ?- "_" conf-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" conf-mode-syntax-table)
  )

;;; font-lock

;; This is ugly...
(if (boundp 'font-lock-preprocessor-face)
    nil
  (copy-face 'font-lock-builtin-face 'font-lock-preprocessor-face)
  (setq font-lock-preprocessor-face 'font-lock-preprocessor-face))

(defvar conf-mode-font-lock-keywords
  '(("\\<\\(OBJECT\\|TYPE\\)\\>"
     (1 font-lock-keyword-face))
    ("\\<\\([a-zA-Z][a-zA-Z0-9_-]*\\):"
     (1 font-lock-variable-name-face))
    ))

;;; imenu

(defconst conf-imenu-generic-function
  '((nil "OBJECT\\s-*\\([a-zA-Z0-9_-]*\\)" 1)))

;;; Code

(defun conf-mode ()
  (interactive)

  (kill-all-local-variables)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "/*")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-end)
  (setq comment-end "*/")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'conf-indent-line)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(conf-mode-font-lock-keywords nil nil nil nil))
  (setq imenu-generic-expression conf-imenu-generic-function)

  (setq major-mode 'conf-mode)
  (setq mode-name "Conf")
  (use-local-map conf-mode-map)
  (set-syntax-table conf-mode-syntax-table)
  (run-hooks 'conf-mode-hook))

(defvar conf-offset 8
  "*Indentation offset used in Conf mode")

(defun conf-indent-line ()
  (save-excursion
    (back-to-indentation)
    (let (col)
      (cond ((looking-at "}")
             (indent-line-to (save-excursion
                               (backward-up-list 1)
                               (if (and (/= (char-before) ?\#)
                                        (= (char-after) ?\{))
                                   (current-indentation)
                                 (error "No matching {")))))
            ;; Don't alter comment indentation
            ((looking-at "#")
             nil)
            ;; Inside () [] or {}
            ((save-excursion
               (condition-case e
                   (progn
                     (backward-up-list 1)
                     (if (= (char-after) ?\{)
                         (setq col (+ (current-indentation) conf-offset))
                       (setq col (1+ (current-column)))))
                 (error nil)))
             (indent-line-to col))
            (t
             (indent-line-to (save-excursion (forward-line -1)
                                             (current-indentation)))))))
  (if (re-search-backward "^[ \t]*\\=" nil t)
      (back-to-indentation)))
