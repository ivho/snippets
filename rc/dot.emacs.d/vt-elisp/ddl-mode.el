(require 'cc-mode)

(defconst c-DDL-primitive-type-kwds
  "char\\|double\\|float\\|u?int[0-9]*\\|long\\|short\\|void")

(defconst c-DDL-specifier-kwds
  "const\\|extern\\|static")

(defconst c-DDL-class-kwds "struct\\|device\\|bank\\|register\\|field\\|group\\|attribute\\|event\\|method\\|parameter")

(defconst c-DDL-extra-toplevel-kwds "extern")

(defconst c-DDL-other-decl-kwds "enum\\|typedef")

(defconst c-DDL-decl-level-kwds "is")

(defconst c-DDL-block-stmt-1-kwds "do\\|else")

(defconst c-DDL-block-stmt-2-kwds "for\\|foreach\\|if\\|switch\\|while")

(defconst c-DDL-simple-stmt-kwds "break\\|continue\\|return\\|fail\\|exit\\|error")

(defconst c-DDL-label-kwds "case\\|default")

(defconst c-DDL-expr-kwds "sizeof\\|sizeoftype\\|typeof\\|defined\\|undefined")

(defconst c-DDL-keywords
  (concat c-DDL-primitive-type-kwds "\\|" c-DDL-specifier-kwds
	  "\\|" c-DDL-class-kwds "\\|" c-DDL-extra-toplevel-kwds
	  "\\|" c-DDL-other-decl-kwds
	  "\\|" c-DDL-decl-level-kwds ;; "\\|" c-DDL-protection-kwds
	  "\\|" c-DDL-block-stmt-1-kwds "\\|" c-DDL-block-stmt-2-kwds
	  "\\|" c-DDL-simple-stmt-kwds "\\|" c-DDL-label-kwds
	  "\\|" c-DDL-expr-kwds))

(defconst c-DDL-class-key (c-paren-re c-DDL-class-kwds))

(defconst c-DDL-comment-start-regexp "//")

(defun ddl-mode ()
  "Major mode for editing DDL code.

Key bindings:
\\{c-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table c-mode-syntax-table)
  (setq major-mode 'ddl-mode
	mode-name "DDL"
	local-abbrev-table c-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end   ""
	c-keywords (c-identifier-re c-DDL-keywords)
	c-conditional-key c-C-conditional-key
	c-class-key c-DDL-class-key
	c-baseclass-key nil
	c-comment-start-regexp c-DDL-comment-start-regexp
	c-bitfield-key c-C-bitfield-key
        font-lock-defaults '((c-font-lock-keywords c-font-lock-keywords-1
                                                   c-font-lock-keywords-2 c-font-lock-keywords-3)
                             nil nil ((?_ . "w") (?- . "w")) beginning-of-defun
                             (font-lock-mark-block-function . mark-defun))
	)
  (cc-imenu-init cc-imenu-c-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'ddl-mode-hook)
  (c-update-modeline))
