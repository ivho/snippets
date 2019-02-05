(require 'etags)
(require 'package)
;;(add-to-list 'package-archives
;;            '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-list
      '(python-environment flymake ))
	;; deferred epc
	;; 		   flycheck ctable jedi concurrent company cyberpunk-theme elpy
	;; 		   yasnippet pyvenv highlight-indentation find-file-in-project
	;; 		   sql-indent sql exec-path-from-shell iedit
	;; 		   auto-complete popup let-alist git-rebase-mode
	;; 		   git-commit-mode minimap popup))


(package-initialize)

					; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

					; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))
(require 'whitespace)


(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(add-to-list 'load-path "~/.emacs.d/lisp")
;;(require 'stgit)



    (defun setup-tide-mode ()
      (interactive)
       (tide-setup)
       (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
       (eldoc-mode +1)
       (company-mode +1)
       (tide-hl-identifier-mode +1))

    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)

    ;; formats the buffer before saving
;;    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)

    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-hook 'typescript-mode-hook
	      (lambda ()
		(when (string-equal "tsx" (file-name-extension buffer-file-name))
		  (setup-tide-mode))))

(setq tide-format-options
      '(:indentSize 1))

    ;; funky typescript linting in web-mode
;;    (flycheck-add-mode 'typescript-tslint 'web-mode)



;; formats the buffer before saving
;(add-hook 'before-save-hook 'tide-format-before-save)

;(add-hook 'typescript-mode-hook #'setup-tide-mode)

;(auto-install-from-url "https://raw.github.com/aki2o/emacs-tss/master/tss.el")
;(auto-install-from-url "https://raw.github.com/aki2o/emacs-tss/master/typescript.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(package-selected-packages
   (quote
    (tide json-mode js2-mode editorconfig company-web company-c-headers)))
 '(typescript-indent-level 2))

(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; aptitude install pyflakes to check python code
;;(require 'flymake-cursor)
;;(global-set-key [f4] 'flymake-goto-next-error)

 (when (load "flymake" t)
   (defun flymake-pyflakes-init ()
     (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  
   (add-to-list 'flymake-allowed-file-name-masks
 		'("create-app\\'\\.py\\'" flymake-pyflakes-init)))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; Build a custom command-line using flymake.mk
(defun flymake-get-kernel-make-cmdline (source base-dir)
  (list "make"
	(list "-s"
	      "-f"
	      "flymake.mk"
	      "-C"
	      base-dir
	      (concat "CHK_SOURCES=" source)
	      "SYNTAX_CHECK_MODE=1"
	      "check-syntax")))

;; Search for flymake.mk to locate kernel tree base
(defun flymake-kernel-make-init ()
  (flymake-simple-make-init-impl 'flymake-create-temp-inplace t t "flymake.mk" 'flymake-get-kernel-make-cmdline))

;; Register against .c files under /linux/ or /kernel/
;; Since the list is parsed in order use `push`
(push '(".+/\\(linux\\|kernel\\)/.+\\.c$" flymake-kernel-make-init) flymake-allowed-file-name-masks)


;;(add-to-list 'load-path "~/.emacs.d/")1
(add-to-list 'load-path "~/.emacs.d/tools")
;;(require 'xcscope)
;;(load-file "yaml-mode.el")
;;(require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;;(require 'xcscope)
;;(require 'nginx-mode)
;;(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;;- Comments are displayed in `font-lock-comment-face'
;; - Strings are displayed in `font-lock-string-face';
;; - Certain other expressions are displayed in other faces according to the
;;   value of the variable `font-lock-keywords'.

(defun git-grepz ()
  (shell-command "git grep apa")
)

(defun git-grepy ()
  (interactive)
;;  (shell-command "git rev-parse --show-toplevel")

  (setq apa (shell-command-to-string "git rev-parse --git-dir"))
  (setq default-directory apa)
  (message apa)
  (shell-command (concatenate 'string "git --git-dir=" apa " grep a"))
;;  (shell-command "git --git-dirgrep printf")
  )
(setq ggtags-executable-directory "/usr/bin")

;;(require 'column-marker)

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (column-enforce-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

(setq show-trailing-whitespace t)
(defun qemu-c-mode ()
  "C mode with adjusted defaults for use with qemu."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 4)
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4))

(defun simics-c-mode ()
  "C mode with adjusted defaults for use with simcis."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 8))

(defun simgen-c-mode ()
  "C mode with adjusted defaults for use with simcis."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 8))

(defun wr-simgen-mode ()
  "C mode with adjusted defaults for use with simcis."
  (interactive)
  (simgen-mode)
  (setq tab-width 4)
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))


(add-to-list 'load-path "/home/eholiva/.emacs.d/local")


;;(add-to-list 'load-path "/usr/share/doc/git-core/contrib/emacs/")

;;(require 'git)
;;(require 'git-blame)
;;(load-library 'magit)
;;(load-file "/home/ivho/simics42/simics-model-builder-4.2.21/scripts/dml-mode.el")
;;(require 'ipython)
(menu-bar-mode 0)
;;(tool-bar-mode 0)


;(load-file "/usr/share/emacs/site-lisp/xcscope.el")
;(require 'xcscope)
;(global-set-key (kbd "<f2>") 'cscope-find-this-symbol)


(global-set-key [(meta return)] 'goto-line)

(defun my-mark-current-word (&optional arg allow-extend)
    "Put point at beginning of current word, set mark at end."
    (interactive "p\np")
    (setq arg (if arg arg 1))
    (if (and allow-extend
             (or (and (eq last-command this-command) (mark t))
                 (region-active-p)))
        (set-mark
         (save-excursion
           (when (< (mark) (point))
             (setq arg (- arg)))
           (goto-char (mark))
           (forward-word arg)
           (point)))
      (let ((wbounds (bounds-of-thing-at-point 'word)))
        (unless (consp wbounds)
          (error "No word at point"))
        (if (>= arg 0)
            (goto-char (car wbounds))
          (goto-char (cdr wbounds)))
        (push-mark (save-excursion
                     (forward-word arg)
                     (point)))
        (activate-mark))))

(global-set-key (kbd "<f12>") 'next-match)
(global-set-key (kbd "<f10>") 'my-mark-current-word)


;;DML stuff
;;(setq load-path (cons "~/.emacs.d/vt-elisp" load-path))
;;(add-to-list 'load-path "/home/packages/vt-elisp")

;;(load "vt-elisp-start")
(add-hook 'dml-mode-hook
  (function (lambda ()
              (setq show-trailing-whitespace t))))

(add-hook 'python-mode-hook
  (function (lambda ()
              (global-set-key [(meta right)] 'python-shift-right)
              (global-set-key [(meta left)] 'python-shift-left)
	      (setq indent-tabs-mode nil)
	      (setq python-indent 4)
              (setq show-trailing-whitespace t))))

(setq auto-mode-alist
      (append '(("\\.dml$" . dml-mode)
		) auto-mode-alist))

(setq indent-tabs-mode nil)

(require 'sgml-mode)
(add-hook 'html-mode-hook
          (function
           (lambda ()
             (progn
               (setq indent-tabs-mode nil)
               (setq show-trailing-whitespace t)
               (setq sgml-basic-offset 2)))))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-mode-hook
      '(lambda () (progn
		    (set-variable 'indent-tabs-mode nil)
		    (setq show-trailing-whitespace t)
		    )))
(set-variable 'indent-tabs-mode nil)
(setq auto-mode-alist
      (cons '(".*\\.[ch]$" . linux-c-mode)
	    auto-mode-alist))

(setq auto-mode-alist
      (cons '("cpu-common/.*\\.c$" . linux-c-mode)
	    auto-mode-alist))

(setq show-trailing-whitespace t)

(put 'upcase-region 'disabled nil)

(defun simics-script-mode ()
  "simics script mode."
  (interactive)
  (setq tab-width 4)
  (setq show-trailing-whitespace t)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq c-indentation 4))

(add-hook 'c-mode-hook 'simics-script-mode)

(defun my-shell-script-mode ()
  "shell script mode."
  (interactive)
  (setq tab-width 4)
  (setq show-trailing-whitespace t)
  (setq basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq indentation 4))

(add-hook 'shell-script-mode-hook 'my-shell-script-mode)

(setq auto-mode-alist
      (cons '("\\.simics$" . simics-script-mode)
	    auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.sg$" . wr-simgen-mode)
	    auto-mode-alist))

(setq auto-mode-alist
      (cons '("\\.include$" . simics-script-mode)
	    auto-mode-alist))

(autoload 'scad-mode "scad-mode" "Major mode for editing SCAD code." t)
(add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode))

(put 'upcase-region 'disabled nil)


(defun my-mark-current-word (&optional arg allow-extend)
    "Put point at beginning of current word, set mark at end."
    (interactive "p\np")
    (setq arg (if arg arg 1))
    (if (and allow-extend
             (or (and (eq last-command this-command) (mark t))
                 (region-active-p)))
        (set-mark
         (save-excursion
           (when (< (mark) (point))
             (setq arg (- arg)))
           (goto-char (mark))
           (forward-word arg)
           (point)))
      (let ((wbounds (bounds-of-thing-at-point 'word)))
        (unless (consp wbounds)
          (error "No word at point"))
        (if (>= arg 0)
            (goto-char (car wbounds))
          (goto-char (cdr wbounds)))
        (push-mark (save-excursion
                     (forward-word arg)
                     (point)))
        (activate-mark))))

(defun get-point (symbol &optional arg)
      "get the point"
      (funcall symbol arg)
      (point)
     )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe 
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))
    ))

(defun copy-word (&optional arg)
      "Copy words at point into kill-ring"
       (interactive "P")
       (copy-thing 'backward-word 'forward-word arg)
       ;;(paste-to-mark arg)
       )
;;(global-set-key [(ctrl return)] 'copy-word)
;;(global-set-key [(control p)] 'copy-word)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:foreground "yellow"))))
 '(minibuffer-prompt ((t (:foreground "green")))))
