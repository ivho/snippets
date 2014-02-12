(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


(add-to-list 'load-path "~/.emacs.d/")

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(compilation-info ((((class color)) (:foreground "blue" :weight bold))))
 )
;;(load-file "quilt.el")



;;- Comments are displayed in `font-lock-comment-face'
;; - Strings are displayed in `font-lock-string-face';
;; - Certain other expressions are displayed in other faces according to the
;;   value of the variable `font-lock-keywords'.


(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq show-trailing-whitespace t)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

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

;;(add-to-list 'load-path "/usr/share/doc/git-core/contrib/emacs/")

;;(require 'git)
;;(require 'git-blame)
;;(load-library 'magit)
;;(load-file "/home/ivho/simics42/simics-model-builder-4.2.21/scripts/dml-mode.el")
;;(require 'ipython)
(menu-bar-mode 0)
(tool-bar-mode 0)


(load-file "/usr/share/emacs/site-lisp/xcscope.el")
(require 'xcscope)
(global-set-key (kbd "<f2>") 'cscope-find-this-symbol)


(global-set-key [(meta return)] 'goto-line)


;;DML stuff
;;(setq load-path (cons "~/.emacs.d/vt-elisp" load-path))
(add-to-list 'load-path "~/.emacs.d/vt-elisp/")

(load "vt-elisp-start")
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

(setq auto-mode-alist
      (cons '("linux.*\\.c$" . linux-c-mode)
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
