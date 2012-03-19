(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "green")))))
;;(load-file "quilt.el")

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


(global-set-key [(meta return)] 'goto-line)

;;DML stuff
(setq load-path (cons "~/.emacs.d/vt-elisp" load-path))
(load "vt-elisp-start")
(add-hook 'dml-mode-hook
  (function (lambda ()
              (setq show-trailing-whitespace t))))

(add-hook 'python-mode-hook
  (function (lambda ()
              (global-set-key [(meta right)] 'python-shift-right)
              (global-set-key [(meta left)] 'python-shift-left)
              (setq show-trailing-whitespace t))))

(setq auto-mode-alist
      (append '(("\\.dml$" . dml-mode)
		) auto-mode-alist))

(setq show-trailing-whitespace t)
