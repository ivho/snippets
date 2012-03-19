(add-to-list 'auto-mode-alist '("RELEASENOTES" . text-mode))

(autoload 'simgen-mode "simgen" "SimGen source editing mode" t)
(add-to-list 'auto-mode-alist '("\\.sg$" . simgen-mode))

(autoload 'conf-mode "simics-conf" "Simics configuration file editing mode" t)
(add-to-list 'auto-mode-alist '("\\.conf$" . conf-mode))

(add-to-list 'auto-mode-alist '("\\.docu$" . sgml-mode))

(autoload 'debug-simics "debug-simics"
  "debug simics running on the same machine by the same user" t)
(autoload 'debug-application "debug-simics"
  "Debug an application by name or pattern.

When run interactively, it will ask for the name of the process to debug.

With a prefix argument, it will instead ask for a regular expression
that shall match the process name.

When run non-interactively, PATTERN holds the name (when EXACT is
non-nil) or the pattern (when EXACT is nil)." t)

(autoload 'test-simics "test-simics"
  "Run simics tests" t)
(autoload 'mark-test-deviations "test-simics"
  "Mark test deviations from mp-failures" t)

(autoload 'simics "simics" "Run Simics" t)

;;; C mode stuff

(require 'cc-styles)

(c-add-style "virtutech"
             '((comment-column . 45)
               (c-basic-offset . 8)
               (c-offsets-alist . ((substatement-open . 0)))
               (c-block-comment-prefix . "")
               (indent-tabs-mode . nil)))

(c-add-style "dml"
             '((comment-column . 45)
               (c-basic-offset . 4)
               (c-offsets-alist . ((substatement-open . 0)
                                   (topmost-intro-cont . +)))
               (c-block-comment-prefix . "")
               (indent-tabs-mode . nil)))

(setq c-default-style '((dml-mode . "dml")
                        (java-mode . "java")
                        (other . "virtutech"))) 

(autoload 'dml-mode "dml-mode" "DML mode" t)
(add-to-list 'auto-mode-alist '("\\.ddl$" . dml-mode))
(add-to-list 'auto-mode-alist '("\\.dml$" . dml-mode))

(autoload 'ifdef-highlight-mode "ifdef" "ifdef highlighting mode" t)
;(defun maybe-ifdef-highlight-mode ()
;  (when (string-match ".*simics.*" buffer-file-name)
;      (ifdef-highlight-mode 1)))

;;; Bugzilla

(autoload 'bugz-list "bugz" "List bugs" t)
(autoload 'bugz-view-bug "bugz" "View a bug" t)

;;(autoload 'cvs-cupdate "pcvs" "CVS update with a vtech twist" t)
;;(eval-after-load "pcvs" (load "cupdate"))
