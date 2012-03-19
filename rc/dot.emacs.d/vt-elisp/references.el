(require 'info)

(defconst is-xemacs (featurep 'xemacs)
  "If true, current editor is XEmacs, if false, current editor is not
XEmacs.")

; add references-mode in minor-mode-alist
(or (assq 'references-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(references-mode " Ref")
				 minor-mode-alist)))

; set advices
(add-hook 'Info-selection-hook 'hide-references)

(if (not is-xemacs)
    (progn
      (defvar visited-nodes nil
	"Keeps the list of visited nodes of the current info subfile
that is being viewed in info.")
      (make-variable-buffer-local 'visited-nodes)))

(defvar current-info-file nil
  "Keeps the current file that is being viewed in info.")
(make-variable-buffer-local 'current-info-file)

; references-mode as variable
(defvar references-mode nil
  "Specifies if references mode is active. If nil, minor mode is not
active, otherwhise minor mode is active.")
(make-variable-buffer-local 'references-mode)

; references-mode as function
(defun references-mode (&optional arg)
  "Toggle references mode.
With arg, turn references mode on if and only if arg is a positive
integer, otherwise turn references mode off."
  (interactive)
  ; set value of references-mode
  (setq references-mode 
	(cond
	 (arg
	  (and (integerp arg) (> arg 0)))
	 (t
	  (not references-mode))))
  (if references-mode
      ; hide references
      (progn
	(if (not is-xemacs)
	    (setq visited-nodes nil))
	(setq current-info-file Info-current-file)
	(hide-references))
    ; show references, check if gnu emacs or xemacs lucid
    (if is-xemacs
	(show-references)
      (save-excursion
	(let ((current-node Info-current-node))
	  (while visited-nodes
	    (Info-goto-node (car visited-nodes))
	    (setq visited-nodes (cdr visited-nodes))
	    (setq Info-history (cdr Info-history))
	    (show-references))
	  (Info-goto-node current-node)
	  (setq Info-history (cdr Info-history)))))))

(defun put-text-property-references (property value)
  "Put a text property in the reference lines of the current buffer."
  (interactive)
  ; remove read only
  (toggle-read-only)
  ; hide the reference lines
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "#%" nil t)
      (beginning-of-line)
      (let ((start (point)))
        (forward-line 1)
        (put-text-property start (point) property value))))
  ; restore read only
  (toggle-read-only))

(defun hide-references ()
  "Hide the reference lines of the current buffer.  If current editor
is not XEmacs, adds the current buffer's node to the variable
`visited-nodes`."
  (interactive)
  (if references-mode
      (progn
	(put-text-property-references 'invisible t)
	(if (not is-xemacs)
	    (progn
	      ; control the value of visited-nodes
	      (if (equal Info-current-file "dir")
		  (setq visited-nodes nil))
	      (if (not (equal Info-current-file current-info-file))
		  (setq visited-nodes (cons Info-current-node nil)))
	      (if (not (member Info-current-node visited-nodes))
		  (setq visited-nodes (cons Info-current-node
					    visited-nodes)))
	      ; set buffer as unmodified
	      (set-buffer-modified-p nil)))
	(setq current-info-file Info-current-file))))

(defun show-references ()
  "Show the reference lines of the current buffer."
  (interactive)
  (put-text-property-references 'invisible nil))

(defun reference-string ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\\s-*#%\\s-*\\(.*\\)")
        (match-string 1)
      nil)))

(defun search-reference-backward ()
  "Searches the first reference line found in the current document
before the current position of the cursor. Leaves the point at the
begining of the line of the found reference, or at the same position
if no reference is found."
  (interactive)
  (cond
   ((not (search-reference-backward-buffer))
    (if is-xemacs
	(Info-global-prev)
      (Info-backward-node))
    (goto-char (point-max))
    (while (not (search-reference-backward-buffer))
      (if is-xemacs
	  (Info-global-prev)
	(Info-backward-node))
      (setq Info-history (cdr Info-history))
      (goto-char (point-max)))))
  (message (or (reference-string) "No reference")))

(defun search-reference-backward-buffer ()
  "Searches the first reference line found in the current buffer
before the current position of the cursor. Leaves the point at the
begining of the line of the found reference, or at the same position
if no reference is found."
  (interactive)
  (cond
   ((search-backward "#%" nil t)
    (beginning-of-line)
    t)
   (t
    nil)))

(defun search-reference-forward ()
  "Searches the first reference line found in the current document
after the current position of the cursor. Leaves the point at the end
of the line of the found reference, or at the same position if no
reference is found."
  (interactive)
  (cond
   ((not (search-reference-forward-buffer))
    (if is-xemacs
	(Info-global-next)
      (Info-forward-node))
    (while (not (search-reference-forward-buffer))
      (if is-xemacs
	  (Info-global-next)
	(Info-forward-node))
      (setq Info-history (cdr Info-history)))))
  (message (or (reference-string) "No reference")))

(defun search-reference-forward-buffer ()
  "Searches the first reference line found in the current buffer
after the current position of the cursor. Leaves the point at the end
of the line of the found reference, or at the same position if no
reference is found."
  (interactive)
  (cond
   ((search-forward "#%" nil t)
    (end-of-line)
    t)
   (t
    nil)))

(defun get-reference-info ()
  "Searches the information of the first reference line found before
the current position of the cursor. It returns the information as a
list of two components, the first component is the path name of the
file, and the second is the line number. Leaves the point at the
begining of the line of the found reference, or at the same position
if no reference is found."
  (save-excursion
    ; search beginning of file name
    (re-search-forward "[^#% ]")
    (goto-char (match-beginning 0))
    ; read file name, if any
    (re-search-forward "[^ ]*" nil t)
    (if (equal (match-string 0) ":")
	(setq filename nil)
      (setq filename (match-string 0)))
    ; read line number
    (re-search-forward "[0-9]+" nil t)
    (let ((linenumber (string-to-number (match-string 0))))
      ;; return list with file name and line number
      (cons filename (cons linenumber nil)))))

(defun view-file-previous-reference ()
  "Edit the file specified by the first reference found before the
current position of the cursor. The file will be edit in another
window than the current one."
  (interactive)
  ;; keep current node
  (let ((orig-node Info-current-node)
        reference-info)
    (save-excursion
      ;; search references backward until find the first with file name
      (search-reference-backward)
      (setq reference-info (get-reference-info))
      (while (not (car reference-info))
        (search-reference-backward)
        (setq reference-info (get-reference-info)))
      ;; go back to original node, if necessary
      (cond 
       ((not (equal Info-current-node (Info-goto-node orig-node)))
        (setq Info-history (cdr Info-history)))))
    ;; open file in other window
    (find-file-other-window (car reference-info))
    (goto-line (car (cdr reference-info)))))

; key bindings
(require 'info)
(define-key Info-mode-map "v" 'view-file-previous-reference)
(define-key Info-mode-map "r" 'search-reference-backward)
(define-key Info-mode-map "c" 'search-reference-forward)

(provide 'references)
