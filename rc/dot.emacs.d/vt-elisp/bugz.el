;;; bugz.el

(require 'sqlquery)

(defun bugz-cached-thing (cache fill-cache id)
  (let ((thing (assq id (car cache))))
    (if thing
	(cdr thing)
      (progn
	(rplaca cache (funcall fill-cache))
	(let ((thing (assq id (car cache))))
	  (if thing
	      (cdr thing)
	    "<unknown>"))))))
  
(defun bugz-get-product-alist ()
  (mapcar '(lambda (e)
	     (cons (elt e 0) (elt e 1)))
	  (sqlquery products ((id . string-to-number) name description))))

(defvar bugz-products-cache (list nil))
(defun bugz-product-name (id)
  (bugz-cached-thing bugz-products-cache #'bugz-get-product-alist id))

(defun bugz-get-component-alist ()
  (mapcar '(lambda (e)
	     (cons (elt e 0) (elt e 1)))
	  (sqlquery components ((id . string-to-number) name description))))

(defvar bugz-components-cache (list nil))
(defun bugz-component-name (id)
  (bugz-cached-thing bugz-components-cache #'bugz-get-component-alist id))


;; Users

(defvar bugz-users-cache nil)
(defvar bugz-users-by-id nil)
(defvar bugz-users-by-login nil)

(defsubst bugz-user->id       (user) (elt user 0))
(defsubst bugz-user->name     (user) (elt user 1))
(defsubst bugz-user->login    (user) (elt user 2))

(defun bugz-users ()
  (or bugz-users-cache
      (setq bugz-users-by-id nil
            bugz-users-by-login nil
            bugz-users-cache
            (sqlquery profiles ((userid . string-to-number)
                                realname login_name)
                      nil nil nil
                      (lambda (user)
                        (setq bugz-users-by-id (cons (cons (bugz-user->id user)
                                                           user)
                                                     bugz-users-by-id)
                              bugz-users-by-login (cons (cons (bugz-user->login user)
                                                           user)
                                                     bugz-users-by-login)))))))

(defun bugz-user (id &optional dont-refetch)
  (bugz-users)
  (let ((user (if (numberp id)
                  (cdr-safe (assq id bugz-users-by-id))
                (cdr-safe (assoc id bugz-users-by-login)))))
    (or user
        (if dont-refetch
            nil
          (setq bugz-users-cache nil)
          (bugz-user id t)))))
        

;; Fields

(defvar bugz-fields-cache nil)
(defvar bugz-fields-by-id nil)

(defsubst bugz-field->id       (field) (elt field 0))
(defsubst bugz-field->name     (field) (elt field 1))
(defsubst bugz-field->desc     (field) (elt field 2))

(defun bugz-fields ()
  (or bugz-fields-cache
      (setq bugz-fields-by-id nil
            bugz-fields-cache
            (sqlquery fielddefs
                      ((id . string-to-number)
                       name description)
                      nil nil nil
                      (lambda (field)
                        (setq bugz-fields-by-id (cons (cons (bugz-field->id field)
                                                           field)
                                                     bugz-fields-by-id)))))))

(defun bugz-field-name (id)
  (bugz-fields)
  (bugz-field->desc (cdr (assq id bugz-fields-by-id))))

;; Bugs

(defun bugz-subtract-time (time secs)
  "Subtract SECS seconds from the the timespec TIME."
  (let ((hi (/ secs 65536))
        (lo (% secs 65536)))
    (if (> lo (nth 1 time))
        (setq hi (1+ hi)
              lo (+ lo 65536)))
    (list (- (nth 0 time) hi)
          (- (nth 1 time) lo)
          (nth 2 time))))

(defun bugz-sql-field-cond (field op value)
  (cond ((eq field 'owner)
         (format "assigned_to %s %d" op
                 (bugz-user->id (bugz-user value))))
        ((eq field 'milestone)
         (format "target_milestone %s '%s'" op value))))

(defun bugz-translate-query (query)
  (cond ((null query)
         "")

        ((eq (car query) '=)
         (let ((field (nth 1 query))
               (value (nth 2 query)))
           (bugz-sql-field-cond field "=" value)))

        ((eq (car query) 'like)
         (let ((field (nth 1 query))
               (value (nth 2 query)))
           (bugz-sql-field-cond field "like" value)))

        ((eq (car query) 'owner)
         (format "assigned_to = %d"
                 (bugz-user->id (bugz-user (cadr query)))))
        ((eq (car query) 'reporter)
         (format "reporter = %d"
                 (bugz-user->id (bugz-user (cadr query)))))
        ((eq (car query) 'milestone)
         (format "target_milestone = '%s'"
                 (cadr query)))
        ((eq (car query) 'status)
         (format "bug_status IN (%s)"
                 (cond ((stringp (cadr query))
                        (format "'%s'" (cadr query)))
                       ((eq (cadr query) 'open)
                        "'UNCONFIRMED','NEW','ASSIGNED','REOPENED'")
                       ((eq (cadr query) 'closed)
                        "'CLOSED'")
                       ((eq (cadr query) 'new)
                        "'UNCONFIRMED','NEW'"))))
        ((eq (car query) 'and)
         (concat "(" (mapconcat 'bugz-translate-query
                                (cdr query)
                                " AND ") ")"))
        ((eq (car query) 'or)
         (concat "(" (mapconcat 'bugz-translate-query
                                (cdr query)
                                " OR ") ")"))
        ((eq (car query) 'age<)
         (let ((secs (* (cadr query) 3600)))
           (concat "delta_ts > "
                   (format-time-string "%Y%m%d%H%M%S"
                                       (bugz-subtract-time (current-time) secs)))))
        (t
         (error "bad query"))))

(defun bugz-get-bugs (&optional query)
  "Search for bugs"
  (sqlquery bugs
            ((bug_id . string-to-number)
             short_desc
             (product_id . string-to-number)
             (component_id . string-to-number)
             version
             (assigned_to . string-to-number)
             (priority . intern)
             (bug_status . intern)
             (bug_severity . intern)
             (resolution . intern)
             (reporter . string-to-number)
             creation_ts
             delta_ts
             target_milestone)
            (replace-regexp-in-string "%" "%%" (bugz-translate-query query))))

(defun bugz-bug (id)
  "Get one bug"
  (car-safe
   (sqlquery bugs
             ((bug_id . string-to-number)
              short_desc
              (product_id . string-to-number)
              (component_id . string-to-number)
              version
              (assigned_to . string-to-number)
              (priority . intern)
              (bug_status . intern)
              (bug_severity . intern)
              (resolution . intern)
              (reporter . string-to-number)
              creation_ts
              delta_ts
              target_milestone
              keywords)
             "bug_id = %d"
             nil
             (id))))

(defun bugz-get-cc-numbers (bug-id)
  "Get CCs of a bug, as a list of user IDs"
  (mapcar (lambda (field) (string-to-number (elt field 1)))
	  (sqlquery cc
		    ((bug_id . string-to-number)
		     who)
		    "bug_id = %d" nil (bug-id))))

(defsubst bugz-bug->id          (bug) (elt bug 0))
(defsubst bugz-bug->desc        (bug) (elt bug 1))
(defsubst bugz-bug->product     (bug) (bugz-product-name (elt bug 2)))
(defsubst bugz-bug->component   (bug) (bugz-component-name (elt bug 3)))
(defsubst bugz-bug->version     (bug) (elt bug 4))
(defsubst bugz-bug->assigned    (bug) (elt bug 5))
(defsubst bugz-bug->priority    (bug) (elt bug 6))
(defsubst bugz-bug->status      (bug) (elt bug 7))
(defsubst bugz-bug->severity    (bug) (elt bug 8))
(defsubst bugz-bug->resolution  (bug) (elt bug 9))
(defsubst bugz-bug->reporter    (bug) (elt bug 10))
(defsubst bugz-bug->creation-ts (bug) (elt bug 11))
(defsubst bugz-bug->delta-ts    (bug) (elt bug 12))
(defsubst bugz-bug->milestone   (bug) (elt bug 13))
(defsubst bugz-bug->keywords    (bug) (elt bug 14))

(defun bugz-cc-list (bug-id)
  "list of CCs of a bug"
  (mapcar (lambda (uid) (elt (bugz-user uid) 2))
	  (bugz-get-cc-numbers bug-id)))

(defun bugz-cc-string (bug)
  "CCs of a bug as a string"
  (mapconcat (lambda (x) x) (bugz-cc-list (bugz-bug->id bug)) ", "))

;; Comments (longdescs)

(defun bugz-fixup-comment (s)
  (with-temp-buffer
    (insert s)
    (goto-char 0)
    (while (search-forward "\\" nil t)
      (forward-char -1)
      (cond ((looking-at "\\\\t")
             (replace-match "\t  "))
            ((looking-at "\\\\n")
             (replace-match "\n"))
            (t
             (delete-char 1)
             (forward-char 1))))
    (split-string (buffer-string) "\n")))

(defun bugz-comments (bug-id)
  "Get comments to a bug"
  (sqlquery longdescs
            ((bug_id . string-to-number)
             (who . string-to-number)
             (bug_when)
             (thetext . bugz-fixup-comment)) 
            "bug_id = %d" "bug_when" (bug-id)))

(defsubst bugz-comment->id       (bug) (elt bug 0))
(defsubst bugz-comment->who      (bug) (elt bug 1))
(defsubst bugz-comment->when     (bug) (elt bug 2))
(defsubst bugz-comment->text     (bug) (elt bug 3))

;; Bug activity

(defun bugz-bugactivity (bug-id)
  "Get the changelog of a bug"
  (sqlquery bugs_activity
            ((bug_id . string-to-number)
             (who . string-to-number)
             bug_when
             (fieldid . string-to-number)
             removed added) 
            "bug_id = %d" "bug_when" (bug-id)))

(defsubst bugz-bugactivity->id       (bug) (elt bug 0))
(defsubst bugz-bugactivity->who      (bug) (elt bug 1))
(defsubst bugz-bugactivity->when     (bug) (elt bug 2))
(defsubst bugz-bugactivity->field    (bug) (elt bug 3))
(defsubst bugz-bugactivity->removed  (bug) (elt bug 4))
(defsubst bugz-bugactivity->added    (bug) (elt bug 5))

;;; Bug list UI

(defvar bugz-list-mode-map nil
  "Keymap for bugz-list-mode")

(defvar bugz-list-autoselect t
  "*Automatically select bugs when moving to them")

(defvar bugz-list-changes-order 'oldest-first
  "*The order in which changes and comments are listed.
Possible values are oldest-first and newest-first.")

(defvar bugz-bugs nil)
(defvar bugz-bugs-grouped nil)

(if bugz-list-mode-map
    nil
  (setq bugz-list-mode-map (make-sparse-keymap))
  (define-key bugz-list-mode-map "q" 'bugz-list-quit)
  (define-key bugz-list-mode-map "g" 'bugz-list-refetch)
  (define-key bugz-list-mode-map "G" 'bugz-list)
  (define-key bugz-list-mode-map "d" 'bugz-list-group)
  (define-key bugz-list-mode-map "s" 'bugz-list-sort)
  (define-key bugz-list-mode-map "t" 'bugz-list-toggle-descending)
  (define-key bugz-list-mode-map "n" 'bugz-list-next)
  (define-key bugz-list-mode-map "p" 'bugz-list-previous)
  (define-key bugz-list-mode-map "\r" 'bugz-list-select)
  (define-key bugz-list-mode-map "\C-k" 'bugz-list-kill)
  (define-key bugz-list-mode-map " " 'bugz-list-scroll-up)
  (define-key bugz-list-mode-map "" 'bugz-list-scroll-down)
  (define-key bugz-list-mode-map [mouse-2] 'bugz-list-mouse2))

(defvar bugz-db-host "mysql5.hq.vtech"
  "Bug database host name")
(defvar bugz-db-database "bugs"
  "Bug database name")
(defvar bugz-db-user "bugs"
  "Bug database user name")
(defvar bugz-db-password "bugs"
  "Bug database password")
(defvar bugz-db-encoding 'utf-8
  "Bug database encoding")

(defun bugz-list-mode ()
  "Major mode for listing Bugzilla bugs

The format of the bugs in this list can be changed by setting the
variable `bugz-list-format'.  The list may also be sorted using the
\\[bugz-list-sort] and \\[bugz-list-toggle-descending] commands, and grouped using the \\[bugz-list-group] command.

Here is a complete list of available command in this mode:

\\{bugz-list-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (sqlquery-init bugz-db-host bugz-db-database bugz-db-user bugz-db-password
		 bugz-db-encoding)
  (toggle-read-only 1)
  ;; (make-local-variable 'bugz-bugs)
  (setq major-mode 'bugz-list-mode)
  (setq mode-name "Bugz list")
  (use-local-map bugz-list-mode-map)
  (setq truncate-lines t)
  (run-hooks 'bugz-list-mode-hook)
  (run-hooks 'bugz-common-mode-hook))

(defun bugz-list-next-bug-pos (pos)
  "Find where the next bug entry starts, searching from POS."
  (let ((start (next-single-property-change pos 'bugz-bug)))
    (if start
        (if (get-text-property start 'bugz-bug)
            start
          (bugz-list-next-bug-pos start))
      nil)))

(defun bugz-list-previous-bug-pos (pos)
  "Find where the next bug entry starts, searching from POS.
If POS is in a bug, the start of that bug is returned."
  (let ((start (previous-single-property-change pos 'bugz-bug)))
    (if start
        (if (get-text-property start 'bugz-bug)
            start
          (bugz-list-previous-bug-pos start))
      nil)))

(defun bugz-list-current-bug-pos (pos)
  "Find where the bug entry at POS starts."
  (if (get-text-property pos 'bugz-bug)
      (previous-single-property-change pos 'bugz-bug)
    nil))

(defun bugz-list-next (&optional count)
  (interactive "p")
  (let (pos bug)
    (while (and (> count 0)
                (setq pos (bugz-list-next-bug-pos (point))))
      (goto-char pos)
      (setq count (1- count))))
  (bugz-list-position-point)
  (if bugz-list-autoselect
      (bugz-list-select (point))))

(defun bugz-list-previous (&optional count)
  (interactive "p")
  (let ((pos (bugz-list-current-bug-pos (point))) bug)
    (if pos (goto-char pos))
    (while (and (> count 0)
                (setq pos (bugz-list-previous-bug-pos (point))))
      (goto-char pos)
      (setq count (1- count))))
  (bugz-list-position-point)
  (if bugz-list-autoselect
      (bugz-list-select (point))))

(defun bugz-list-position-point ()
  "Position point on bug"
  (if (not (get-text-property (point) 'bugz-bug))
      nil
    (forward-char 1)
    (goto-char (previous-single-property-change (point) 'bugz-bug))
    (search-forward ":")
    (let* ((wpos (window-start))
           (height (window-height))
           (toplines (count-lines wpos (point))))
      (cond ((and (> wpos 0) (< toplines 2))
             (recenter 3))
            ((< (- height toplines) 3)
             (recenter -3))))))

(defun bugz-list-kill ()
  "Omit bug at point from listing"
  (interactive)
  (let ((bug-pos (bugz-list-current-bug-pos (point)))
        (inhibit-read-only t))
    (if (not bug-pos)
        (error "No bug here"))
    (goto-char bug-pos)
    (delete-region (point) (next-single-property-change (point) 'bugz-bug))
    (bugz-list-position-point)))

(defvar bugz-query-history nil)
(defvar bugz-sort-history nil)
(defvar bugz-last-query nil)
(defvar bugz-group-key nil)

(defvar bugz-configure-windows t)

(defvar bugz-sort-keys
  '(("ID"           bugz-bug->id <)
    ("Priority"     bugz-bug->priority (lambda (x y) (string> x y)))
    ("Severity"     bugz-bug->severity severity<)
    ("Status"       bugz-bug->status status<)
    ("Milestone"    bugz-bug->milestone string<)
    ("Last changed" bugz-bug->delta-ts string<)
    ("Owner"        (lambda (x) (bugz-user->login
                                 (bugz-user (bugz-bug->assigned x)))) string<)
    ("Product/Component"
                    (lambda (x) (concat (bugz-bug->product x)
                                        "/"
                                        (bugz-bug->component x))) string<)))
(defvar bugz-sort-key (assoc "Last changed" bugz-sort-keys))
(defvar bugz-sort-desc t)

(defconst bugz-severities
  '(enhancement trivial minor normal major critical blocker))

(defun bugz-list-select (pos)
  "View bug under point"
  (interactive "d")
  (let ((bug (get-text-property pos 'bugz-bug)))
    (if (null bug)
        (message "No bug on line")
      (save-selected-window
        (select-window (bugz-bug-view-window t))
        (bugz-view-bug (bugz-bug->id bug) nil)))))

(defun bugz-compile-format (spec)
  (let ((buf (get-buffer-create " *bugz-format*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert spec)
      (goto-char (point-min))
      (let (parts)
        (while (not (eobp))
          (setq parts
                (cons 
                 (cond ((looking-at "%\\[")
                        (goto-char (match-end 0))
                        '(setq label-start (point)))
                       ((looking-at "%\\]")
                        (goto-char (match-end 0))
                        '(put-text-property label-start (point)
                                            'face 'bugz-label-face))
                       ((looking-at "%\\(-?[0-9]*\\)(\\([^):]*\\)\\(:[^)]*\\|\\))")
                        (goto-char (match-end 0))
                        (let* ((width (match-string 1))
                               (field (match-string 2))
                               (mod (match-string 3))
                               (getter (list (intern (concat "bugz-bug->"
                                                             field))
                                             'bug)))
                          (cond ((string= mod ":login")
                                 (setq getter `(bugz-user->login
                                                (bugz-user ,getter))))
                                ((string= mod ":name")
                                 (setq getter `(bugz-user->name
                                                (bugz-user ,getter)))))
                          `(insert (format ,(concat "%" width "s")
                                           ,getter))))
                       ((looking-at "[^%]*")
                        (goto-char (match-end 0))
                        `(insert ,(match-string 0)))
                       (t
                        (error "Strange...")))
                 parts)))
        `(lambda (bug)
           (let (label-start)
             ,@(nreverse parts)))))))
                 

(defvar bugz-list-format
  (concat "%5(id): %-8(status) %[%(desc)%]\n"
          "       %-8(assigned:login) %(delta-ts)\n"
          "                %(product)/%(component) %(version) %(severity) %(priority)\n"
          )
  "*The format for bugs in the bug list")

(defun bugz-list-refresh ()
  (interactive)
  (let ((bug-groups bugz-bugs-grouped)
        (inhibit-read-only t)
        (fmt (bugz-compile-format bugz-list-format)))
    (erase-buffer)
    (insert (bugz-label "Query: ") bugz-last-query
            (bugz-label "  Order: ")
            (or (car-safe bugz-sort-key)
                "Not sorted") " "
            (if bugz-sort-desc "descending" "ascending")
            "\n")
    (while bug-groups
      (insert "\n")
      (if bugz-group-key
          (insert (format "%s: %s\n"
                          (car bugz-group-key)
                          (funcall (nth 1 bugz-group-key)
                                   (car (car bug-groups))))))
      (let ((bugs (car bug-groups)))
        (while bugs
          (let ((bug (car bugs))
                (p (point)))
            (funcall fmt bug)
            (add-text-properties p (point)
                                 `(bugz-bug ,bug)))
          (setq bugs (cdr bugs)))
        (setq bug-groups (cdr bug-groups))))
    (set-buffer-modified-p nil)
    (let ((pos (text-property-not-all (point-min) (point-max)
                                      'bugz-bug nil)))
      (if (not pos)
              (goto-char (point-min))
        (goto-char pos)
        (bugz-list-position-point)))))

(defvar bugz-named-queries
  `(("All bugs" . nil)
    ("Changed in the last 24h" . (age< 24))
    ("Changed in the last 48h" . (age< 48))
    ("Changed in the last week" . (age< 168))
    ("Changed in the last month" . (age< ,(* 24 31)))
    ("Changed in the last month" . (age< ,(* 24 31)))
    ("Changed in the last two months" . (age< ,(* 24 61)))
    ("Open bugs" . (status open))
    ("New bugs" . (status new))
    ("Assigned to me" . (and (owner ,(user-login-name))
                             (status open)))
    ("My bugs" . (and (or (owner ,(user-login-name))
                          (reporter ,(user-login-name)))
                      (status open)))
    ("Milestone 1.4" . (milestone "1.4"))
    ("Milestone 1.6" . (milestone "1.6"))
    ("Milestone 1.8" . (milestone "1.8"))
    ("Milestone 2.0" . (milestone "2.0"))
    ("Milestone 2.2" . (milestone "2.2"))
    ("Milestone 2.2.0" . (milestone "2.2.0"))
    ("Milestone 3.0" . (like milestone "3.0%"))
    ("My 3.0" . (and (like milestone "3.0%")
                     (= owner ,(user-login-name))
                     (status open)))
    ("My 3.2" . (and (milestone "3.2")
                     (= owner ,(user-login-name))
                     (status open)))
    ("Milestone Future" . (milestone "Future"))
    ("Milestone undecided" . (milestone "---"))
    )
  "An alist of named predefined queries")

(defvar bugz-default-query "Changed in the last 24h"
  "The default query")

(defun bugz-list (query-name)
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read
                        (format "Query (%s): " (or bugz-last-query
                                                   bugz-default-query))
                        bugz-named-queries
                        nil t nil 'bugz-query-history
                        (or bugz-last-query bugz-default-query)))))
  (setq bugz-last-query query-name)
  (let ((buf (get-buffer-create "*Bug list*"))
        (query (cdr (assoc query-name bugz-named-queries))))
    (switch-to-buffer buf)
    (unless (eq major-mode 'bugz-list-mode)
      (bugz-list-mode))
    (message "Listing bugs...")
    (bugz-set-bugs (bugz-get-bugs query))
    (message "Listing bugs...done")
    (bugz-list-refresh)))

(defun bugz-list-refetch ()
  (interactive)
  (bugz-list bugz-last-query))

(defun severity< (sev1 sev2)
  (let ((sev bugz-severities)
        result)
    (while sev
      (cond ((and (eq sev1 (car sev))
                  (not (eq sev2 (car sev))))
             (setq result t sev nil))
            ((eq sev2 (car sev))
             (setq result nil sev nil))
            (t
             (setq sev (cdr sev)))))
    result))

(defconst bugz-statuses
  '(UNCONFIRMED NEW ASSIGNED REOPENED RESOLVED VERIFIED CLOSED))

(defun status< (st1 st2)
  (let ((st bugz-statuses)
        result)
    (while st
      (cond ((and (eq st1 (car st))
                  (not (eq st2 (car st))))
             (setq result t st nil))
            ((eq st2 (car st))
             (setq result nil st nil))
            (t
             (setq st (cdr st)))))
    result))

(defun bugz-sort-bugs (bugs key)
  "Sort the bugs in BUGS acccording to KEY.
The bug list is modified by side effect."
  (if key
      (let ((getter (nth 1 key))
            (<< (nth 2 key)))
        (sort bugs
              (if bugz-sort-desc
                  (lambda (a b)
                    (funcall <<
                             (funcall getter b)
                             (funcall getter a)))
                (lambda (a b)
                  (funcall <<
                           (funcall getter a)
                           (funcall getter b))))))
    bugs))

(defun bugz-group-bugs (bugs key)
  (cond ((not bugs)
         nil)
        ((not key)
         (list (copy-sequence bugs)))
        (t
         (let* ((bugz-sort-desc nil)
                (sorted (reverse (bugz-sort-bugs (copy-sequence bugs) key)))
                (getter (nth 1 key))
                (<< (nth 2 key))
                all this group)
           (setq this (list (car sorted))
                 group (funcall getter (car this))
                 sorted (cdr sorted))
           (while sorted
             (if (funcall << (funcall getter (car sorted)) group)
                 (setq all (cons this all)
                       this (list (car sorted))
                       group (funcall getter (car this)))
               (setq this (cons (car sorted) this)))
             (setq sorted (cdr sorted)))
           (cons this all)))))

(defun bugz-set-bugs (bugs)
  "Set bugz-bugs to a new list of bugs, the sort it into bugz-bugs-grouped"
  (setq bugz-bugs-grouped
        (mapcar (lambda (x) (bugz-sort-bugs x bugz-sort-key))
                (bugz-group-bugs bugs bugz-group-key)))
  (setq bugz-bugs (apply 'append bugz-bugs-grouped)))

(defun bugz-list-sort (key)
  (interactive
   (let ((last-sort (or (car-safe bugz-sort-key) ""))
         (completion-ignore-case t))
     (list (assoc (completing-read
                   (format "Sort by (%s): " last-sort)
                   bugz-sort-keys
                   nil t nil 'bugz-sort-history
                   last-sort)
                  bugz-sort-keys))))
  (setq bugz-sort-key key)
  (bugz-set-bugs bugz-bugs)
  (bugz-list-refresh))

(defun bugz-list-group (key)
  (interactive
   (let ((last-group (or (car-safe (rassq bugz-group-key
                                          bugz-sort-keys))
                         "No grouping"))
         (completion-ignore-case t))
     (list (assoc (completing-read
                   (format "Group by (%s): " last-group)
                   (cons '("No grouping" . nil)
                         bugz-sort-keys)
                   nil t nil 'bugz-sort-history
                   last-group)
                  bugz-sort-keys))))
  (setq bugz-group-key key)
  (bugz-set-bugs bugz-bugs)
  (bugz-list-refresh))

(defun bugz-list-toggle-descending ()
  (interactive)
  (setq bugz-sort-desc (not bugz-sort-desc))
  (bugz-set-bugs bugz-bugs)
  (bugz-list-refresh))

(defun bugz-list-quit()
  (interactive)
  (let ((proc-buf (get-buffer " *bugz*"))
        (bug-buf (get-buffer "*Bug*")))
    (when proc-buf
      (kill-buffer proc-buf))
    (when bug-buf
      (let ((win (get-buffer-window bug-buf)))
        (when win (delete-window win)))
      (kill-buffer bug-buf))
    (kill-buffer (current-buffer))))

(defun bugz-list-mouse2 (evt)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-end evt)))
    (bugz-list-select (posn-point (event-end evt)))))

(defun bugz-list-scroll-up (&optional arg)
  "Scroll bug window up"
  (interactive "P")
  (if (bugz-bug-view-window nil)
      (when (save-selected-window
              (select-window (bugz-bug-view-window t))
              (if (pos-visible-in-window-p (point-max))
                  t
                (scroll-up arg)))
        (bugz-list-next 1))
    (bugz-list-select (point))))

(defun bugz-list-scroll-down (&optional arg)
  "Scroll bug window down"
  (interactive "P")
  (when (save-selected-window
          (select-window (bugz-bug-view-window t))
          (if (pos-visible-in-window-p (point-min))
              t
            (scroll-down arg)))
    (bugz-list-previous 1)))
  

;;; Bug view UI

(defvar bugz-bug-id nil)

(defvar bugz-view-mode-map nil
  "Keymap for bugz-view-mode")

(if bugz-view-mode-map
    nil
  (setq bugz-view-mode-map (make-sparse-keymap))
  (define-key bugz-view-mode-map "q" 'bugz-view-quit)
  (define-key bugz-view-mode-map "g" 'bugz-view-refetch)
  (define-key bugz-view-mode-map "p" 'bugz-view-prev)
  (define-key bugz-view-mode-map "n" 'bugz-view-next)
  (define-key bugz-view-mode-map "\r" 'bugz-view-select)
  (define-key bugz-view-mode-map [mouse-2] 'bugz-view-mouse2))


(defun bugz-view-mode ()
  "Major mode for viewing Bugzilla bugs

Here is a complete list of available command in this mode:

\\{bugz-view-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (toggle-read-only 1)
  (sqlquery-init bugz-db-host bugz-db-database bugz-db-user bugz-db-password
		 bugz-db-encoding)
  (make-local-variable 'bugz-bug-id)
  (setq major-mode 'bugz-view-mode)
  (setq mode-name "Bugz view")
  (use-local-map bugz-view-mode-map)
  (run-hooks 'bugz-view-mode-hook)
  (run-hooks 'bugz-common-mode-hook))

(defface bugz-label-face
  '((t (:bold t)))
  "Face for labels in the bugz buffer")

(defface bugz-text-face
  '((t))
  "Face for text in the bugz buffer")

(defun bugz-label (str)
  (let ((s (copy-sequence str)))
    (put-text-property 0 (length s) 'face 'bugz-label-face s)
    s))

(defun bugz-merge-changelog (activities comments)
  "Merge a list of comments and a list of bug activities to a single list.
Each element in the new list is a vector [WHO WHEN ACTIVITIES COMMENTS]."
  (cond ((and activities
              (or (null comments)
                  (string< (bugz-bugactivity->when (car activities))
                           (bugz-comment->when (car comments)))))
         ;; The first activity is at least as old as the first comment
         (let ((merged (bugz-merge-changelog (cdr activities) comments))
               (act (car activities)))
           (if (and merged
                    (eq (elt (car merged) 0)
                        (bugz-bugactivity->who act))
                    (equal (elt (car merged) 1)
                           (bugz-bugactivity->when act)))
               (aset (car merged) 2 (cons act (aref (car merged) 2)))
             (setq merged (cons (vector (bugz-bugactivity->who act)
                                        (bugz-bugactivity->when act)
                                        (list act)
                                        '())
                                merged)))
           merged))
        (comments
         ;; The first comment is at least as old as the first activity
         (let ((merged (bugz-merge-changelog activities (cdr comments)))
               (com (car comments)))
           (if (and merged
                    (eq (elt (car merged) 0)
                        (bugz-comment->who com))
                    (equal (elt (car merged) 1)
                           (bugz-comment->when com)))
               (aset (car merged) 3 (cons com (aref (car merged) 3)))
             (setq merged (cons (vector (bugz-comment->who com)
                                        (bugz-comment->when com)
                                        '()
                                        (list com))
                                merged)))
           merged))
        (t
         '())))

(defun bugz-view-refetch ()
  (interactive)
  (message "Fetching bug %d..." bugz-bug-id)
  (let* ((bug (bugz-bug bugz-bug-id))
         (descs (bugz-comments bugz-bug-id))
         (activ (bugz-bugactivity bugz-bug-id))
         (changes (bugz-merge-changelog activ descs))
         (inhibit-read-only t))
    (message nil)
    (erase-buffer)
    (insert (bugz-label "Bug # ") (number-to-string (bugz-bug->id bug))
            (bugz-label "    last change: ") (bugz-bug->delta-ts bug) "\n")
    (let ((url (format "http://bugzilla.hq.vtech/show_bug.cgi?id=%d"
                       (bugz-bug->id bug)))
          (p (point)))
      (insert url)
      (add-text-properties p (point) `(mouse-face highlight url ,url)))
    (insert "\n\n"
            (bugz-label "  Description: ") (bugz-bug->desc bug) "\n"
            (bugz-label "  Status:      ") (symbol-name (bugz-bug->status bug))
            " " (symbol-name (bugz-bug->resolution bug)) "\n"
            (bugz-label "  Keywords:    ") (bugz-bug->keywords bug) "\n"
            (bugz-label "  Assigned to: ") (bugz-user->name (bugz-user (bugz-bug->assigned bug))) "\n"
            (bugz-label "  Reporter:    ") (bugz-user->name (bugz-user (bugz-bug->reporter bug))) "\n"
            (bugz-label "  Prod/comp:   ")
            (bugz-bug->product bug) "/" (bugz-bug->component bug) " " (bugz-bug->version bug) "\n"
            (bugz-label "  Severity:    ") (symbol-name (bugz-bug->severity bug)) "\n"
            (bugz-label "  Milestone:   ") (bugz-bug->milestone bug) "\n"
	    (bugz-label "  CC:          ") (bugz-cc-string bug) "\n"
            )
    (if (eq bugz-list-changes-order 'newest-first)
        (setq changes (nreverse changes)))
    (while changes
      (insert "\n--- "
              (bugz-label (bugz-user->name (bugz-user (elt (car changes) 0))))
              "  " (elt (car changes) 1) "\n")
      (mapcar '(lambda (act)
                 (insert (format "%s: %s\n"
                                 (bugz-field-name (bugz-bugactivity->field act))
                                 (bugz-bugactivity->added act))))
              (elt (car changes) 2))
      (mapcar '(lambda (comment)
                 (insert "\n")
                 (mapcar '(lambda (s)
                            (insert s "\n"))
                         (bugz-comment->text comment)))
              (elt (car changes) 3))
      (setq changes (cdr changes)))

    ;; Scan for bug references
    (goto-char (point-min))
    (while (re-search-forward "\\(bug\\|duplicate of\\|BugsThisDependsOn:\\|OtherBugsDependingOnThis:\\) ?#?\\([0-9]+\\)" nil t)
      (add-text-properties (match-beginning 2) (match-end 2)
                           `(face underline
                             mouse-face highlight
                             bug ,(string-to-number (match-string 2)))))

    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun bugz-bug-view-buffer (create)
  "Return the bug buffer.
If there is no such buffer, and CREATE is non-nil, create it."
  (get-buffer-create "*Bug*"))  

(defun bugz-bug-view-window (create)
  "Return the window displaying the bug buffer.
If there is no such window, and CREATE is non-nil, create it."
  (let ((buf (bugz-bug-view-buffer create)))
    (and buf
         (or (get-buffer-window buf)
             (and create
                  (if bugz-configure-windows
                      (save-selected-window
                        (delete-other-windows)
                        (split-window nil (/ (window-height (selected-window)) 4))
                        (set-window-buffer (next-window) buf)
                        (next-window))
                    (display-buffer buf t)))))))

(defun bugz-view-bug (bug-id &optional other-window)
  (interactive "nBug #")
  (let ((buf (bugz-bug-view-buffer t)))
    (if other-window
        (switch-to-buffer-other-window buf)
      (switch-to-buffer buf))
    (unless (eq major-mode 'bugz-view-mode)
      (bugz-view-mode))
    (setq bugz-bug-id bug-id)
    (bugz-view-refetch)))

(defun bugz-view-quit ()
  (interactive)
  (let* ((buf (current-buffer))
         (win (get-buffer-window buf)))
    (when win
      (condition-case e
          (delete-window win)
        (error)))
    (kill-buffer buf)))

(defun bugz-view-next ()
  (interactive)
  (let ((bugs bugz-bugs))
    (while (and bugs (/= bugz-bug-id (bugz-bug->id (car bugs))))
      (setq bugs (cdr bugs)))
    (if (and bugs (cdr bugs))
        (bugz-view-bug (bugz-bug->id (car (cdr bugs))))
      (error "No next bug in list"))))
    
(defun bugz-view-prev ()
  (interactive)
  (let ((bugs bugz-bugs)
        prev)
    (while (and bugs (/= bugz-bug-id (bugz-bug->id (car bugs))))
      (setq prev (car bugs))
      (setq bugs (cdr bugs)))
    (if prev
        (bugz-view-bug (bugz-bug->id prev))
      (error "No previous bug in list"))))

(defun bugz-view-mouse2 (evt)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-end evt)))
    (bugz-view-select (posn-point (event-end evt)))))

(defun bugz-view-select (pos)
  "Press a button at point.  The view buffer should be current."
  (interactive "d")
  (let ((url (get-text-property pos 'url))
        (bug (get-text-property pos 'bug)))
      (cond (url
             (browse-url url))
            (bug
             (bugz-view-bug bug))
            (t
             (error "Not a button")))))
