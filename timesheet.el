;;; timesheet.el --- Timesheet management add-on for org-mode

;; Copyright © 2014-2018 Informatique, Inc.

;; Author: Tom Marble
;; URL: https://github.com/tmarble/timesheet.el
;; Version: 0.4.1
;; Created: 2018-08-01
;; Keywords: org timesheet
;; Package-Requires: ((s "1") (org "7") (auctex "11"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Debian-depends: emacs24 make gawk sed git tar rubber texlive-latex-extra texlive-fonts-recommended texlive-fonts-extra evince
;;
;; This library adds timetracking and invoice generation to org-mode
;; and relies heavily on
;; org clocking http://orgmode.org/org.html#Clocking-work-time
;; and TODO items http://orgmode.org/org.html#TODO-Items
;; and org spreadsheets http://orgmode.org/org.html#The-spreadsheet
;;
;; This library attempts to conform to packaging conventions:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html
;; Bugs, enhancements welcome!

;;; Usage

;; Ensure TEXINPUTS is set to (in your ~/.bashrc)
;; export TEXINPUTS=.:$HOME/.emacs.d/elpa/auctex-11.87.4/latex:
;;
;; Start by creating an example client...
;;   M-x timesheet-example
;;   You will be viewing the buffer yoyodyne.org that already has some example
;;   time entries... Create an invoice with
;;   M-x timesheet-invoice-this
;;
;; Next steps...
;; - customize your name (in defs.tex) and logo (in logo.pdf).
;; - update some time entries.
;;
;; Example key bindings
;;  see example.emacs.d/foo/bindings.el

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 's)
(require 'org)
(require 'org-clock)
(require 'org-table)

;; vars

;; timesheet-version should match the Version comment above
(defconst timesheet-version "0.4.1")

(defconst timesheet-path (file-name-directory (or load-file-name (buffer-file-name))))

(defconst timesheet-currencies
  '(("USD" . "$")
    ("GBP" . "£")
    ("EUR" . "€")))

;; customizations

(defgroup timesheet nil
  "Timesheet functions"
  :tag "Timesheet"
  :group 'org)

(defcustom timesheet-invoice-number 100
  "Next invoice number."
  :type '(integer)
  :group 'timesheet)

(defcustom timesheet-invoice-script
  (expand-file-name "timesheet-invoice" (expand-file-name "bin" timesheet-path))
  "Script to create a timesheet invoice."
  :type 'string
  :group 'timesheet)

(defcustom timesheet-company-dir
  (expand-file-name "Timesheet" "~")
  "Parent directory for timesheet invoices."
  :type 'string
  :group 'timesheet)

(defcustom timesheet-always-recalculate-times nil
  "When non-nil, recalculate all times before creating invoices.
Typically the user is expected to run `timesheet-calc-today' at
the end of each day, creating the corresponding entry under the
\"Timesheet\" heading.  If this option is non-nil, all day
entries will be updated before creating an invoice.  This may
slow down invoice creation."
  :type 'boolean
  :group 'timesheet)

(defcustom timesheet-default-clocktable-options
  ":maxlevel 4 :emphasize nil"
  "Default options to add to the reporting clocktables."
  :type 'string
  :group 'timesheet)

(defcustom timesheet-clocktable-include-properties nil
  "A list of property names (as strings) to add to clocktables."
  :type 'list
  :group 'timesheet)

(defcustom timesheet-before-period-report-hook nil
  "Hook run before a period report is made.
The hook runs after the appropriate heading has been
found/created, but before the clocktable is created/updated.
Point is at the clocktable #+BEGIN line, if the table already
exists, or at bol where the #+BEGIN line will be inserted."
  :type 'hook
  :group 'timesheet)

(defcustom timesheet-after-period-report-hook nil
  "Hook run after a period report is made.
Point is at the #+BEGIN line of the report clocktable."
  :type 'hook
  :group 'timesheet)

;; get the next invoice number (and increment the counter)
;; if the argument is given.. set the next invoice number
;;;###autoload
(defun timesheet-next-invoice (&optional invoice)
  "Get next invoice number (following `timesheet-invoice-number' or INVOICE if present)."
  (interactive)
  (let* ((next-invoice (if invoice invoice timesheet-invoice-number))
         (arg (list 'timesheet-invoice-number (1+ next-invoice))))
    (custom-set-variables arg)
    (custom-save-all)
    next-invoice))

;; debug functions
;; these will probably be removed from a future version...

(defvar timesheet-debug-msg-delay 3)

;;;###autoload
(defun timesheet-debug-msg (&rest msgs)
  "Display some debug MSGS."
  (interactive)
  (message (apply 'concat msgs))
  (redisplay t)
  (sleep-for timesheet-debug-msg-delay))

;;;###autoload
(defun timesheet-debug-time (time &rest msgs)
  "Display TIME with some debug MSGS."
  (interactive)
  (let ((at-time (if time time (current-time))))
    (apply 'timesheet-debug-msg (cons (format-time-string "%Y-%m-%d %a %H:%M:%S" at-time) msgs))))

;;;###autoload
(defun timesheet-debug-time-cal (time-cal &rest msgs)
  "Display TIME-CAL with some debug MSGS."
  (interactive)
  (apply 'timesheet-debug-time (cons (apply 'encode-time time-cal) msgs)))

;; functions

(defun timesheet-get-file-property (property)
  "Return the value of the file PROPERTY (or nil if not found)"
  (cdr (assoc-string property org-file-properties t)))

(defun timesheet-get-currency-symbol (currency)
  "Return the currency symbol for CURRENCY (or nil if not found)"
  (cdr (assoc-string currency timesheet-currencies)))

(defun timesheet-template-files ()
  "Return a list of pathnames for timesheet template files."
  (let ((share-dir (expand-file-name "share" timesheet-path)))
    (directory-files share-dir t (concat "\\." (regexp-opt '("tex" "pdf" "org"))  "\\'"))))

(defun timesheet-round-time-down (tl)
  "Round the time in TL (timelist format) to the previous quarter hour (default).
The constant roundmin can be set to any value between 1 and 60 (default 15)."
  (let* ((s (car tl))
         (m (nth 1 tl))
         (h (nth 2 tl))
         (rest (nthcdr 3 tl))
         (roundmin-str (org-table-get-constant "roundmin"))
         (roundmin (if (string= roundmin-str "#UNDEFINED_NAME") 15
                      (string-to-number roundmin-str))))
    (cons s
          (cons (* (/ m roundmin) roundmin)
                (cons h rest)))))

(defun timesheet-round-time-up (tl)
  "Round the time in TL (timelist format) do the next quarter hour."
  (let* ((s (car tl))
         (m (nth 1 tl))
         (h (nth 2 tl))
         (rest (nthcdr 3 tl))
         (roundmin-str (org-table-get-constant "roundmin"))
         (roundmin (if (string= roundmin-str "#UNDEFINED_NAME") 15
                      (string-to-number roundmin-str)))
         (md (nth 1 (timesheet-round-time-down tl)))
         (mu (* (min (/ 60 roundmin) ;; most chunks possible is 4 * 15 = 60
                     (floor (/ (+ m (floor (* 0.67 roundmin)))
                               roundmin))) ;; within 5 min of 15 min boundary
                roundmin)) ; 15 min chunks
         tup)
    (if (> mu md)
        (setq m mu)
      (setq m md))
    (when (= m 60)
      (setq m 0)
      (setq h (+ h 1))) ;; BUG! overflow days!
    (setq tup (cons s (cons m (cons h rest))))
    tup))

;; timesheet-get-heading-path should return
;; ("Tasks" "ProjectName" "GoalName" "TaskName")
(defun timesheet-get-heading-path ()
  "Return the full heading path."
  ;; NOTE: for a brief moment a version of org worked with simply (org-get-outline-path)
  (append (org-get-outline-path) (list (nth 4 (org-heading-components)))))

(defun timesheet-find-or-create-olp (segments &optional sortedp)
  "Find the heading corresponding to outline path SEGMENTS.
Each element of SEGMENTS should be a string representing exact
header text.  Creates any headings that do not exist, and returns
a marker pointing to the final header.

When optional SORTEDP is non-nil, any newly-created heading
segments will be inserted in sorted order among their siblings,
if any.

The current value of `case-fold-search' will affect which path
segments are considered to be \"found\"."
  ;; First, assume it exists.
  (let ((found-marker (ignore-errors (org-find-olp segments t)))
	(sorter (when sortedp
		  (if (functionp sortedp) sortedp #'string<)))
	(crawl 0)
	partial segment)
    (save-excursion
      (if found-marker found-marker
	(while segments
	  (setq found-marker
		(condition-case nil
		    (org-find-olp
		     (setq partial
			   (append partial
				   (list (setq segment (pop segments)))))
		     t)
		  (error
		   ;; If there's no found-marker then we're inserting
		   ;; a top-level heading, so stick it at the end of
		   ;; the buffer.
		   (if found-marker
		       (progn
			 (goto-char found-marker)
			 (if (and sorter
				  (org-goto-first-child))
			     (progn
			       (while (and (funcall
					    sorter
					    (nth 4 (org-heading-components))
					    segment)
					   (/= crawl (point)))
				 (setq crawl (point))
				 (org-forward-heading-same-level 1))
			       ;;Gross.
			       (org-insert-heading (when (= crawl (point))
						     '(4))))
			   (end-of-line)
			   (org-insert-subheading '(4))))
		     (goto-char (point-max))
		     (org-insert-heading nil nil 'top-level))
		   (insert segment)
		   (beginning-of-line)
		   (point-marker)))))
	found-marker))))

;;;###autoload
(defun timesheet-clock-update-timeclock (&optional withpath)
  "If this is a CLOCK line, update /round it and return t.
Otherwise, return nil.  Optionally using WITHPATH."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (when (looking-at org-clock-string)
      (let ((re (concat "[ \t]*" org-clock-string
			" *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
			"\\([ \t]*=>.*\\)?\\)?"))
	    ts te h m s neg dts dte ets ete secs fh)
	(cond
	 ((not (looking-at re))
	  nil)
	 ((not (match-end 2))
	  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
		     (> org-clock-marker (point))
		     (<= org-clock-marker (point-at-eol)))
	    ;; The clock is running here
	    (setq org-clock-start-time
		  (apply 'encode-time
			 (org-parse-time-string (match-string 1))))
	    (org-clock-update-mode-line)))
	 (t
	  (and (match-end 4) (delete-region (match-beginning 4) (match-end 4)))
	  (end-of-line 1)
	  (setq ts (match-string 1)
		te (match-string 3))
          (setq dts (timesheet-round-time-down (org-parse-time-string ts))
                dte (timesheet-round-time-up (org-parse-time-string te)))
          (setq ets (apply 'encode-time dts)
                ete (apply 'encode-time dte))
	  (setq ts (format-time-string "%Y-%m-%d %a %H:%M" ets)
                te (format-time-string "%Y-%m-%d %a %H:%M" ete))
	  (setq s (- (org-float-time ete)
		     (org-float-time ets))
		neg (< s 0)
		s (abs s)
                secs s
                fh (/ secs 3600.0)
		h (floor (/ s 3600))
		s (- s (* 3600 h))

		m (floor (/ s 60))
		s (- s (* 60 s)))
	  (insert " => " (format "%s -- %s @ %5.2f" ts te fh))
          (if withpath
              (list ets ete fh (timesheet-get-heading-path))
            (list ets ete fh))))))))

(defun timesheet-same-day-p (t1 t2)
  "Return true of T1 and T2 (timelist format) are on the same day."
  (let* ((dt1 (decode-time t1))
         (dt2 (decode-time t2)))
    (and (= (nth 5 dt1) (nth 5 dt2))
         (= (nth 4 dt1) (nth 4 dt2))
         (= (nth 3 dt1) (nth 3 dt2)))))

(defun timesheet-ymd-to-time (date-string)
  "Convert a date string like YYYY-MM-DD to a time value.
Returns the value of midnight on that day."
  (encode-time (append
		'(0 0 0)
		(nthcdr 3 (parse-time-string date-string)))))

(defun timesheet-midnight (day-time)
  "Round DAY-TIME to midnight on that day."
  (let* ((day-time-cal (decode-time day-time))
         (day-cal (append '(0 0 0)
                          (nthcdr 3 day-time-cal)))
         (day (apply 'encode-time day-cal)))
    day))

(defun timesheet-add-days (time days)
  "Offset TIME by a positive (or negative) number of DAYS."
  (let* ((day (* 60 60 24))
         (d (abs days))
         (time-func (if (< days 0) 'time-subtract 'time-add))
         (time2 (funcall time-func time (seconds-to-time (* d day)))))
    time2))

;;;###autoload
(defun timesheet-today ()
  "Date for calculating timesheet: today."
  (interactive)
  (let* ((now (current-time))
         (today (timesheet-midnight now)))
    today))

;;;###autoload
(defun timesheet-yesterday ()
  "Date for calculating timesheet: yesterday."
  (interactive)
  (timesheet-add-days (timesheet-today) -1))

;;;###autoload
(defun timesheet-at-point ()
  "Date for calculating timesheet: current clock line."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (when (looking-at org-clock-string)
      (let ((re (concat "[ \t]*" org-clock-string
			" *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
			"\\([ \t]*=>.*\\)?\\)?")))
        (when (looking-at re)
          (timesheet-midnight (apply 'encode-time (org-parse-time-string (match-string 1)))))))))

(defun timesheet-cmp-task (atask btask)
  "Compare two tasks and return t if ATASK < BTASK."
  (let* ((apath (car atask))
         (bpath (car btask))
         (alen (length apath))
         (blen (length bpath))
         cmp)
    (if (> alen blen)
        (setq cmp t)
      (when (= alen blen)
        ;; return true of the components of apath are lexicographically after bpath
        (while apath
          (let* ((a (pop apath))
                 (b (pop bpath)))
            (cond ((string< b a)
                   (setq cmp t)
                   (setq apath nil)) ;; we know a < b, stop
                  ((string= b a)
                   nil) ; keep comparing along apath
                  (t
                   (setq apath nil))) ;; we know a > b, stop
            ))
        ))
    cmp))

(defun timesheet-paths-same-p (a b)
  "Return t if paths A and B are the same."
  (when (and (listp a)
             (listp b)
             (= (length a) (length b)))
    (let* ((cmp t)
           (a0 (pop a))
           (b0 (pop b)))
      (while a0
        (if (string= a0 b0)
            (progn
              (setq a0 (pop a))
              (setq b0 (pop b)))
          (progn
            (setq a0 nil)
            (setq cmp nil))))
      cmp)))

(defun timesheet-rollup-times (clocks)
  "Sort and rollup CLOCKS."
  (let* (clock
         task
         tasks
         rtasks
         (day-hours 0.0)
         (project-hours 0.0)
         (goal-hours 0.0)
         (task-hours 0.0)
         prev-task-path
         prev-goal-path
         prev-project-path)
    (dolist (clock clocks)
      (setq task (list (cdr (nth 3 clock)) (car clock) (nth 1 clock) (nth 2 clock)))
      (push task tasks))
    ;; sort tasks by path: longest, descending
    (setq rtasks (sort tasks 'timesheet-cmp-task))
    ;; go through.. when path changes, make roll up entry (path start stop total-hours)
    (setq tasks nil)
    (dolist (task rtasks)
      (let* ((task-path (car task))
             (goal-path (butlast task-path))
             (project-path (butlast goal-path))
             (hours (nth 3 task))
             (p (length task-path)))
        ;; if the path is not the same, then make rollup for previous
        (unless (timesheet-paths-same-p task-path prev-task-path)
          (unless (= task-hours 0.0)
            (push (list prev-task-path nil nil task-hours) tasks)
            (setq task-hours 0.0))
          (unless (timesheet-paths-same-p goal-path prev-goal-path)
            (unless (= goal-hours 0.0)
              (push (list prev-goal-path nil nil goal-hours) tasks)
              (setq goal-hours 0.0))
            (unless (timesheet-paths-same-p project-path prev-project-path)
              (unless (= project-hours 0.0)
                (push (list prev-project-path nil nil project-hours) tasks)
                (setq project-hours 0.0)))))
        ;; add this leaf task
        (push task tasks)
        ;; add totals
        (when (> p 0)
          (when (> p 1)
            (when (> p 2)
              (setq task-hours (+ task-hours hours)))
            (setq goal-hours (+ goal-hours hours)))
          (setq project-hours (+ project-hours hours)))
        (setq day-hours (+ day-hours hours))
        (setq prev-task-path task-path)
        (setq prev-goal-path goal-path)
        (setq prev-project-path project-path)
        ))
    (unless (= task-hours 0.0)
      (push (list prev-task-path nil nil task-hours) tasks))
    (unless (= goal-hours 0.0)
      (push (list prev-goal-path nil nil goal-hours) tasks))
    (unless (= project-hours 0.0)
      (push (list prev-project-path nil nil project-hours) tasks))
    (push (list nil nil nil day-hours) tasks)
    tasks))

(defun timesheet-calc (day)
  "Calculate timesheet for the given DAY."
  (let* ((clocks (timesheet-clocks day (timesheet-add-days day 1)))
         (tasks (timesheet-rollup-times clocks))
         (day-hours (nth 3 (pop tasks))))
    (goto-char (timesheet-find-or-create-olp
		(list "Timesheet"
		      (format-time-string "%Y-%m" day))
		t))
    ;; Delete contents of heading.
    (org-cut-subtree)
    (org-insert-subheading '(4))
    ;; Insert updated data for the day.
    (insert (format "%s = %3.2f hours\n"
		    (format-time-string "%Y-%m-%d %a" day) day-hours))
    (forward-line -1)
    (end-of-line)
    (pcase-dolist (`(,path ,start ,stop ,hours) tasks)
      ;; Don't print clock entries.
      (unless start
	(insert "\n***" (make-string (length path) "*"))
        (insert (format " %s = %3.2f hours" (car (last path)) hours))))))

(defun timesheet-clocks (start-time end-time)
  "Return a list of clocks for the time interval given by START-TIME and END-TIME."
  (save-excursion
    (save-restriction
      (let (day-times sehp)
        (goto-char (point-max))
        (beginning-of-line 1)
        (while (not (bobp))
          (setq sehp (timesheet-clock-update-timeclock t)) ;; start end hours path
          (when (and sehp
                     (listp sehp)
                     (time-less-p start-time (car sehp))
                     (time-less-p (car sehp) end-time))
            (push sehp day-times))
          (beginning-of-line 0))
        day-times))))

;;;###autoload
(defun timesheet-calc-today ()
  "Calculate timesheet for today."
  (interactive)
  (timesheet-calc (timesheet-today)))

;;;###autoload
(defun timesheet-calc-yesterday ()
  "Calculate timesheet for yesterday."
  (interactive)
  (timesheet-calc (timesheet-yesterday)))

;;;###autoload
(defun timesheet-calc-at-point ()
  "Calculate timesheet for the date on this line."
  (interactive)
  (let ((day (timesheet-at-point)))
    (if day
        (timesheet-calc day)
      (message (concat "no " org-clock-string " at point!")))))

(defun timesheet-calc-all ()
  "Calculate timesheet for all dates in the buffer."
  (let (seen day)
    (save-excursion
      (goto-char (point-max))
      (while (null (bobp))
	(when (and (timesheet-clock-update-timeclock)
		   (null (member (setq day (timesheet-at-point)) seen)))
	  (timesheet-calc-at-point)
	  ;; This "seen" guard provides a bit of efficiency, but every
	  ;; call to `timesheet-calc-at-point' is still running over
	  ;; every clock line in the buffer.
	  (push day seen))
	(beginning-of-line 0)))))

(defun timesheet-cmp-string-lists (asl bsl)
  "Compare two string lists and return t if ASL < BSL."
  (let ((i 0)
        (cmp 0)) ;; -1 <, 0 =, 1 >
    (while (and (= cmp 0) (> (length asl) i) (> (length bsl) i))
      (let ((a (nth i asl))
            (b (nth i bsl)))
        (if (string< a b)
            (setq cmp -1)
          (if (not (string= a b))
              (setq cmp 1)))
        )
      (setq i (1+ i)))
    (when (and (= cmp 0) (< (length asl) (length bsl)))
      (setq cmp -1))
    (= cmp -1)))

(defun timesheet-project-times (&optional start end)
  "Get list of all project times under the Timesheet heading.
If optional START and END are given, only return times in that
range.

Return value is a list of times that look like, for instance:

(\"2019-10\" \"2019-10-28 Mon = 7.00 hours\" \"Report writing = 3.00 hours\")"
  (save-excursion
    (save-restriction
      (goto-char (timesheet-find-or-create-olp '("Timesheet")))
      (when (or timesheet-always-recalculate-times
		;; Timesheet subtree is empty.
		(save-excursion
		  (null (org-goto-first-child))))
	(timesheet-calc-all))
      (let ((date-re "^[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}")
	    path project-times date)
	(org-map-tree
	 (lambda ()
	   (setq path (timesheet-get-heading-path))
	   (when (and (= (length path) 4)
		      (null (member path project-times))
		      ;; If we're going to compare a start/end, then
		      ;; parse the date.
		      (or (null (or start end))
			  (and (string-match
				date-re
				(nth 2 path))
			       (setq date
				     (timesheet-ymd-to-time
				      (match-string 0 (nth 2 path))))))
		      (or (null start)
			  (time-less-p start date))
		      (or (null end)
			  (time-less-p date end)))
	     (push (cdr path) project-times))))
        (sort project-times 'timesheet-cmp-string-lists)))))

(defun timesheet--make-timesheet-heading-text (date period)
  "Make a timesheet report heading for DATE.
PERIOD may be one of the symbols `day', `week', or `month'."
  (pcase period
    ('day (format-time-string "%Y-%m-%d" date))
    ('week (format "%s week #%d: %s - %s"
            (format-time-string "%Y" date)
            (timesheet-week-number date)
            (format-time-string "%B %d" date)
            (format-time-string "%B %d" (timesheet-add-days date 6))))
    (_ (format-time-string "%B" date))))

;;;###autoload
(defun timesheet-table-goto (top col row)
  "In the table given at TOP move to a position COL ROW."
  (interactive)
  (goto-char top)
  (forward-line row)
  (org-table-goto-column col))

(defun timesheet-period (start period-or-end)
  "Produce a timesheet for a period of time.
START is a time value.  PERIOD-OR-END is either a time value
representing the end, or one of the symbols `day', `week', or
`month'.

The timesheet will be inserted under a dedicated heading in the
buffer.  If PERIOD-OR-END is one of the pre-set symbols, the
timesheet will be inserted under a heading corresponding to that
period.  Otherwise it will go under the heading \"Other\"."
  (let ((end (if (consp period-or-end) period-or-end
	       (pcase period-or-end
		 ('day (timesheet-add-days start 1))
		 ('week (timesheet-add-days start 7))
		 ('month (timesheet-first-day-next-month start)))))
	table-top)
    (goto-char (timesheet-find-or-create-olp
		(list
		 (pcase period-or-end
		   ('day "Daily")
		   ('week "Weekly")
		   ('month "Monthly")
		   (_ "Other"))
		 (timesheet--make-timesheet-heading-text
		  start period-or-end))))
    (save-restriction
      (org-narrow-to-subtree)
      (org-end-of-meta-data)
      (cond
       ((looking-at "^$"))
       ((eolp) (newline))
       ((looking-at org-heading-regexp)
	(org-open-line 1)))
      (if (re-search-forward "^#\\+BEGIN: clocktable" nil t)
	  (progn
	    (beginning-of-line)
	    (run-hooks 'timesheet-before-period-report-hook))
	(setq table-top (point-marker))
	(run-hooks 'timesheet-before-period-report-hook)
	(insert (format "#+BEGIN: clocktable %s :scope file %s %s"
			timesheet-default-clocktable-options
			(if timesheet-clocktable-include-properties
			    (format
			     "%S" timesheet-clocktable-include-properties)
			  "")
			(format ":tstart \"%s\" :tend \"%s\""
				(format-time-string
				 "%Y-%m-%d" start)
				(format-time-string
				 "%Y-%m-%d" end)))
		"\n#+END: clocktable\n")
	(goto-char table-top))
      (org-dblock-update)
      (run-hooks 'timesheet-after-period-report-hook))))

(defun timesheet-week-time (time)
  "Round TIME to beginning of the week."
  (let* ((time-cal (decode-time time))
         (dow (nth 6 time-cal)) ;; 0 == Sunday
         (day (* 60 60 24))) ; in seconds
    (when (= dow 0)
      (setq dow 7))
    (timesheet-midnight (time-subtract time (seconds-to-time (* day (1- dow)))))))

(defun timesheet-week-number (time)
  "Calculate the ISO week number for this TIME."
  (let* ((w-str (format-time-string "%W" time))
         (w (string-to-number w-str)))
    (1+ w)))

;;;###autoload
(defun timesheet-this-week ()
  "Date for calculating timesheet: today."
  (interactive)
  (let* ((now (current-time))
         (week (timesheet-week-time now)))
    week))

;;;###autoload
(defun timesheet-last-week ()
  "Date for calculating timesheet: yesterday."
  (interactive)
  (let* ((this (timesheet-this-week))
         (day (* 60 60 24)) ; in seconds
         (last (time-subtract this (seconds-to-time (* 7 day)))))
    last))

;;;###autoload
(defun timesheet-weekly-this ()
  "Calculate timesheet this week."
  (interactive)
  (timesheet-period (timesheet-this-week) 'week))

;;;###autoload
(defun timesheet-weekly-last ()
  "Calculate timesheet last week."
  (interactive)
  (timesheet-period (timesheet-last-week) 'week))

;;;###autoload
(defun timesheet-weekly-at-point ()
  "Calculate week for the date on this line."
  (interactive)
  (let ((day (timesheet-at-point)))
    (if day
        (timesheet-period day 'week)
      (message (concat "no " org-clock-string " at point!")))))

;;;###autoload
(defun timesheet-daily-today ()
  "Calculate timesheet for today."
  (interactive)
  (timesheet-period (timesheet-today) 'day))

;;;###autoload
(defun timesheet-daily-yesterday ()
  "Calculate timesheet for yesterday."
  (interactive)
  (timesheet-period (timesheet-yesterday) 'day))

;;;###autoload
(defun timesheet-daily-at-point ()
  "Calculate timesheet for day under point."
  (interactive)
  (let ((day (timesheet-at-point)))
    (if day
        (timesheet-period (timesheet-midnight day) 'day)
      (message (concat "no " org-clock-string " at point!")))))

;; NOTE: this is not handled particularly well... and it needs a better user interface
;; (defun timesheet-overlap (day)
;;   "calculate timesheet overlap for today"
;;   (interactive)
;;   (let* ((clocks (timesheet-clocks day))
;;          (sorted-clocks (sort clocks (lambda (a b) (time-less-p (car a) (car b)))))
;;          times overlap
;;          last-start last-end
;;          start end
;;          path
;;          )
;;     (goto-char (point-max))
;;     (insert "\n* Overlap\n")
;;     (dolist (sehp sorted-clocks)
;;       (setq start (car sehp))
;;       (setq end (nth 1 sehp))
;;       (setq path (nth 3 sehp))
;;       (cond ((and last-end (time-less-p start last-end)) ;; overlap
;;              (setq overlap t)
;;              )
;;             ((and last-start (timesheet-same-day-p start last-start)) ;; same day
;;              nil
;;              )
;;             (last-start ;; new day (save previous day)
;;              (setq overlap nil)))
;;       (when overlap
;;         (insert (format "OVERLAP at %s in %s\n"
;;                         (format-time-string "%Y-%m-%d %a %H:%M" end)
;;                         path)))
;;       (setq last-start start)
;;       (setq last-end end)
;;       )
;;     (message "check overlap")
;;     ))

;; (defun timesheet-overlap-today ()
;;   "calculate timesheet for today"
;;   (interactive)
;;   (timesheet-overlap (timesheet-today)))

;; (defun timesheet-overlap-yesterday ()
;;   "calculate timesheet for yesterday"
;;   (interactive)
;;   (timesheet-overlap (timesheet-yesterday)))

(defun timesheet-currency (v)
  "Return currency value for V."
  (let* ((currency (timesheet-get-file-property "Currency"))
         (currency-symbol (timesheet-get-currency-symbol (or currency "USD")))
         (fv (format "%s%3.2f" currency-symbol (or v 0)))
         (len (length fv)))
    (cond ((>= v 1000000.00)
           (concat (substring fv 0 (- len 9)) ","
                   (substring fv (- len 9) (- len 6)) ","
                   (substring fv (- len 6))))
          ((>= v 1000.00)
           (concat (substring fv 0 (- len 6)) ","
                   (substring fv (- len 6))))
          (t
           fv))))

(defun timesheet-month-time (&optional time)
  "Round `current-time' (or TIME if given) to beginning of the month."
  (let* ((at-time (if time time (current-time)))
         (time-cal (decode-time at-time))
         (first-cal (list (car time-cal)
                          (nth 1 time-cal)
                          (nth 2 time-cal)
                          1 ;; first day of month
                          (nth 4 time-cal)
                          (nth 5 time-cal))))
    (timesheet-midnight (apply 'encode-time first-cal))))

;;;###autoload
(defun timesheet-this-month ()
  "Date for calculating timesheet: this month."
  (interactive)
  (timesheet-month-time))

(defun timesheet-days-in-month (year month)
  "How many days in the month given by YEAR MONTH."
  (let* ((jason '(0 31 28 31 30 31 30 31 31 30 31 30 31))
         (days (nth month jason)))
    (if (and (= month 2) (date-leap-year-p year))
        (1+ days)
      days)))

;;;###autoload
(defun timesheet-last-month ()
  "Date for calculating timesheet: last month."
  (interactive)
  (let* ((this-month (timesheet-this-month))
         (secs-per-day (* 60 60 24))) ; in seconds
    (timesheet-month-time (time-subtract this-month (seconds-to-time secs-per-day)))))

;;;###autoload
(defun timesheet-last-day-in-month (&optional time)
  "Return the date for the last day in this month.
Current month or month for TIME if present."
  (interactive)
  (let* ((at-time (if time time (current-time)))
         (time-cal (decode-time at-time))
         (month (nth 4 time-cal))
         (year (nth 5 time-cal))
         (last-cal (list 0 ;(car time-cal)
                         0 ; (nth 1 time-cal)
                         0 ;  (nth 2 time-cal)
                         (timesheet-days-in-month year month) ;; last day of month
                         month
                         year)))
    (apply 'encode-time last-cal)))

;;;###autoload
(defun timesheet-first-day-next-month (&optional time)
  "Return the date for the first day in the next month.
Current month or month for TIME if present."
  (interactive)
  (let* ((last-day (timesheet-last-day-in-month time))
         (secs-per-day (* 60 60 24))) ; in seconds
    (timesheet-month-time (time-add last-day (seconds-to-time secs-per-day)))))

;;;###autoload
(defun timesheet-invoice-this ()
  "Calculate invoice this month."
  (interactive)
  (timesheet-invoice-monthly (timesheet-this-month)))

;;;###autoload
(defun timesheet-invoice-at-point ()
  "Calculate invoice at point (a CLOCK line)."
  (interactive)
  (let ((day (timesheet-at-point)))
    (if day
        (timesheet-invoice-monthly day)
      (message (concat "no " org-clock-string " at point!")))))

;;;###autoload
(defun timesheet-invoice-last ()
  "Calculate invoice last month."
  (interactive)
  (timesheet-invoice-monthly (timesheet-last-month)))

(defun timesheet-invoice-weekly-at-point ()
  "Calculate week for the date on this line."
  (interactive)
  (let ((day (timesheet-at-point)))
    (if day
        (timesheet-invoice-weekly day)
      (message (concat "no " org-clock-string " at point!")))))

(defun timesheet-american-month (month)
  "Using MONTH return Month DD, YYYY."
  (let* ((mname (format-time-string "%B" month))
         (m (nth 3 (decode-time month)))
         (space (if (< m 10) "" " "))
         (dd-yyyy (format-time-string "%e, %Y" month)))
    (concat mname space dd-yyyy)))

(defun timesheet-invoice-weekly (week)
  "Prepare invoice for week starting WEEK."
  (let* ((heading (format-time-string "%Y-%m-%d" week))
	 (customer (org-table-get-constant "customer"))
         (invoice-dir
          (expand-file-name
	   heading
           (expand-file-name "Invoices"
                             (expand-file-name customer
                                               timesheet-company-dir))))
	 (next-week (timesheet-add-days week 7))
	 (all-project-times (timesheet-project-times week next-week))
	 detail-top row total-hours amount-due invoice-str)
    (goto-char (timesheet-find-or-create-olp
		`("Invoices" "Weekly" ,heading) 'sorted))
    ;; Get or set invoice prop.
    (setq invoice-str (or (org-entry-get nil "TIMESHEET_INVOICE")
			  (let ((inv (number-to-string
				      (timesheet-next-invoice))))
			    (org-entry-put (point) "TIMESHEET_INVOICE"
					   inv)
			    inv)))
    (make-directory invoice-dir t)
    ;; Goto Header heading.
    (goto-char (timesheet-find-or-create-olp
		`("Invoices" "Weekly" ,heading "Header")))
    (save-excursion
      (org-end-of-meta-data)
      (unless (looking-at-p org-complex-heading-regexp)
	(delete-region
	 (point) (save-excursion (org-end-of-subtree t t)))))
    (org-entry-put (point) "EXPORT_FILE_NAME"
		   (expand-file-name "header.tex" invoice-dir))
    (org-end-of-meta-data)
    (insert "Invoice No.: " invoice-str "\n\n")
    (insert "Date Submitted: " (format-time-string "%Y-%m-%d" (current-time))
	    "\n\n")
    (insert "#+begin_billto\n")
    (insert (org-entry-get (point) "BillTo1" t) "\n")
    (insert (org-entry-get (point) "BillTo2" t) "\n")
    (insert (org-entry-get (point) "BillTo3" t) "\n")
    (insert "#+end_billto\n\n")
    (insert "#+begin_remitto\n")
    (insert (org-entry-get (point) "RemitTo1" t) "\n")
    (insert (org-entry-get (point) "RemitTo2" t) "\n")
    (insert (org-entry-get (point) "RemitTo3" t) "\n")
    (insert "#+end_remitto\n")
    ;; Export this heading only, as a latex "chunk" that will later be
    ;; included in the template.
    (org-export-to-file 'latex
	(expand-file-name "header.tex" invoice-dir)
      nil 'subtree nil 'body-only)
    (goto-char (timesheet-find-or-create-olp
		`("Invoices" "Weekly" ,heading "Detail")))
    (save-excursion
      (org-end-of-meta-data)
      (unless (looking-at-p org-complex-heading-regexp)
	(delete-region
	 (point) (save-excursion (org-end-of-subtree t nil)))))
    (org-entry-put (point) "EXPORT_FILE_NAME"
		   (expand-file-name "detail.tex" invoice-dir))
    (org-end-of-meta-data)
    (setq detail-top (point-marker))
    (insert "|----------+-------------+----------+--------+-----------|----|\n")
    (insert "| Date     | Description | Quantity |   Rate |    Amount |  A |\n")
    (insert "|----------+-------------+----------+--------+-----------|----|\n")
    (insert "|          |             |          |        |           |    |\n")
    (insert "|----------+-------------+----------+--------+-----------|----|\n")
    (insert "| /Month/  |             |     0.00 |        |      0.00 |    |\n")
    (insert "|----------+-------------+----------+--------+-----------|----|\n")
    (insert "#+TBLFM:$4=$rate;%3.2f;::$6=$3*$rate;%3.2f;::$5='(timesheet-currency $6);N::@>$3=vsum(@2$3..@-1$3);%3.2f;::@>$4=string(\"/Total/\");::@>$6=vsum(@2$6..@-1$6;%3.2f;::@>$5='(timesheet-currency (apply '+ '(@2$6..@-1$6)));N::\n")
    (setq row 3)
    (timesheet-table-goto detail-top 1 row)
    ;; insert project times
    (let (prev-day prev-projects prev-hours)
      (dolist (pt all-project-times)
        (let ((day-total (nth 1 pt)) ;; 2013-06-11 Tue = 6.50 hours
              (project-total (nth 2 pt)) ;; SuperProject = 6.50 hours
              day project hours)
          (when (string-match "^\\([0-9\-]+\\) ... = \\([0-9\.]+\\) hours" day-total)
            (setq day (match-string 1 day-total))
            (setq hours (match-string 2 day-total))
            (when (string-match "^\\(.+\\) = \\([0-9\.]+\\) hours" project-total)
              (setq project (match-string 1 project-total)))
            (when (and prev-day (not (string= day prev-day))) ;; emit prev-day
              (insert prev-day)
              (org-table-next-field)
              (insert prev-projects)
              (org-table-next-field)
              (insert prev-hours)
              (org-table-insert-row t)
              (setq row (1+ row))
              (setq prev-projects nil))
            (if prev-projects ;; add project to this days project list
                (setq prev-projects (concat prev-projects ", " project))
              (setq prev-projects project))
            (setq prev-day day)
            (setq prev-hours hours)
            )))
      ;; emit last day (if there has been at least one day)
      (when (and prev-day prev-projects prev-hours)
        (insert prev-day)
        (org-table-next-field)
        (insert prev-projects)
        (org-table-next-field)
        (insert prev-hours)))
    ;; compute formulae in table
    (org-table-iterate)
    (timesheet-table-goto detail-top 1 2)
    (org-table-insert-row t)
    (timesheet-table-goto detail-top 5 3)
    (insert "<r>")
    (org-table-next-field)
    (insert "<2>")
    (org-table-align)
    (org-export-to-file 'latex
	(expand-file-name "detail.tex" invoice-dir)
      nil 'subtree nil 'body-only)
    (outline-up-heading 1)))

(defun timesheet-invoice-monthly (month)
  "Prepare invoice for the given MONTH."
  ;; if this is a new invoice, get the next invoice number
  ;; else preserve the existing number
  (let* ((yyyy-mm (format-time-string "%Y-%m" month))
         (customer (org-table-get-constant "customer"))
         (invoice-dir
          (expand-file-name yyyy-mm
                            (expand-file-name "Invoices"
                                              (expand-file-name customer
                                                                timesheet-company-dir))))
         (next-month (timesheet-first-day-next-month month))
         (all-project-times (timesheet-project-times))
         (invoice-top (make-marker))
	 invoice-str
         header-top
         detail-top
         row
         total-hours amount-due)
    (make-directory invoice-dir t)
    (goto-char (move-marker
		invoice-top
		(timesheet-find-or-create-olp
		 `("Invoices" "Monthly" ,yyyy-mm) 'sorted)))
    ;; Get or set invoice prop.
    (setq invoice-str (or (org-entry-get nil "TIMESHEET_INVOICE")
			  (let ((inv (number-to-string
				      (timesheet-next-invoice))))
			    (org-entry-put (point) "TIMESHEET_INVOICE"
					   inv)
			    inv)))
    (goto-char (timesheet-find-or-create-olp
		`("Invoices" "Monthly" ,yyyy-mm "Header")))
    (save-excursion
      (org-end-of-meta-data)
      (unless (looking-at-p org-complex-heading-regexp)
	(delete-region
	 (point) (save-excursion (org-end-of-subtree t t)))))
    (org-set-property "BillDate" (timesheet-american-month next-month))
    (org-set-property "DueDate" (timesheet-american-month (timesheet-last-day-in-month next-month)))
    (org-set-property "TotalHours" "0.00")
    (org-set-property "AmountDue" "0.00")
    (org-set-property "TABLE_EXPORT_FILE" (expand-file-name "header.tsv" invoice-dir))
    (org-set-property "TABLE_EXPORT_FORMAT" "orgtbl-to-tsv")
    (org-set-property "PDF" (concat "file://" invoice-dir "/Invoice-" invoice-str ".pdf"))
    (org-end-of-meta-data)
    (setq header-top (point-marker))
    (insert "#+BEGIN:\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "| /Remit To/    | /Date/       | /Invoice #/ |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "|               |              |             |\n")
    (insert "|               |              |             |\n")
    (insert "|               |              |             |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "| /Bill To/     | /Terms/      | /Due Date/  |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "|               |              |             |\n")
    (insert "|               |              |             |\n")
    (insert "|               |              |             |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "| /Total Hours/ | /Amount Due/ | /Enclosed/  |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "|               |              |             |\n")
    (insert "|---------------+--------------+-------------|\n")
    (insert "#+TBLFM: @2$1='(concat \"$PROP_RemitTo1\");::@2$2='(concat \"$PROP_BillDate\");::@2$3=$PROP_TIMESHEET_INVOICE;N::@3$1='(concat \"$PROP_RemitTo2\");::@4$1='(concat \"$PROP_RemitTo3\");::@6$1='(concat \"$PROP_BillTo1\");::@6$2='(concat \"$PROP_Terms\");::@6$3='(concat \"$PROP_DueDate\");::@7$1='(concat \"$PROP_BillTo2\");::@8$1='(concat \"$PROP_BillTo3\");::@10$1='(concat \"$PROP_TotalHours\");%3.2f::@10$2='(timesheet-currency $PROP_AmountDue);::\n")
    (insert "#+END:\n")
    (goto-char (timesheet-find-or-create-olp
		`("Invoices" "Monthly" ,yyyy-mm "Detail")))
    (save-excursion
      (org-end-of-meta-data)
      (unless (looking-at-p org-complex-heading-regexp)
	(delete-region
	 (point) (save-excursion (org-end-of-subtree t t)))))
    ;; Get or set invoice prop.
    (org-set-property "TABLE_EXPORT_FILE" (expand-file-name "detail.tsv" invoice-dir))
    (org-set-property "TABLE_EXPORT_FORMAT" "orgtbl-to-tsv")
    (org-end-of-meta-data)
    (setq detail-top (point-marker))
    (insert "#+BEGIN:\n")
    (insert "|----------+-------------+----------+--------+-----------|----|\n")
    (insert "| Date     | Description | Quantity |   Rate |    Amount |  A |\n")
    (insert "|----------+-------------+----------+--------+-----------|----|\n")
    (insert "|          |             |          |        |           |    |\n")
    (insert "|----------+-------------+----------+--------+-----------|----|\n")
    (insert "| /Month/  |             |     0.00 |        |      0.00 |    |\n")
    (insert "|----------+-------------+----------+--------+-----------|----|\n")
    (insert "#+TBLFM:$4=$rate;%3.2f;::$6=$3*$rate;%3.2f;::$5='(timesheet-currency $6);N::@>$3=vsum(@2$3..@-1$3);%3.2f;::@>$4=string(\"/Total/\");::@>$6=vsum(@2$6..@-1$6;%3.2f;::@>$5='(timesheet-currency (apply '+ '(@2$6..@-1$6)));N::\n")
    (insert "#+END:\n")
    (setq row 4)
    (timesheet-table-goto detail-top 1 row)
    ;; insert project times
    (let (prev-day prev-projects prev-hours)
      (dolist (pt all-project-times)
        (let* ((ym (car pt)) ;; 2013-06
               (day-total (nth 1 pt))     ;; 2013-06-11 Tue = 6.50 hours
               (project-total (nth 2 pt)) ;; SuperProject = 6.50 hours
               day project hours)
          (when (string= ym yyyy-mm) ;; this month
            (message (format "entry %s == %s" ym yyyy-mm))
            (when (string-match "^\\([0-9\-]+\\) ... = \\([0-9\.]+\\) hours" day-total)
              (setq day (match-string 1 day-total))
              (setq hours (match-string 2 day-total))
              (when (string-match "^\\(.+\\) = \\([0-9\.]+\\) hours" project-total)
                (setq project (match-string 1 project-total)))
              (when (and prev-day (not (string= day prev-day))) ;; emit prev-day
                (insert prev-day)
                (org-table-next-field)
                (insert prev-projects)
                (org-table-next-field)
                (insert prev-hours)
                (org-table-insert-row t)
                (setq row (1+ row))
                (setq prev-projects nil))
              (if prev-projects ;; add project to this days project list
                  (setq prev-projects (concat prev-projects ", " project))
                (setq prev-projects project))
              (setq prev-day day)
              (setq prev-hours hours)
              ))))
      ;; emit last day (if there has been at least one day)
      (when (and prev-day prev-projects prev-hours)
        (insert prev-day)
        (org-table-next-field)
        (insert prev-projects)
        (org-table-next-field)
        (insert prev-hours)))
    ;; compute formulae in table
    (org-table-iterate)
    (timesheet-table-goto detail-top 1 2)
    (org-table-insert-row t)
    (timesheet-table-goto detail-top 5 3)
    (insert "<r>")
    (org-table-next-field)
    (insert "<2>")
    (org-table-align)
    ;; export
    (org-table-export)
    (timesheet-table-goto detail-top 3 (+ row 3))
    (setq total-hours (s-trim (substring-no-properties (caar (org-table-copy-region (point) (point))))))
    (timesheet-table-goto detail-top 6 (+ row 3))
    (setq amount-due (s-trim (substring-no-properties (caar (org-table-copy-region (point) (point))))))
    (org-get-last-sibling) ; Detail
    (org-get-last-sibling) ; Header
    ;; set properties
    (org-set-property "TotalHours" total-hours)
    (org-set-property "AmountDue" amount-due)
    (timesheet-table-goto header-top 2 16)
    ;; update table
    (org-table-iterate)
    (org-table-align)
    (org-table-export)
    (timesheet-run timesheet-invoice-script "-d" "-v" "-i" invoice-dir "-p")
    (goto-char invoice-top)
    (message (concat yyyy-mm " Invoice #" invoice-str))))

(defun timesheet-run (script &rest args)
  "Run a company specific SCRIPT (with optional ARGS) to generate the timesheet."
  (let ((buffer-name "*timesheet-run*"))
    (unless (file-executable-p script)
      (user-error "The script does not exist: %s" script))
    (with-output-to-temp-buffer buffer-name
      (let ((rv (apply 'call-process (append (list script nil buffer-name script) args))))
        (if (= rv 0)
            (message (format "%s successful" script ))
          (message (format "%s failed with %d" script rv)))
        rv))))

;;;###autoload
(defun timesheet-example ()
  "Setup a timesheet example with a customer called Yoyodyne."
  (interactive)
  (let* ((org-file "yoyodyne.org")
         (customer "Yoyodyne")
         (share-dir (file-name-as-directory (expand-file-name "share" timesheet-company-dir)))
         (customer-dir (file-name-as-directory (expand-file-name customer timesheet-company-dir)))
         (customer-org (expand-file-name org-file customer-dir)))
    (message (format "Making timesheet example with customer: %s" customer))
    (make-directory share-dir t)
    (make-directory customer-dir t)
    (dolist (f (timesheet-template-files))
      (if (s-ends-with? ".org" f)
          (copy-file f customer-org t)
        (copy-file f share-dir t)))
    ;; open a buffer with customer-org
    (find-file customer-org)))

(provide 'timesheet)
;;; timesheet.el ends here
