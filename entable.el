
;;; entable.el --- A package to convert between org-table and org-headings  -*- lexical-binding: t; -*-

;; Package-Requires: ((org "9.1") (seq "2.20"))

;;; Commentary:
;; This package provides functions to transform org-tables into org-headings and vice versa.

;;; Code:

(defun entable/-mapcar* (fn &rest lists)
  "Apply FN to successive elements from each of LISTS.
Stop when the shortest list runs out."
  (unless (null lists)
    (let ((result ())
          (min-length (apply #'min (mapcar #'length lists))))
      (dotimes (i min-length (nreverse result))
        (push (apply fn (mapcar (lambda (list) (nth i list)) lists))
              result)))))

(defun entable/headings-to-list (begin end)
  "Convert org headings in region from BEGIN to END into list."
  (let (result)
    ;; Parse only the selected region, not the entire buffer
    (save-restriction
      (narrow-to-region begin end)
      (let ((tree (org-element-parse-buffer)))
        ;; Map the headlines to create the result list
        (org-element-map tree 'headline
          (lambda (headline)
            (let* ((title (org-element-property :raw-value headline))
                   (subheadings (org-element-map (org-element-contents headline) 'headline
                                  (lambda (subheading) (org-element-property :raw-value subheading)))))
              (when subheadings
                (push (cons title subheadings) result))))))
      ;; Pad the result list to same length
      (nreverse result))))


(defun entable/insert-list-as-org-table (list)
  "Convert LIST of strings into an org table and insert it at the end of the buffer."
  (let* ((max-columns (apply 'max (mapcar 'length list)))
         (table (make-vector max-columns nil)))

    ;; Populate the table vector
    (dotimes (i max-columns)
      (let ((column (make-list (length list) "")))
        ;; Prepare the newline separated string for each column
        (dotimes (j (length list))
          (setf (nth j column) (or (nth i (nth j list)) "")))
        (setf (aref table i) column)))

    ;; Insert the table
    (dotimes (i max-columns)
      (insert "| " (mapconcat 'identity (aref table i) " | ") " |\n")
      ;; Insert the heading separator after the first row
      (when (= i 0)
        (insert "|-" (make-string (* max-columns 2) ?-) "-|\n")))

    (org-table-align)))

(defun entable/replace-region-with-table ()
  "Replace the headings within the region with an org table."
  (interactive)
  (let ((begin (region-beginning))
        (end (region-end)))
    (when (and (org-at-heading-p) (not (org-at-table-p)))
      (let* ((headings (entable/headings-to-list begin end)))
        (delete-region begin end)
        (entable/insert-list-as-org-table headings)))))

(defun entable/replace-table-with-headings ()
  "Replace the org table at point with org headings, ignoring cells with only whitespace."
  (interactive)
  (let* ((current-level (or (org-current-level) 0))
         (table (entable/org-table-to-list)))
    (delete-region (org-table-begin) (org-table-end))
    (dolist (row table)
      (insert (make-string (1+ current-level) ?*) " "
              (car row) "\n")
      (dolist (subheading (cdr row))
        (unless (string-match-p "^\\s-*$" subheading) ; Ignore cells with only whitespace
          (insert (make-string (+ current-level 2) ?*) " "
                  subheading "\n"))))))


(defun entable/org-table-to-list ()
  "Converts an org table to a list."
  (interactive)
  (let* ((table (if (org-at-table-p) (org-table-to-lisp) (user-error "Not at a table")))
         (header (mapcar #'substring-no-properties (car table)))
         (rows (cdr table))
         (content (mapcar
                   (lambda (row)
                     (mapcar
                      (lambda (cell)
                        (substring-no-properties cell))
                      row))
                   (seq-filter #'sequencep rows)))
         (transposed (apply #'entable/-mapcar* #'list header content)))
    transposed))

(provide 'entable)
;;; entable.el ends here
