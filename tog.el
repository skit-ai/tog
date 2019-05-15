;;; tog.el --- NLP data tagging utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.2.8
;; Package-Requires: ((emacs "26") (ov "1.0.6") (s "1.12.0") (f "0.20.0") (dash "2.15.0") (helm "3.2"))
;; URL: https://github.com/lepisma/tog

;;; Commentary:

;; NLP data tagging utilities
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 's)
(require 'tog-conv)
(require 'tog-progress)

(defcustom tog-buffer-name "*tog*"
  "Name of the tagging buffer. This is the shared buffer for all
kind of tagging. Note that only one kind of tagging can happen at
a given time.")

(defcustom tog-next-hook nil
  "Hook after next is pressed.")

(defvar tog-source-file nil
  "Path to the json source file where we have the data. The
assumption is that the file contains a list of dictionaries each
with at least an `id' key. The rest is open for interpretation by
the main loading code.")

(defvar tog-items nil
  "List of item objects for tagging. Each object is identified
using an `id' key.")

(defvar tog-index nil
  "Index of current item being tagged in the list `tog-items'.")

(defvar tog-types nil
  "Types like LOC, NAME etc. allowed for a task. If only one item
is present in the list, we assume that to be the default while
tagging.")

(defun tog-goto (idx)
  "Jump to idx item for tagging. Boundary handling is done in
this function so the caller need not worry about anything other
+/-."
  (setq tog-index (min (- (length tog-items) 1) (max 0 idx)))
  (tog-show (nth tog-index tog-items))
  (cond ((= tog-index 0) (message "Reached first item"))
        ((= tog-index (- (length tog-items) 1)) (message "Reached last item"))))

(defun tog-next ()
  "Go to next item for tagging."
  (interactive)
  (tog-goto (+ tog-index 1))
  (run-hooks 'tog-next-hook))

(defun tog-next-untagged ()
  "Go to next item which is untagged, ignoring current one."
  (interactive)
  (let ((jump-index (+ tog-index 1)))
    (while (and (oref (nth jump-index tog-items) :tag)
                (< jump-index (length tog-items)))
      (cl-incf jump-index))
    (tog-goto jump-index)
    (run-hooks 'tog-next-hook)))

(defun tog-prev-untagged ()
  "Go to previous item which is untagged, ignoring current one."
  (interactive)
  (let ((jump-index (- tog-index 1)))
    (while (and (oref (nth jump-index tog-items) :tag)
                (>= jump-index 0))
      (cl-decf jump-index))
    (tog-goto jump-index)))

(defun tog-prev ()
  "Go to previous item for tagging."
  (interactive)
  (tog-goto (- tog-index 1)))

(defun tog-save ()
  "Save tags in a file. We generate a sibling file with the same
name as the source file and add a suffix to it. This can be
changed but I don't feel that is ever really needed."
  (interactive)
  (let* ((canon-source-file (s-chop-suffix ".gz" tog-source-file))
         (file-path (concat canon-source-file ".tog"))
         (tags))
    (dolist (it tog-items)
      (if (oref it :tag)
          ;; JSON needs string keys
          (push (cons (number-to-string (oref it :id)) (oref it :tag)) tags)))
    (f-write (json-encode-alist tags) 'utf-8 file-path)
    (message "Tags saved at %s" file-path)))

(defun tog-load-tags ()
  "Load tags from the sibling file and apply to current items.
Tags are stored as map from item-id to tag objects represented as
list of alist."
  (interactive)
  (if (null tog-source-file)
      (error "tog-source-file not defined, load a data file first.")
    (let* ((canon-source-file (s-chop-suffix ".gz" tog-source-file))
           (file-path (concat canon-source-file ".tog"))
           (json-array-type 'list))
      (if (not (f-exists? file-path))
          (message "Tag file not found. Loading nothing")
        ;; TODO: This can be sped up if I choose right data structures. Not a
        ;;       problem at the moment though.
        (dolist (tag-data (json-read-file file-path))
          (dolist (tog-item tog-items)
            (if (= (oref tog-item :id)
                   (string-to-number (symbol-name (car tag-data))))
                (dolist (tag (cdr tag-data))
                  (update-tag tog-item tag)))))))))

;;;###autoload
(define-derived-mode tog-mode org-mode "tog"
  "Major mode for togging."
  :after-hook (setq header-line-format '(:eval (tog-progress-build-header)))
  (read-only-mode))

;;;###autoload
(defun tog ()
  "Start tagging after the variables `tog-items', `tog-types' and
`tog-method' are all set."
  (interactive)
  (if (null tog-items)
      (message "No data loaded, try running a loader.")
    (setq tog-index -1)
    (tog-next)))

;;;###autoload
(defun tog-quit ()
  "Exit and cleanup. We don't save before exiting by default
since this might do bad things to tag file if the user is exiting
because of an screw up while tagging."
  (interactive)
  (when (get-buffer tog-buffer-name)
    (kill-buffer tog-buffer-name))
  (setq tog-items nil
        tog-index nil
        tog-source-file nil
        tog-types nil))

(provide 'tog)

;;; tog.el ends here
