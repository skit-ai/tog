;;; tog.el --- NLP data tagging utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.1.0
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
(require 'tog-conv)

(defcustom tog-buffer-name "*tog*"
  "Name of the tagging buffer")

(defvar tog-source-file nil
  "Path to the source file where we have the data.")

(defvar tog-items nil
  "List of item objects for tagging.")

(defvar tog-index nil
  "Index of current item being tagged.")

(defvar tog-types nil
  "Types like LOC, NAME etc. allowed for a task.")

(defun tog-next ()
  "Go to next item for tagging."
  (interactive)
  (if (= (length tog-items) (+ 1 tog-index))
      (message "Reached last item")
    (cl-incf tog-index)
    (tog-show (nth tog-index tog-items))))

(defun tog-prev ()
  "Go to previous item for tagging."
  (interactive)
  (if (zerop tog-index)
      (message "Reached first item")
    (cl-decf tog-index)
    (tog-show (nth tog-index tog-items))))

(defun tog-save ()
  "Save tags in a file"
  (interactive)
  (let ((file-path (concat tog-source-file ".tog"))
        (tags))
    (dolist (it tog-items)
      (if (oref it :tag)
          ;; JSON needs string keys
          (push (cons (number-to-string (oref it :id)) (oref it :tag)) tags)))
    (f-write (json-encode-alist tags) 'utf-8 file-path)
    (message "Tags saved at %s" file-path)))

;;;###autoload
(define-derived-mode tog-mode org-mode "tog"
  "Major mode for togging."
  (setq header-line-format '(:eval (format "  Doing item %s. Total %s." (+ 1 tog-index) (length tog-items)))))

;;;###autoload
(defun tog ()
  (interactive)
  (if (null tog-items)
      (message "No data loaded, try running a loader.")
    (setq tog-index -1)
    (tog-next)))

;;;###autoload
(defun tog-quit ()
  "Exit and cleanup."
  (interactive)
  (tog-save)
  (if (bufferp tog-buffer-name)
      (kill-buffer tog-buffer-name))
  (setq tog-items nil
        tog-index nil
        tog-source-file nil
        tog-types nil))

(provide 'tog)

;;; tog.el ends here
