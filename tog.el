;;; tog.el --- NLP data tagging utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.3
;; Package-Requires: ((emacs "26") (ov "1.0.6") (s "1.12.0"))
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
(require 'ov)
(require 's)
(require 'tog-utils)

(defcustom tog-buffer-name "*tog*"
  "Name of the tagging buffer")

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
  (let ((file-path "./tog-tags.json")
        (tags))
    (dolist (it tog-items)
      (if (and (slot-boundp it :tag) (oref it :tag))
          ;; JSON needs string keys
          (push (cons (number-to-string (oref it :id)) (oref conv tag)) tags)))
    (f-write (json-encode-alist tags) 'utf-8 file-path)
    (message "Tags saved at %s" file-path)))

(defmacro tog-bind-char-keys (&rest chars)
  (let ((idx 0)
        (defuns nil))
    (dolist (c chars)
      (let ((command-sym (make-symbol (format "tog-tag-%s" idx))))
        (push `(progn
                 (defun ,command-sym () (interactive) (tog-tag ,idx))
                 (define-key tog-mode-map (kbd ,(string c)) ',command-sym))
              defuns))
      (cl-incf idx))
    `(progn ,@defuns)))

(tog-bind-char-keys ?1 ?2 ?3 ?4 ?5)

(provide 'tog)

;;; tog.el ends here
