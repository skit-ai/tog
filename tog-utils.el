;;; tog-utils.el --- Tog utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Tog utilities
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

(defun maplines-pos (fn)
  "Map over line (position based not text based). FN takes no
argument and is called by iterating the point over lines."
  (save-excursion
    (goto-char (point-min))
    (let ((res))
      (while (not (eobp))
        (push (funcall fn) res)
        (forward-line))
      (nreverse res))))

(defun maplines (fn)
  "Map over line texts. FN takes a single argument which is the
line text."
  (maplines-pos
   (lambda ()
     (let ((text (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position))))
       (funcall fn text)))))

(provide 'tog-utils)

;;; tog-utils.el ends here
