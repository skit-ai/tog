;;; tog-parse.el --- Parsing functions for tog -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Parsing functions for tog
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

(defun tog-parse-line-id (&optional pattern)
  "Return line id in integer. The default assumption is that the
text goes like:

1. text here"
  (let ((pattern (or pattern "^\\([[:digit:]]\\)\. "))))
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward pattern (line-end-position) t)
      (string-to-number (match-string-no-properties 1)))))

(defun tog-parse-text-range (&optional pattern)
  "Return marked range relative to the text. PATTERN identifies
the parts to ignore. If defaults to the usual assumption about
having an integer with a full stop at the end."
  (when (region-active-p)
    (let ((bounds (car (region-bounds)))
          (line-start (line-beginning-position))
          (pattern (or pattern "^[[:digit:]]\. ")))
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward pattern (line-end-position) t)
          (let ((offset (+ line-start (length (match-string-no-properties 0)))))
            (cons (- (car bounds) offset)
                  (- (cdr bounds) offset))))))))


(provide 'tog-parse)

;;; tog-parse.el ends here
