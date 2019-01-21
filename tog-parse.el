;;; tog-parse.el --- Parsing fns for tog -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Parsing fns for tog
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

(defun tog-parse-line-id ()
  "Return identifier for the current line."
  (save-excursion
    (goto-char (line-beginning-position))
    (if (re-search-forward "^\\([0-9,a-z,A-Z]+\\): " (line-end-position) t)
        (match-string-no-properties 1))))

(defun tog-parse-line-start-pos ()
  "Return starting position of text data (relative to buffer) for current line."
  (save-excursion
    (goto-char (line-beginning-position))
    (if (re-search-forward "^\\([0-9,a-z,A-Z]+: \\)" (line-end-position) t)
        (+ (line-beginning-position) (length (match-string-no-properties 1))))))

(defun tog-parse-buffer-types ()
  "Parse tog-types from the buffer"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^# tog-types:\\(.*\\)" nil t)
    (let ((str (match-string-no-properties 1)))
      (when str
        (s-split " " (s-trim str))))))

(defun tog-parse-tag ()
  "Return tag information from current line."
  (save-excursion
    (goto-char (line-beginning-position))
    (if (re-search-forward "^\\([0-9,a-z,A-Z]+\\): \\([0-9]+\\),\\([0-9]+\\),\\(.*\\)" (line-end-position) t)
        (list (match-string-no-properties 1)
              (cons (string-to-number (match-string-no-properties 2))
                    (string-to-number (match-string-no-properties 3)))
              (match-string-no-properties 4)))))

(provide 'tog-parse)

;;; tog-parse.el ends here
