;;; tog-stats.el --- Stats calculator for files -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Stats calculator for files
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

(require 'f)
(require 's)


(defun tog-stats-jq (&rest args)
  "Run jq with given arguments."
  (with-temp-buffer
    (apply #'call-process "jq" nil t nil args)
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun tog-stats-count (input-file)
  "Count items present and items done for given input json."
  (let ((tog-file (concat input-file ".tog")))
    (cons (string-to-number (tog-stats-jq ". | length" input-file))
          (if (f-exists? tog-file)
              (string-to-number (tog-stats-jq ". | length" tog-file))
            0))))

(provide 'tog-stats)

;;; tog-stats.el ends here
