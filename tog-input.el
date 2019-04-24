;;; tog-input.el --- Input utilities for tog -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Input utilities for tog
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

(require 'helm)
(require 'tog-utils)

(defun tog-input-string (name &optional default)
  "Ask for input string from user."
  (let ((entry (read-string (format "%s> " name) (or default (region-text)))))
    (if (string= entry "") nil entry)))

(defun tog-input-choice (options &optional prompt)
  "Return one from given"
  (helm :sources (helm-build-sync-source "tog-input-choice"
                   :candidates options)
        :buffer "*helm tog input choice*"
        :prompt (or prompt "Choice: ")))

(provide 'tog-input)

;;; tog-input.el ends here
