;;; tog-hl.el --- tog highlighting -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; tog highlighting
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
(require 'ov)

;; Variables defined elsewhere
(defvar tog-types)

(defcustom tog-hl-palette
  '(("#f1c40f" . "#191d24")
    ("#3498db" . "#191d24")
    ("#9b59b6" . "#ffffff")
    ("#34495e" . "#ffffff"))
  "List of color pairs (background . foreground) to highlight entities.
We are not going the full faces way but will be moving there if
needed.")

(defun tog-hl-props-for-type (type)
  "Return extra face related attributes for the tog type to go in
overlays."
  (let* ((idx (or (cl-position type tog-types :test 'equal) 0))
         (colors (nth (% idx (length tog-hl-palette)) tog-hl-palette)))
    `(face (:background ,(car colors) :foreground ,(cdr colors))
           ,@(if type
                 (list 'after-string
                       (propertize (format " %s " type)
                                   'face `(:box (:line-width 1 :color)
                                                :background ,(cdr colors)
                                                :foreground ,(car colors))))))))

(defun tog-hl-mark (type)
  "Highlight the current region using the given type"
  (if (not (region-active-p))
      (message "No region active for highlighting")
    (ov (region-beginning) (region-end) (tog-hl-props-for-type type))))

(defun tog-hl-unmark (arg)
  "Remove mark from current position. If arg is not nil, clear
the complete line."
  (interactive "P")
  (let ((ovs (if arg (ov-in (line-beginning-position) (line-end-position)) (list (ov-at)))))
    (dolist (o ovs)
      (delete-overlay o))))

(provide 'tog-hl)

;;; tog-hl.el ends here
