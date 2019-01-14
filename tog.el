;;; tog.el --- NLP data tagging utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (ov "1.0.6") (s "1.12.0"))
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

(require 'ov)
(require 's)

(defvar tog-highlight-patterns
  '(("^[0-9]+" . font-lock-constant-face)
    ("^#.*" . font-lock-comment-face))
  "Highlight faces for the buffer")

(defface tog-highlight
  '((t :background "yellow"
       :foreground "black"))
  "Face for annotated entity")

(defun tog-tag-file-name ()
  "Return the annotation file name for current buffer."
  (s-replace-regexp "tog$" "tag.tog" (buffer-file-name)))

(defun tog-line-id ()
  "Return identifier for the current line."
  (save-excursion
    (goto-char (line-beginning-position))
    (if (re-search-forward "^\\([0-9]+\\)" (line-end-position) t)
        (string-to-number (match-string-no-properties 1)))))

(defun tog-line-start-pos ()
  "Return starting position of text data (relative to buffer) for that line."
  (+ (line-beginning-position)
     (length (number-to-string (tog-line-id))) 1))

;;;###autoload
(defun tog-tag ()
  "Annotate the currently selected region."
  (interactive)
  (let* ((line-id (tog-line-id))
         (line-start (if line-id (tog-line-start-pos))))
    (if line-id
        (ov (region-beginning) (region-end)
            'face 'tog-highlight
            'tog-line-id line-id
            'tog-start (- (region-beginning) line-start)
            'tog-end (- (region-end) line-start)))))

(defun tog-format-ov (o)
  (format "%s: %s, %s" (ov-val o 'tog-line-id) (ov-val o 'tog-start) (ov-val o 'tog-end)))

(defun tog-save-tags ()
  "Save all the annotations for current buffer."
  (interactive)
  (let ((ovs (ov-all)))
    (with-temp-file (tog-tag-file-name)
      (goto-char (point-min))
      (dolist (o ovs)
        (insert (tog-format-ov o) "\n")))))

;;;###autoload
(define-derived-mode tog-mode text-mode "tog"
  "Major mode for working with tog files"
  (setq font-lock-defaults '(tog-highlight-patterns))
  (tog-loag-tags))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tog\\'" . tog-mode))

(provide 'tog)

;;; tog.el ends here
