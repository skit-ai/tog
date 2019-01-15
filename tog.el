;;; tog.el --- NLP data tagging utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
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
(require 'ov)
(require 's)
(require 'tog-utils)
(require 'tog-parse)

(defvar tog-highlight-patterns
  '(("^[0-9]+" . font-lock-constant-face)
    ("^#.*" . font-lock-comment-face))
  "Highlight faces for the buffer")

(defface tog-highlight
  '((t :background "yellow"
       :foreground "black"))
  "Face for annotated entity")

(defcustom tog-tag-update-hook nil
  "Stuff that runs after each tag update."
  :type 'hook)

(defun tog-tag-file-name ()
  "Return the annotation file name for current buffer."
  (s-replace-regexp "tog$" "tog.tag" (buffer-file-name)))

(defun tog-read-tags ()
  "Read tags for current tog file."
  (with-current-buffer (find-file-noselect (tog-tag-file-name))
    (let ((h (make-hash-table)))
      (maplines-pos
       (lambda ()
         (let ((tag (tog-parse-tag)))
           (if tag (puthash (car tag) (cons (cdr tag) (gethash (car tag) h nil)) h)))))
      h)))

(defun tog-region-valid-p ()
  "Tell if the region lies in the valid zone."
  (if (region-active-p)
      (let ((tbol (tog-parse-line-start-pos)))
        (if tbol
            (<= tbol (region-beginning) (region-end) (line-end-position))))))

;;;###autoload
(defun tog-tag ()
  "Annotate the currently selected region."
  (interactive)
  (let* ((tbol (tog-parse-line-start-pos))
         (bound (cond ((tog-region-valid-p) (cons (region-beginning) (region-end)))
                      ((not (region-active-p)) (cons tbol (line-end-position)))
                      (t nil))))
    (if bound
        (let* ((line-id (tog-parse-line-id)))
          (when line-id
            (ov (car bound) (cdr bound)
                'face 'tog-highlight
                'tog-line-id line-id
                'tog-start (- (car bound) tbol)
                'tog-end (- (cdr bound) tbol))
            (deactivate-mark)
            (run-hook-with-args 'tog-tag-update-hook)))
      (message "Invalid region selected"))))

;;;###autoload
(defun tog-untag (arg)
  (interactive "P")
  (let ((ovs (if arg (ov-in (line-beginning-position) (line-end-position)) (list (ov-at)))))
    (dolist (o ovs)
      (delete-overlay o))
    (run-hook-with-args 'tog-tag-update-hook)))

(defun tog-format-ov (o)
  (format "%s: %s,%s" (ov-val o 'tog-line-id) (ov-val o 'tog-start) (ov-val o 'tog-end)))

(defun tog-load-tags ()
  "Load tags for the current file."
  (let ((tag-table (tog-read-tags)))
    (when tag-table
      (maplines-pos
       (lambda ()
         (let ((line-id (tog-parse-line-id))
               (line-start (tog-parse-line-start-pos)))
           (dolist (td (gethash line-id tag-table nil))
             (ov (+ line-start (car td)) (+ line-start (cdr td))
                 'face 'tog-highlight
                 'tog-line-id line-id
                 'tog-start (car td)
                 'tog-end (cdr td)))))))))

;;;###autoload
(defun tog-save-tags ()
  "Save all the annotations for current buffer."
  (interactive)
  (let ((ovs (ov-all)))
    (with-current-buffer (find-file-noselect (tog-tag-file-name))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (dolist (o ovs)
        (insert (tog-format-ov o) "\n"))
      (save-buffer))))

;;;###autoload
(define-derived-mode tog-mode text-mode "tog"
  "Major mode for working with tog files"
  (setq font-lock-defaults '(tog-highlight-patterns))
  (tog-load-tags)
  (read-only-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tog\\'" . tog-mode))

(provide 'tog)

;;; tog.el ends here
