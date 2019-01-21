;;; tog.el --- NLP data tagging utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.2
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
  '(("^[0-9,a-z,A-Z]+" . font-lock-constant-face)
    ("^#.*" . font-lock-comment-face))
  "Highlight faces for the buffer")

(defcustom tog-palette
  '(("#f1c40f" . "#191d24")
    ("#3498db" . "#191d24")
    ("#9b59b6" . "#ffffff")
    ("#34495e" . "#ffffff"))
  "List of color pairs (background . foreground) to highlight entities.
We are not going the full faces way but will be moving there if
needed.")

(defvar-local tog-types nil
  "Types like LOC, NAME etc. allowed in a buffer.
The usual way to set this is to declare them in the tog file
using the following:
# tog-types: LOC TIME NAME")

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

(defun tog-buffer-to-line-region (region-pair)
  "Buffer region definition to line one. Assume the buffer region
  is a valid one."
  (save-excursion
    (goto-char (car region-pair))
    (let ((tbol (tog-parse-line-start-pos)))
      (cons (- (car region-pair) tbol)
            (- (cdr region-pair) tbol)))))

(defun tog-line-to-buffer-region (region-pair)
  "Line offset to buffer. Assume we are in that line."
  (let ((tbol (tog-parse-line-start-pos)))
    (cons (+ tbol (car region-pair))
          (+ tbol (cdr region-pair)))))

(defun tog-region-valid-p ()
  "Tell if the region lies in the valid zone."
  (if (region-active-p)
      (let ((tbol (tog-parse-line-start-pos)))
        (if tbol
            (<= tbol (region-beginning) (region-end) (line-end-position))))))

(defun tog-ov-props-for-type (type)
  "Return extra face related attributes for the tog type to go in
overlays."
  (let* ((idx (or (cl-position type tog-types :test 'equal) 0))
         (colors (nth (% idx (length tog-palette)) tog-palette)))
    `(face (:background ,(car colors) :foreground ,(cdr colors))
           ,@(if type
                 (list 'after-string
                       (propertize (format " %s " type)
                                   'face `(:box (:line-width 1 :color)
                                                :background ,(cdr colors)
                                                :foreground ,(car colors))))))))

(defun tog-make-ov (region-bound line-bound line-id type)
  "Create an overlay using the given specifications."
  (apply #'ov (car region-bound) (cdr region-bound)
         'tog-start (car line-bound)
         'tog-end (cdr line-bound)
         'tog-line-id line-id
         'tog-type type
         (tog-ov-props-for-type type)))

;;;###autoload
(defun tog-tag (type-index)
  "Annotate the currently selected region."
  (interactive (list 0))
  (let* ((tbol (tog-parse-line-start-pos))
         (bound (cond ((tog-region-valid-p) (cons (region-beginning) (region-end)))
                      ((not (region-active-p)) (cons tbol (line-end-position)))
                      (t nil))))
    (if bound
        (let ((line-id (tog-parse-line-id))
              (type (nth type-index tog-types)))
          (when line-id
            (tog-make-ov bound (tog-buffer-to-line-region bound) line-id type)
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

(defun tog-load-tags ()
  "Load tags for the current file."
  (let ((tag-table (tog-read-tags)))
    (when tag-table
      (maplines-pos
       (lambda ()
         (let ((line-id (tog-parse-line-id)))
           (dolist (tag-info (gethash line-id tag-table nil))
             (let ((line-bound (car tag-info))
                   (type (cadr tag-info)))
               (tog-make-ov (tog-line-to-buffer-region line-bound) line-bound line-id type)))))))))

;;;###autoload
(defun tog-save-tags ()
  "Save all the annotations for current buffer."
  (interactive)
  (let ((ovs (ov-all)))
    (with-current-buffer (find-file-noselect (tog-tag-file-name))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (dolist (o ovs)
        (if (ov-val o 'tog-line-id)
            (insert (tog-format-ov o) "\n")))
      (save-buffer))))

;;;###autoload
(define-derived-mode tog-mode text-mode "tog"
  "Major mode for working with tog files"
  (setq font-lock-defaults '(tog-highlight-patterns))
  (setq-local tog-types (tog-parse-buffer-types))
  (tog-load-tags)
  (read-only-mode))

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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tog\\'" . tog-mode))

(provide 'tog)

;;; tog.el ends here
