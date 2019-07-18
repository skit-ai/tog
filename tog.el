;;; tog.el --- NLP data tagging utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.3.1
;; Package-Requires: ((emacs "26") (ov "1.0.6") (s "1.12.0") (f "0.20.0") (dash "2.15.0") (helm "3.2"))
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
(require 's)
(require 'tog-base)
(require 'tog-io)
(require 'tog-progress)
(require 'tog-timer)
(require 'tog-utils)

(defcustom tog-buffer-name "*tog*"
  "Name of the tagging buffer. This is the shared buffer for all
kind of tagging. Note that only one kind of tagging can happen at
a given time.")

(defcustom tog-nav-hook nil
  "Hook for any navigation event.")

(defcustom tog-annotate-hook nil
  "Hook after a tag is applied.")

(defvar tog-loader nil
  "Data loader for current task")

(defmacro deftog-nav (name lambda-list docstring &rest body)
  "Macro for creating interactive navigation command. This makes
sure that the current item is re-rendered and navigation hooks
are called."
  (declare (indent defun))
  `(defun ,name ,lambda-list
     ,docstring
     (interactive)
     ,@body
     (tog-show (oref tog-loader :current-item))
     (run-hooks 'tog-nav-hook)))

(deftog-nav tog-goto (idx)
  "Jump to idx item for tagging. Boundary handling is done in
this function so the caller need not worry about anything other
+/-."
  (tog-io-goto tog-loader idx))

(deftog-nav tog-next ()
  "Go to next item for tagging."
  (tog-io-next tog-loader))

(deftog-nav tog-prev ()
  "Go to previous item for tagging."
  (tog-io-prev tog-loader))

(deftog-nav tog-next-untagged ()
  "Go to next item which is untagged, ignoring current one."
  (tog-io-next-untagged tog-loader))

(deftog-nav tog-prev-untagged ()
  "Go to previous item which is untagged, ignoring current one."
  (tog-io-prev-untagged tog-loader))

(deftog-nav tog-last-tagged ()
  "Go to the last tagged item"
  (tog-io-last-tagged tog-loader))

(defun tog-save-tags ()
  (interactive)
  (tog-io-save-tags tog-loader))

(defun tog-load-tags ()
  (interactive)
  (tog-io-load-tags tog-loader))

;;;###autoload
(define-derived-mode tog-mode org-mode "tog"
  "Major mode for togging."
  :after-hook (setq header-line-format '(:eval (tog-progress-build-header)))
  (read-only-mode))

;;;###autoload
(defun tog ()
  "Start tagging."
  (interactive)
  (if (null tog-loader)
      (message "tog-loader is null. Try creating a loader.")
    (tog-timer-start)
    (tog-next)))

(defun tog-tag ()
  "Command to initiate tag collection for current tog-item."
  (interactive)
  (with-current-tog-item
   (tog-annotate current-item)
   (tog-show current-item)))

(defun tog-clear ()
  "Command to clear tag for current item."
  (interactive)
  (with-current-tog-item
   (tog-clear-tag current-item)
   (tog-show current-item)))

;;;###autoload
(defun tog-quit ()
  "Exit and cleanup. We don't save before exiting by default
since this might do bad things to tag file because of certain
screw ups while tagging."
  (interactive)
  (when (get-buffer tog-buffer-name)
    (kill-buffer tog-buffer-name))
  (setq tog-loader nil)
  (tog-timer-reset))

(provide 'tog)

;;; tog.el ends here
