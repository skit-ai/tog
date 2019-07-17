;;; tog-base.el --- Base classes for tog -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Base classes for tog
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

(require 'eieio)

;; Forward declarations
(defvar tog-annotate-hook)
(defvar tog-buffer-name)

(defclass tog-item ()
  ((id :initarg :id)
   (tag :initarg :tag :initform nil))
  :abstract t
  :documentation "Base class specifying an item to present while tagging.")

(cl-defmethod tog-show :before ((obj tog-item))
  "Before method which cleans up the buffer"
  (let ((buffer (get-buffer-create tog-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (dolist (o (ov-all)) (delete-overlay o))
        (erase-buffer)
        (tog-mode)))))

(cl-defmethod tog-show :after ((obj tog-item))
  (switch-to-buffer (get-buffer-create tog-buffer-name)))

(cl-defgeneric tog-add-tag ((obj tog-item) tag)
  "Add given tag to the object")

(cl-defmethod tog-clear-tag ((obj tog-item))
  (oset obj :tag nil))

(cl-defmethod tog-annotate :after ((obj tog-item))
  "After method for running tag-hook"
  (run-hooks 'tog-annotate-hook))

(provide 'tog-base)

;;; tog-base.el ends here
