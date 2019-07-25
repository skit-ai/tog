;;; tog-io.el --- Source/Sink utilities for tog -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Source/Sink utilities for tog
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
(require 'eieio)
(require 'f)
(require 'json)
(require 's)
(require 'tog-base)

(defclass tog-io-loader ()
  ((current-item :initarg :current-item :initform nil
                 :documentation "Slot holding current item in the loader."))
  :abstract t
  :documentation "General loader interface")

(defclass tog-io-jumpable-loader ()
  ((current-index :initarg :current-index :initform nil :documentation "Index of the current item in the list ITEMS.")
   (items :initarg :items :initform nil :documentation "All the items."))
  :abstract t
  :documentation "A loader with full jump capabilities.")

(cl-defmethod tog-io-goto ((loader tog-io-jumpable-loader) index)
  "Jump the loader to given index."
  (with-slots (current-index current-item items) loader
    (setq current-index (min (- (length items) 1) (max 0 index)))
    (setq current-item (nth current-index items))
    (cond ((= current-index 0) (message "Reached first item"))
          ((= current-index (- (length items) 1)) (message "Reached last item")))))

(cl-defmethod tog-io-next ((loader tog-io-jumpable-loader))
  (tog-io-goto loader (+ (oref loader :current-index) 1)))

(cl-defmethod tog-io-prev ((loader tog-io-jumpable-loader))
  (tog-io-goto loader (- (oref loader :current-index) 1)))

(cl-defmethod tog-io-next-untagged ((loader tog-io-jumpable-loader))
  "Jump the loader to the next untagged item."
  (with-slots (current-index items) loader
    (let ((jump-index (+ current-index 1)))
      (while (and (oref (nth jump-index items) :tag)
                  (< jump-index (length items)))
        (cl-incf jump-index))
      (tog-io-goto loader jump-index))))

(cl-defmethod tog-io-prev-untagged ((loader tog-io-jumpable-loader))
  "Jump the loader to the previous untagged item."
  (with-slots (current-index items) loader
    (let ((jump-index (- current-index 1)))
      (while (and (oref (nth jump-index items) :tag)
                  (>= jump-index 0))
        (cl-decf jump-index))
      (tog-io-goto loader jump-index))))

(cl-defmethod tog-io-last-tagged ((loader tog-io-jumpable-loader))
  "Go to the last tagged item."
  (with-slots (items) loader
    (let ((tagged-index)
          (i 0))
      (while (< i (length items))
        (when (oref (nth i items) :tag)
          (setq tagged-index i))
        (cl-incf i))
      (if tagged-index
          (tog-io-goto loader tagged-index)
        (message "No items tagged")))))

(defclass tog-io-json-loader (tog-io-jumpable-loader tog-io-loader)
  ((source-file :initarg :source-file :documentation "Source file name of json.")
   (tag-file :initarg :tag-file :initform nil :documentation "File name for keeping tags in."))
  :documentation "Simple json loader with sibling tag file.")

(cl-defmethod tog-io-save-tags ((loader tog-io-json-loader))
  "Save tags in sibling .tog file."
  (let ((tag-file (oref loader :tag-file))
        (tags))
    (dolist (it (oref loader :items))
      (when (oref it :tag)
        ;; JSON needs string keys
        (push (cons (number-to-string (oref it :id)) (oref it :tag)) tags)))
    (f-write (json-encode-alist tags) 'utf-8 tag-file)
    (message "Tags saved at %s" tag-file)))

(cl-defmethod tog-io-load-tags ((loader tog-io-json-loader))
  "Load tags from the sibling .tog file"
  (let ((json-array-type 'list)
        (tag-file (oref loader :tag-file)))
    (if (not (f-exists? tag-file))
        (message "Tag file not found. Nothing to load.")
      ;; This can mostly be sped up but it's not really slow at the moment.
      (dolist (tag-data (json-read-file tag-file))
        (dolist (item (oref loader :items))
          (if (= (oref item :id)
                 (string-to-number (symbol-name (car tag-data))))
              (dolist (tag (cdr tag-data))
                (tog-add-tag item tag))))))))

(defun make-tog-io-json-loader (file-path item-factory)
  "Read items from json FILE-PATH, run ITEM-FACTORY to get
taggable items and return a json loader. ITEM-FACTORY takes a
hashtable and return a tog-item instance."
  (let ((json-object-type 'hash-table)
        (json-array-type 'vector)
        (canon-source-file (s-chop-suffix ".gz" file-path)))
    (tog-io-json-loader :items (mapcar item-factory (json-read-file file-path))
                        ;; We initialize to -1 since we always do a first call
                        ;; to next.
                        :current-index -1
                        :source-file file-path
                        :tag-file (concat canon-source-file ".tog"))))

(provide 'tog-io)

;;; tog-io.el ends here
