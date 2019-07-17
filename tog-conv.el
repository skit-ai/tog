;;; tog-conv.el --- Tagging for conversations with alternatives -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Tagging for conversations with alternatives
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
(require 'json)
(require 'f)
(require 'ov)
(require 's)
(require 'dash)
(require 'tog-base)
(require 'tog-hl)
(require 'tog-utils)
(require 'tog-parse)
(require 'tog-player)
(require 'tog-input)


(defcustom tog-conv-timezone "Asia/Kolkata"
  "Timezone to enforce on displayed items")

(defcustom tog-conv-method 'ranged
  "Method of tagging to use. Possible values are:

- `ranged': Ask for type and a range to highlight for that range.
  Note that we only allow one range for a single type. This
  should not be a problem since we can just rename the types to
  for disambiguation. Additional fields include `text',
  `alt-index' and `text-range'. If `text' is not provided but a
  tag for a type is present, we assume this particular type is
  not present in the tagged item.

- `transcript': Ask for a written transcript for the
  conversation. Fields are `text' and (optionally) `alt-index'.

- `boolean': Ask if something is there or not. No extra metadata
  is present other than `value' which keeps the boolean value (t,
  nil). If multiple types are present, this can be used for
  things like intent tagging.")

(defcustom tog-conv-prefill-prompt nil
  "Tell whether to fill marked region in tag prompt while tagging")

(defclass tog-conv (tog-item)
  ((alternatives :initarg :alternatives)
   (reftime :initarg :reftime)
   (audio-url :initarg :audio-url)
   (state :initarg :state)
   (metadata :initarg :metadata))
  "A conversation (turn) for a call.")

(defun tog-conv-tag-same-type (ta tb)
  "Tell if two tag objects are equal in type."
  (string= (alist-get 'type ta) (alist-get 'type tb)))

(cl-defmethod update-tag ((obj tog-conv) tag)
  "Apply tag to a conversation. This overrides an already present
tag of same type."
  (oset obj :tag (upsert tag (lambda (it) (tog-conv-tag-same-type tag it)) (oref obj :tag))))

(defun tog-conv-clear ()
  "Clear current conversation."
  (interactive)
  (let ((item (nth tog-index tog-items)))
    (clear-tags item)
    (tog-show item)))

(defun make-conv (it)
  "Use parsed json hash from db to create a conversation."
  (tog-conv :alternatives (gethash "alternatives" it)
            :id (gethash "conversation_id" it)
            :reftime (gethash "reftime" it)
            :audio-url (gethash "audio_url" it)
            :state (s-replace-all '(("_" . "-")) (gethash "state" it))
            :metadata `((call-id . ,(gethash "call_id" it)))))

(defun tog-conv-load-from-json (file-path)
  "Read conversations dictionaries from json."
  ;; TODO: Check whether there is performance gain if we switch to the C
  ;;       json-parse-string function.
  (let ((json-object-type 'hash-table)
        (json-array-type 'vector))
    (setq tog-items (mapcar #'make-conv (json-read-file file-path))
          tog-source-file file-path)))

(defun tog-conv-play ()
  "Command for playing current item."
  (interactive)
  (let ((url (oref (nth tog-index tog-items) :audio-url)))
    (if (not url)
        (message "No audio found for this conversation")
      (tog-player-play url))))

(cl-defmethod call-url ((obj tog-conv))
  "Return metabase call url."
  (format "https://metabase.vernacular.ai/question/38?call_id=%s" (alist-get 'call-id (oref obj :metadata))))

(cl-defmethod texts ((obj tog-conv))
  "Return a list of texts in the alternatives.

NOTE: We don't merge multiple broken utterances. The assumption
is that the data generator takes care of this and gives us merged
text in :alternatives. This does not affect the value of
:alternatives which might be present in :prediction."
  (mapcar (lambda (it) (gethash "transcript" it)) (aref (oref obj :alternatives) 0)))

(cl-defmethod ranged-alt-tags ((obj tog-conv) &optional alt-index)
  "Return ranged tags present on obj."
  (if (null alt-index)
      (-filter (lambda (t) (alist-get 'text-range t)) (oref obj :tag))
    (-filter (lambda (t)
               (--if-let (alist-get 'alt-index t)
                   (and (= alt-index it) (alist-get 'text-range t))))
             (oref obj :tag))))

(cl-defmethod tog-show-ranged-tags ((obj tog-conv))
  "Display alternative texts and ranged tags for current buffer."
  (let ((i 0))
    (dolist (text (texts obj))
      (insert (number-to-string i) ". " text)
      ;; Highlight tags that are applied in this conversation
      (save-excursion
        (dolist (tag (ranged-alt-tags obj i))
          (goto-char (line-beginning-position))
          (re-search-forward "^[0-9]+\. ")
          (let ((range (alist-get 'text-range tag)))
            (goto-char (+ (point) (car range)))
            (set-mark-command nil)
            (goto-char (+ (point) (- (cadr range) (car range))))
            (setq deactivate-mark nil))
          (tog-hl-mark (alist-get 'type tag))))
      (insert "\n")
      (cl-incf i))))

(cl-defmethod tog-show-all-tags ((obj tog-conv))
  "Display all tags for current buffer item"
  (dolist (tag (oref obj :tag))
    (insert "# - " (format "%s" tag) "\n")))

(cl-defmethod tog-show ((obj tog-conv))
  "Display a tog conv item in buffer for tagging."
  (let ((buffer (get-buffer-create tog-buffer-name))
        (local-iso-time (format-time-string "%FT%T%z" (date-to-time (oref obj :reftime)) tog-conv-timezone)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (dolist (o (ov-all)) (delete-overlay o))
        (delete-region (point-min) (point-max))
        (tog-mode)
        (insert "* item " (number-to-string (oref obj :id)) "\n")
        (org-set-property "REFTIME" local-iso-time)
        (org-set-property "CALL-URL" (call-url obj))
        (org-set-property "STATE" (oref obj :state))
        (insert "\n")

        (tog-show-ranged-tags obj)
        (when (oref obj :tag)
          (insert "\n# All tags:\n")
          (tog-show-all-tags obj)
          (org-todo "DONE"))

        (goto-char (point-min))
        ;; This is only for putting cursor at a comfortable position
        (re-search-forward "^0\. ")))
    (switch-to-buffer buffer)))

(defun tog-conv-make-tag-ranged (tag-type)
  "Make a ranged type tag based on current pointer position and
other things."
  (let* ((current-text (region-text))
         (input-text (tog-input-string tag-type))
         (text-same-p (string= input-text current-text)))
    `((type . ,tag-type)
      (text . ,input-text)
      (alt-index . ,(and (region-active-p) text-same-p (tog-parse-line-id)))
      (text-range . ,(and text-same-p (tog-parse-text-range))))))

(defun tog-conv-make-tag-boolean (tag-type)
  "Make a boolean (t, nil) tag."
  `((type . ,tag-type)
    (value . ,(y-or-n-p (format "%s? " tag-type)))))

(defun tog-conv-make-tag-transcript (tag-type)
  "Make a transcription based tag. If present on an alternative,
we take that text as the default."
  (let* ((current-text (tog-parse-line))
         (input-text (tog-input-string tag-type (or current-text ""))))
    `((type . ,tag-type)
      (text . ,input-text)
      (alt-index . ,(and (string= input-text current-text) (tog-parse-line-id))))))

;;;###autoload
(defun tog-conv-tag ()
  "Annotate current conversation."
  (interactive)
  (let* ((prefill-text (when (and tog-conv-prefill-prompt (region-active-p))
                         (region-text)))
         (tag-type (if (= 1 (length tog-types))
                       (car tog-types)
                     (tog-input-choice tog-types "Type: " prefill-text))))
    (when tag-type
      (let ((tag  (cl-ecase tog-conv-method
                    (transcript (tog-conv-make-tag-transcript tag-type))
                    (ranged (tog-conv-make-tag-ranged tag-type))
                    (boolean (tog-conv-make-tag-boolean tag-type)))))
        (update-tag (nth tog-index tog-items) tag)
        (tog-show (nth tog-index tog-items))))))

(provide 'tog-conv)

;;; tog-conv.el ends here
