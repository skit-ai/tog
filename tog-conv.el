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
(require 'helm)
(require 'json)
(require 'f)
(require 'ov)
(require 's)
(require 'dash)
(require 'tog-hl)
(require 'tog-utils)

(defvar tog-conv-player-proc nil
  "Process related to the audio player.")

(defclass tog-conv ()
  ((alternatives :initarg :alternatives)
   (id :initarg :id)
   (reftime :initarg :reftime)
   (audio-url :initarg :audio-url)
   (state :initarg :state)
   (metadata :initarg :metadata)
   (tag :initarg :tag))
  "A conversation (turn) for a call.")

(defun tog-conv-tag-same-type (ta tb)
  "Tell if two tag objects are equal in type."
  (string= (alist-get 'tag-type ta)
           (alist-get 'tag-type tb)))

(cl-defmethod apply-tag ((obj tog-conv) tag)
  "Apply tag to a conversation. This overrides an already present
tag of same type."
  (let ((current-tags (if (slot-boundp obj :tag) (oref obj :tag))))
    (let ((override-idx (-find-index (lambda (t) (tog-conv-tag-same-type t tag)) current-tags)))
      (if (null override-idx)
          (oset obj :tag (cons tag current-tags))
        (oset obj :tag (-replace-at override-idx tag current-tags))))))

(cl-defmethod clear-tags ((obj tog-conv))
  "Remove all tags from the item."
  (oset obj :tag nil))

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
  (setq tog-items (mapcar #'make-conv (json-parse-string (f-read file-path)))
        tog-source-file file-path))

(cl-defmethod play ((obj tog-conv))
  "Stop the current playback and play this conversation."
  (let ((program "mplayer")
        (url (oref obj :audio-url)))
    (if (not url)
        (message "No audio found for this conversation")
      (if tog-conv-player-proc (delete-process tog-conv-player-proc))
      (setq tog-conv-player-proc (start-process "tog-player" nil program url)))))

(defun tog-conv-play ()
  "Command for playing current item."
  (interactive)
  (play (nth tog-index tog-items)))

(cl-defmethod call-url ((obj tog-conv))
  "Return metabase call url."
  (format "https://metabase.vernacular.ai/question/38?call_id=%s" (alist-get 'call-id (oref obj :metadata))))

(cl-defmethod texts ((obj tog-conv))
  "Return a list of texts in the alternatives.
NOTE: We don't merge multiple broken utterances."
  (mapcar (lambda (it) (gethash "transcript" it)) (aref (oref obj :alternatives) 0)))

(cl-defmethod tog-show ((obj tog-conv))
  "Display a tog conv item in buffer for tagging."
  (let ((buffer (get-buffer-create tog-buffer-name)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (tog-mode)
      (insert "* item " (number-to-string (oref obj :id)) "\n")
      (org-set-property "REFTIME" (oref obj :reftime))
      (org-set-property "CALL-URL" (call-url obj))
      (org-set-property "STATE" (oref obj :state))
      (insert "\n")

      (let ((i 0))
        (dolist (text (texts obj))
          (insert (number-to-string i) ". " text "\n")
          (incf i)))

      (if (slot-boundp obj :tag)
          (org-todo "DONE"))

      (read-only-mode)
      (goto-char (point-min))
      ;; This is only for putting cursor at a comfortable position
      (re-search-forward "^0\. "))
    (switch-to-buffer buffer)))

(defun tog-conv-parse-alt-index ()
  "Return alternative number for current line."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward "^\\([[:digit:]]\\)\. " (line-end-position) t)
      (string-to-number (match-string-no-properties 1)))))

(defun tog-conv-parse-text-range ()
  "Return text range for marked alternative."
  (when (region-active-p)
    (let ((bounds (car (region-bounds)))
          (line-start (line-beginning-position)))
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward "^\\([[:digit:]]\. +\\)" (line-end-position) t)
          (let ((offset (+ line-start (length (match-string-no-properties 1)))))
            (list (- (car bounds) offset) (- (cdr bounds) offset))))))))

(defun tog-conv-make-tag (tag-type)
  "Create tag from current selection and input."
  (let* ((user-entry (read-string (format "%s> " tag-type) (region-text))))
    (apply-tag (tog-alts-current-conv)
               `((type . ,tag-type)
                 (text . ,(if (string= user-entry "") nil user-entry))
                 (alt-index . ,(tog-conv-parse-alt-index))
                 (text-range . ,(tog-conv-parse-text-range))))

    ;; Highlight for feedback
    (when (region-active-p)
      (tog-hl-mark tag-type)
      (deactivate-mark))))

(defun tog-conv-tag ()
  "Annotate current conversation."
  (interactive)
  (helm :sources (helm-build-sync-source "types"
                   :candidates tog-types
                   :action '(("annotate" . tog-conv-make-tag)))
        :buffer "*helm tog-conv tag*"
        :prompt "Type: "))

(provide 'tog-conv)

;;; tog-conv.el ends here
