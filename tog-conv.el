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
(require 'tog-parse)
(require 'tog-input)


(defvar tog-conv-player-proc nil
  "Process related to the audio player.")

(defclass tog-conv ()
  ((alternatives :initarg :alternatives)
   (id :initarg :id)
   (reftime :initarg :reftime)
   (audio-url :initarg :audio-url)
   (state :initarg :state)
   (metadata :initarg :metadata)
   (tag :initarg :tag :initform nil))
  "A conversation (turn) for a call.")

(defun tog-conv-tag-same-type (ta tb)
  "Tell if two tag objects are equal in type."
  (string= (alist-get 'type ta) (alist-get 'type tb)))

(cl-defmethod update-tag ((obj tog-conv) tag)
  "Apply tag to a conversation. This overrides an already present
tag of same type."
  (oset obj :tag (upsert tag (lambda (it) (tog-conv-tag-same-type tag it)) (oref obj :tag))))

(cl-defmethod clear-tags ((obj tog-conv))
  "Remove all tags from the item."
  (oset obj :tag nil))

(defun tog-conv-clear ()
  "Clear current conversation."
  (interactive)
  (clear-tags (nth tog-index tog-items))
  (read-only-mode -1)
  (org-todo "")
  (read-only-mode)
  (dolist (o (ov-all)) (delete-overlay o)))

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

(cl-defmethod ranged-alt-tags ((obj tog-conv) alt-index)
  "Return ranged tags present for the alternative index."
  (-filter (lambda (t) (--if-let (alist-get 'alt-index t) (and (= alt-index it) (alist-get 'text-range t))))
           (oref obj :tag)))

(cl-defmethod tog-show ((obj tog-conv))
  "Display a tog conv item in buffer for tagging."
  (let ((buffer (get-buffer-create tog-buffer-name)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (dolist (o (ov-all)) (delete-overlay o))
      (delete-region (point-min) (point-max))
      (tog-mode)
      (insert "* item " (number-to-string (oref obj :id)) "\n")
      (org-set-property "REFTIME" (oref obj :reftime))
      (org-set-property "CALL-URL" (call-url obj))
      (org-set-property "STATE" (oref obj :state))
      (insert "\n")

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
          (incf i)))

      (if (oref obj :tag)
          (org-todo "DONE"))

      (read-only-mode)
      (goto-char (point-min))
      ;; This is only for putting cursor at a comfortable position
      (re-search-forward "^0\. "))
    (switch-to-buffer buffer)))

;;;###autoload
(defun tog-conv-tag ()
  "Annotate current conversation."
  (interactive)
  (let* ((tag-type (tog-input-choice tog-types "Type: "))
         (tag `((type . ,tag-type)
                (text . ,(tog-input-string tag-type))
                (alt-index . ,(and (region-active-p) (tog-parse-line-id)))
                (text-range . ,(tog-parse-text-range)))))
    (update-tag (nth tog-index tog-items) tag)

    ;; Highlight for feedback
    (when (region-active-p)
      (tog-hl-mark tag-type)
      (deactivate-mark))

    (read-only-mode -1)
    (org-todo "DONE")
    (read-only-mode)))

(provide 'tog-conv)

;;; tog-conv.el ends here
