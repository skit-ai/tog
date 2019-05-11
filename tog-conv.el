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
(require 'tog-hl)
(require 'tog-utils)
(require 'tog-parse)
(require 'tog-player)
(require 'tog-input)


(defcustom tog-conv-after-tag-hook nil
  "Hook after a tag is applied.")

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
  (setq tog-items (mapcar #'make-conv (json-parse-string (f-read file-path)))
        tog-source-file file-path))

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
TODO: We don't merge multiple broken utterances."
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
      (incf i))))

(cl-defmethod tog-show-all-tags ((obj tog-conv))
  "Display all tags for current buffer item"
  (dolist (tag (oref obj :tag))
    (insert "# - " (format "%s" tag) "\n")))

(cl-defmethod tog-show ((obj tog-conv))
  "Display a tog conv item in buffer for tagging."
  (let ((buffer (get-buffer-create tog-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (dolist (o (ov-all)) (delete-overlay o))
        (delete-region (point-min) (point-max))
        (tog-mode)
        (insert "* item " (number-to-string (oref obj :id)) "\n")
        (org-set-property "REFTIME" (oref obj :reftime))
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

;;;###autoload
(defun tog-conv-tag ()
  "Annotate current conversation."
  (interactive)
  (let ((tag-type (if (= 1 (length tog-types))
                      (car tog-types)
                    (tog-input-choice tog-types "Type: "))))
    (when tag-type
      (let ((tag  (ecase tog-method
                    (ranged `((type . ,tag-type)
                              (text . ,(tog-input-string tag-type))
                              (alt-index . ,(and (region-active-p) (tog-parse-line-id)))
                              (text-range . ,(tog-parse-text-range))))
                    (boolean `((type . ,tag-type)
                               (value . ,(y-or-n-p (format "%s? " tag-type))))))))
        (update-tag (nth tog-index tog-items) tag)
        ;; TODO: This redraw gives a jittery experience but we will see later if
        ;;       we need to fix that
        (tog-show (nth tog-index tog-items))
        (run-hooks 'tog-conv-after-tag-hook)))))

(provide 'tog-conv)

;;; tog-conv.el ends here
