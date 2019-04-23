;;; tog-alts.el --- Tagging for alternatives with other metadata -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Tagging for alternatives with other metadata
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

(defcustom tog-alts-buffer-name "*tog-alts*"
  "Name of the tagging buffer")

(defcustom tog-alts-types '("people" "number" "date" "time" "datetime")
  "Types to tag")

(defvar tog-alts-conversation-id nil
  "Variable for identifying currently displayed item")

(defvar tog-alts-player-proc nil
  "Process related to the audio player.")

(defvar tog-alts-conversations nil
  "Variable for keeping conversations.")

(defclass tog-conversation ()
  ((alternatives :initarg :alternatives)
   (id :initarg :id)
   (reftime :initarg :reftime)
   (audio-url :initarg :audio-url)
   (state :initarg :state)
   (metadata :initarg :metadata)
   (tag :initarg :tag)))

(defun make-tog-conversation (it)
  "Use parsed json hash from db to create a conversation."
  (tog-conversation :alternatives (gethash "alternatives" it)
                    :id (gethash "conversation_id" it)
                    :reftime (gethash "reftime" it)
                    :audio-url (gethash "audio_url" it)
                    :state (gethash "state" it)
                    :metadata `((call-id . ,(gethash "call_id" it)))))

(cl-defmethod play ((obj tog-conversation))
  "Stop the current playback and play this conversation."
  (let ((program "mplayer")
        (url (oref obj :audio-url)))
    (if (not url)
        (message "no audio found for this conversation")
      (if tog-alts-player-proc (delete-process tog-alts-player-proc))
      (setq tog-alts-player-proc (start-process "tog-player" nil program url)))))

(cl-defmethod call-url ((obj tog-conversation))
  "Return metabase call url."
  (format "https://metabase.vernacular.ai/question/38?call_id=%s" (alist-get 'call-id (oref obj :metadata))))

(cl-defmethod conversation-texts ((obj tog-conversation))
  "Return a list of texts in the alternatives."
  (mapcar (lambda (it) (gethash "transcript" it)) (aref (oref obj :alternatives) 0)))

(cl-defmethod conversation-get-tags ((obj tog-conversation))
  (if (not (slot-boundp obj :tag)) nil
    (oref obj :tag)))

(cl-defmethod conversation-set-tag ((obj tog-conversation) tag)
  "TODO: Do the proper setf thing."
  (let ((current-tags (conversation-get-tags obj)))
    (let ((tag-idx (-find-index (lambda (t) (string= (alist-get 'tag-type t) (alist-get 'tag-type tag))) current-tags)))
      (if (null tag-idx)
          (oset obj :tag (cons tag current-tags))
        (oset obj :tag (-replace-at tag-idx tag current-tags))))))

(defun tog-alts-conversations-from-json (file-path)
  "Read conversations dictionaries from json. We need the
following keys in each dictionary."
  (let ((convs (mapcar #'make-tog-conversation (json-parse-string (f-read file-path)))))
    (setq tog-alts-conversations (mapcar (lambda (conv) (cons (oref conv :id) conv)) convs))
    nil))

(defun tog-tags-table ()
  "Create tag hash table for currently loaded conversations"
  (let ((tb (make-hash-table)))
    (dolist (conv-pair tog-alts-conversations)
      (let ((conv (cdr conv-pair)))
        (if (and (slot-boundp conv :tag) (oref conv :tag))
            (setf (gethash (number-to-string (oref conv :id)) tb) (oref conv :tag)))))
    tb))

(defun tog-alts-save ()
  "Save tags in given file path TODO: Remove hardcoded piece."
  (interactive)
  (let ((file-path "./tog-alts-tags.json"))
    (f-write (json-encode-hash-table (tog-tags-table)) 'utf-8 file-path)
    (message "Tags saved at %s" file-path)))

(cl-defmethod tog-alts-disp ((obj tog-conversation))
  "Display a tog item in buffer for tagging."
  (let ((buffer (get-buffer-create tog-alts-buffer-name)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (tog-alts-mode)
      (insert "* " (number-to-string (oref obj :id)) "\n")
      (org-set-property "REFTIME" (oref obj :reftime))
      (org-set-property "CALLURL" (call-url obj))
      (org-set-property "STATE" (s-replace-all '(("_" . "-")) (oref obj :state)))
      (insert "\n")
      (let ((i 0))
        (dolist (text (conversation-texts obj))
          (incf i)
          (insert (number-to-string i) ". " text "\n")))
      (if (slot-boundp obj :tag)
          (org-todo "DONE"))
      (read-only-mode)
      (goto-char (point-min))
      (re-search-forward "^1\. "))
    (setq tog-alts-conversation-id (oref obj :id))
    (switch-to-buffer buffer)))

(defun tog-alts-get-alt-number ()
  "Return alternative number for current line."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward "^\\([[:digit:]]\\)\. " (line-end-position) t)
      (string-to-number (match-string-no-properties 1)))))

(defun tog-alts-get-text-range ()
  "Return text range for marked alternative."
  (when (region-active-p)
    (let ((bounds (car (region-bounds)))
          (line-start (line-beginning-position)))
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward "^\\([[:digit:]]\. +\\)" (line-end-position) t)
          (let ((offset (+ line-start (length (match-string-no-properties 1)))))
            (list (- (car bounds) offset) (- (cdr bounds) offset))))))))

(defun tog-alts-current-conv ()
  (alist-get tog-alts-conversation-id tog-alts-conversations))

(defun tog-current-index ()
  ;; TODO Simplify this
  (if (not tog-alts-conversation-id)
      (setq tog-alts-conversation-id (caar tog-alts-conversations)))
  (-find-index (lambda (pair) (= tog-alts-conversation-id (car pair))) tog-alts-conversations))

(defun tog-alts-next ()
  (interactive)
  (if (= (length tog-alts-conversations) (or (tog-current-index) 0))
      (message "Reached end of items")
    (let ((idx (if (null tog-alts-conversation-id) 0 (+ (tog-current-index) 1))))
      (tog-alts-disp (cdr (nth idx tog-alts-conversations))))))

(defun tog-alts-prev ()
  (interactive)
  (if (= 0 (or (tog-current-index) 0))
      (message "Reached beginning of items")
    (let ((idx (if (null tog-alts-conversation-id) 0 (- (tog-current-index) 1))))
      (tog-alts-disp (cdr (nth idx tog-alts-conversations))))))

(defun tog-alts-play ()
  (interactive)
  (play (tog-alts-current-conv)))

(defun tog-alts-make-tag (tag-type)
  "Create tag from current selection and input."
  (let* ((user-entry (read-string (format "%s> " tag-type)))
         (tag-data (if (string= user-entry "") nil user-entry))
         (alt-number (tog-alts-get-alt-number))
         (text-range (tog-alts-get-text-range)))
    (conversation-set-tag (tog-alts-current-conv)
                          `((tag-type . ,tag-type)
                            (tag-data . ,tag-data)
                            (alt-number . ,alt-number)
                            (text-range . ,text-range)))
    ;; Highlight for feedback
    (when (region-active-p)
      (ov (region-beginning) (region-end)
          '(face (:background "#f1c40f" :foreground "#191d24")))
      (deactivate-mark))))

(defun tog-alts-annotate ()
  "Annotate current conversation."
  (interactive)
  (helm :sources (helm-build-sync-source "types"
                   :candidates tog-alts-types
                   :action '(("annotate" . tog-alts-make-tag)))
        :buffer "*helm tog-alts tag*"
        :prompt "Type: "))

;;;###autoload
(define-derived-mode tog-alts-mode org-mode "tog-alts"
  "Major mode for doing tog-alts tagging.")

(defun tog-alts ()
  "Start tagging"
  (interactive)
  (if (null tog-alts-conversations)
      (message "No file loaded, try running tog-alts-conversations-from-json")
    (setq tog-alts-conversation-id nil)
    (tog-alts-next)))

(defun tog-alts-reset ()
  (setq tog-alts-conversations nil))

;; TODO: Make this go in code
(tog-alts-conversations-from-json "./alts-dt-num.json")

;; Keys
(define-key tog-alts-mode-map (kbd "t") 'tog-alts-annotate)
(define-key tog-alts-mode-map (kbd "f") 'tog-alts-next)
(define-key tog-alts-mode-map (kbd "b") 'tog-alts-prev)
(define-key tog-alts-mode-map (kbd "p") 'tog-alts-play)
(define-key tog-alts-mode-map (kbd "C-x C-s") 'tog-alts-save)

(provide 'tog-alts)

;;; tog-alts.el ends here
