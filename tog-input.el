;;; tog-input.el --- Input utilities for tog -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Input utilities for tog
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

(require 'helm)
(require 'tog-utils)

(defcustom tog-input-audio-dir nil
  "Directory to keep recorded audios in.")

(defcustom tog-input-arecord-args (list "-f" "S16_LE" "-r" "48000" "-c" "2" "-d" "600")
  "Arguments to send to arecord while recording. We put a max
duration limit so that an accident don't throw us out of memory.")

(defvar tog-input-arecord-proc nil
  "Variable holding the process used for recording.")

(defun tog-input-start-recording ()
  "Start recording audio."
  (let* ((tmp-file (make-temp-file "tog-input-audio"))
         (args (append tog-input-arecord-args (list ">" (shell-quote-argument tmp-file)))))
    (setq tog-input-arecord-proc (start-process-shell-command "arecord" nil (string-join (cons "arecord" args) " ")))
    (process-put tog-input-arecord-proc 'output-file tmp-file)))

(defun tog-input-stop-recording (output-file)
  "Stop recording and save wav output in OUTPUT-FILE."
  ;; NOTE: arecord takes kill (almost) gracefully but leaves the recording time
  ;;       wrong, so we fix it manually using sox
  (kill-process tog-input-arecord-proc)
  (let ((tmp-file (process-get tog-input-arecord-proc 'output-file)))
    (call-process "sox" nil nil nil "--ignore-length" tmp-file output-file)
    (setq tog-input-arecord-proc nil)
    (delete-file tmp-file)))

(defun tog-input-audio (rec-id)
  "Ask for audio from user and return file path."
  (let ((output-file (f-join tog-input-audio-dir rec-id)))
    (tog-input-start-recording)
    (read-string "Press RET when done.")
    (tog-input-stop-recording output-file)
    output-file))

(defun tog-input-string (name &optional default)
  "Ask for input string from user."
  (let ((entry (read-string (format "%s> " name) (or default (region-text)))))
    (if (string= entry "") nil entry)))

(defun tog-input-choice (options &optional prompt input)
  "Return one from given"
  (helm :sources (helm-build-sync-source "tog-input-choice"
                   :candidates options
                   :multiline t)
        :buffer "*helm tog input choice*"
        :prompt (or prompt "Choice: ")
        :input input))

(provide 'tog-input)

;;; tog-input.el ends here
