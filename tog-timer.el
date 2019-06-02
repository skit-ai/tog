;;; tog-timer.el --- Timing and speed estimation -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Timing and speed estimation
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

(defvar tog-timer-start-time nil
  "Time when we started tagging.")

(defvar tog-timer-done-count 0
  "Counts of items done in current tagging session")

(defun tog-timer-start ()
  "Commence time logging."
  (setq tog-timer-start-time (float-time)
        tog-timer-done-count 0))

(defun tog-timer-reset ()
  "Reset time values and other logs."
  (setq tog-timer-start-time nil
        tog-timer-done-count 0))

(defun tog-timer-update (&optional increment)
  "Update count of done items for this session."
  (setq tog-timer-done-count (+ tog-timer-done-count (or increment 1))))

(defun tog-timer-speed ()
  "Return seconds taken for tagging one item.

NOTE: This doesn't take care of the following probably important
things:
1. Skipped items where we also spent some time
2. Anomalies, specially high values

The idea right now is to just have rough estimate of time spent
in producing a tag, which covers the high level purpose of
timing."
  (let ((time-diff (- (float-time) tog-timer-start-time)))
    (when (not (zerop tog-timer-done-count))
      (/ time-diff tog-timer-done-count))))

(provide 'tog-timer)

;;; tog-timer.el ends here
