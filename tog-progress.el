;;; tog-progress.el --- Progress bar component for tog -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Progress bar component for tog
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

(require 'dash)
(require 's)
(require 'tog-io)
(require 'tog-timer)

(cl-defmethod tog-progress-done ((loader tog-io-jumpable-loader))
  "Return count of done items. This works over all the items and
not only for the current session."
  (-reduce-from (lambda (acc it) (if (oref it :tag) (+ acc 1) acc)) 0 (oref loader :items)))

(defun tog-progress-build-bar (n)
  "Return bar string of given size."
  (propertize (s-repeat n "—") 'face 'font-lock-keyword-face))

(cl-defmethod tog-progress-build-header ((loader tog-io-loader))
  "Return nothing when loader is not jumpable."
  "")

(cl-defmethod tog-progress-build-header ((loader tog-io-jumpable-loader))
  "Prepare string to show in header line"
  (let* ((width (window-width))
         (total-items (length (oref loader :items)))
         (current-pos (round (* width (/ (float (oref loader :current-index)) total-items))))
         (n-done (round (* width (/ (float (tog-progress-done loader)) total-items))))
         (current-pos-marker "⯆"))
    (if (> current-pos n-done)
        (concat (tog-progress-build-bar n-done)
                (s-repeat (- current-pos n-done) " ")
                current-pos-marker)
      (concat (tog-progress-build-bar current-pos)
              current-pos-marker
              (tog-progress-build-bar (- n-done current-pos 1))))))

(cl-defmethod tog-progress-report ((loader tog-io-loader))
  (let ((speed (tog-timer-speed)))
    (message "Speed: %s per item" (if speed (seconds-to-string speed) "NA"))))

(cl-defmethod tog-progress-report ((loader tog-io-jumpable-loader))
  "Display (in message for now) update on the progress of current
session."
  (let* ((total-items (length (oref loader :items)))
         (n-done (tog-progress-done loader))
         (n-left (- total-items n-done))
         (speed (tog-timer-speed)))
    (message "Total items: %s\nDone: %s\nTime left to do %s items: %s [%s per item]"
             total-items n-done n-left
             (if speed (seconds-to-string (* n-left speed)) "NA")
             (if speed (seconds-to-string speed) "NA"))))

(provide 'tog-progress)

;;; tog-progress.el ends here
