;;; tog-player.el --- Player component for tog -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Player component for tog
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
(require 'f)

(defvar tog-player-proc nil
  "Process related to the audio player.")

(defvar tog-player-cache nil
  "Cache dir to look for files")

(defcustom tog-player-command "mplayer"
  "Command for the cli audio player")

(defun tog-player-cache-get (url)
  "Return path of local file for the given url."
  (when tog-player-cache
    (let ((local-file (f-join tog-player-cache (f-filename url))))
      (when (f-exists? local-file)
        local-file))))

(defun tog-player-play (url)
  "Play given url, possible from local cache."
  (if tog-player-proc (delete-process tog-player-proc))
  (--if-let (tog-player-cache-get url) (setq url it))
  (setq tog-player-proc (start-process "tog-player" nil tog-player-command url)))

(provide 'tog-player)

;;; tog-player.el ends here
