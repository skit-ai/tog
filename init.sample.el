;; Example init file for working with standalone `cask emacs'

(add-to-list 'load-path default-directory)

(require 'tog)
(require 'tog-stats)
(require 'cl-lib)

;; Define Keys
(define-key tog-mode-map (kbd "RET") 'tog-conv-tag)
(define-key tog-mode-map (kbd "n") 'tog-next)
(define-key tog-mode-map (kbd "N") 'tog-next-untagged)
(define-key tog-mode-map (kbd "p") 'tog-prev)
(define-key tog-mode-map (kbd "P") 'tog-prev-untagged)
(define-key tog-mode-map (kbd "SPC") 'tog-conv-play)

(define-key tog-mode-map (kbd "DEL") 'tog-conv-clear)

(define-key tog-mode-map (kbd "q") 'tog-quit)
(define-key tog-mode-map (kbd "C-x C-s") 'tog-save)

(define-key tog-mode-map (kbd "t") 'tog-progress-session-report)

;; Cache dir for audios
(setq tog-player-cache (expand-file-name "./audios/"))
(setq tog-player-command "mplayer") ;; sox, cvlc etc.

;; ---------------------------------
;; Setup for tagging ranged entities
;; ---------------------------------
(setq tog-types '("PEOPLE" "DATE" "TIME" "DATETIME"))
(setq tog-conv-method 'ranged)
;; First we load the data file with items to tag
(tog-conv-load-from-json "./conv-dt-num.json")
;; Next, optionally, load the already done tags
(tog-load-tags)
(add-hook 'tog-conv-after-tag-hook #'tog-timer-update)
;; Start the tagging
(tog)

;; -----------------------------------
;; Setup for single intent +/- tagging
;; -----------------------------------
(setq tog-types '("LOCATION-PRESENT"))
(setq tog-conv-method 'boolean)
(tog-conv-load-from-json "./conv-location.json")
(tog-load-tags)
(add-hook 'tog-conv-after-tag-hook #'tog-timer-update)

;; Hook for fast jumps
(defun tog-conv-go-go ()
  (tog-next)
  (tog-conv-play)
  (tog-conv-tag))

(add-hook 'tog-conv-after-tag-hook #'tog-conv-go-go)
(tog)

;; -----------------------
;; Setup for transcription
;; -----------------------
(setq tog-types '("TRANSCRIPT"))
(setq tog-conv-method 'transcript)
(tog-conv-load-from-json "./conv-transcript.json")
(tog-load-tags)
(add-hook 'tog-conv-after-tag-hook #'tog-timer-update)
(tog)

;; --------------------------
;; Setup for single range NER
;; --------------------------
(setq tog-types '("LOCATION"))
(setq tog-conv-method 'ranged)
(tog-conv-load-from-json "./conv-location.json")
(tog-load-tags)
(add-hook 'tog-conv-after-tag-hook #'tog-timer-update)
(tog)

;; ------------------------
;; Setup for intent tagging
;; ------------------------
(setq tog-types '("some-intent" "another-intent"))
(setq tog-conv-method 'boolean)
(tog-conv-load-from-json "./conv-intent.json")
(tog-load-tags)
(add-hook 'tog-conv-after-tag-hook #'tog-timer-update)
(tog)

;; --------------
;; Tags from json
;; --------------

;; In such cases, each item from the json needs to be parsed in pairs of
;; (search-string, tag).
;; For example: ("place-one, that old city, this country etc." . "place-one")
;; In helm, we will show the descriptions (car) of the pairs and keep
;; tag (cdr) as the output.

(defun parse-location--value (value)
  "Return a string from the location json item value."
  (cl-etypecase value
    (string value)
    (list (s-join " " (cl-remove-if #'null (mapcar #'parse-location--value value))))
    (number nil)))

(defun parse-location-item (item)
  "Make a tog-type from given json item."
  (let* ((id (alist-get 'id item))
         (main-text (concat (parse-location--value (alist-get 'key item)) " " (alist-get 'city item)))
         (ignore-keys '(google_place_id city key))
         (rest-values (mapcar #'cdr (cl-remove-if (lambda (kv) (member (car kv) ignore-keys)) item)))
         (rest-text (s-join " " (cl-remove-if #'null (mapcar #'parse-location--value rest-values)))))
    (cons (format "id %d: %s\n\n%s" id main-text rest-text)
          (number-to-string id))))

(let ((file "./location-records.json")
      (json-array-type 'list))
  (setq tog-types (mapcar #'parse-location-item (json-read-file file))))

(setq tog-conv-method 'ranged)
(tog-conv-load-from-json "./conv-location.json.gz")
(tog-load-tags)
(add-hook 'tog-conv-after-tag-hook #'tog-timer-update)
(tog)
