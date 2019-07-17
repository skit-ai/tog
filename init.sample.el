;; Example init file for working with standalone `cask emacs'

(add-to-list 'load-path default-directory)

(require 'tog)
(require 'cl-lib)
(require 'dash)

;; Define general keys
(define-key tog-mode-map (kbd "RET") 'tog-tag)
(define-key tog-mode-map (kbd "n") 'tog-next)
(define-key tog-mode-map (kbd "N") 'tog-next-untagged)
(define-key tog-mode-map (kbd "p") 'tog-prev)
(define-key tog-mode-map (kbd "P") 'tog-prev-untagged)
(define-key tog-mode-map (kbd "DEL") 'tog-clear)
(define-key tog-mode-map (kbd "C-x C-s") 'tog-save)
(define-key tog-mode-map (kbd "q") 'tog-quit)
(define-key tog-mode-map (kbd "t") 'tog-progress-session-report)

;; Conversation tagging specific keys
(define-key tog-mode-map (kbd "SPC") 'tog-conv-play)

;; Cache dir for audios
(setq tog-player-cache (expand-file-name "./audios/"))
(setq tog-player-command "mplayer") ;; sox, cvlc etc.

;; ---------------------------------
;; Setup for tagging ranged entities
;; ---------------------------------
(setq tog-conv-types '("PEOPLE" "DATE" "TIME" "DATETIME"))
(setq tog-conv-method 'ranged)
;; First we load the data file with items to tag
(tog-conv-load-from-json "./conv-dt-num.json")
;; Next, optionally, load the already done tags
(tog-load-tags)
(add-hook 'tog-annotate-hook #'tog-timer-update)
;; Start the tagging
(tog)

;; -----------------------------------
;; Setup for single intent +/- tagging
;; -----------------------------------
(setq tog-conv-types '("LOCATION-PRESENT"))
(setq tog-conv-method 'boolean)
(tog-conv-load-from-json "./conv-location.json")
(tog-load-tags)
(add-hook 'tog-annotate-hook #'tog-timer-update)

;; Hook for fast jumps
(defun tog-conv-go-go ()
  (tog-next)
  (tog-conv-play)
  (tog-conv-tag))

(add-hook 'tog-annotate-hook #'tog-conv-go-go)
(tog)

;; -----------------------
;; Setup for transcription
;; -----------------------
(setq tog-conv-types '("TRANSCRIPT"))
(setq tog-conv-method 'transcript)
(tog-conv-load-from-json "./conv-transcript.json")
(tog-load-tags)
(add-hook 'tog-annotate-hook #'tog-timer-update)
(tog)

;; --------------------------
;; Setup for single range NER
;; --------------------------
(setq tog-conv-types '("LOCATION"))
(setq tog-conv-method 'ranged)
(tog-conv-load-from-json "./conv-location.json")
(tog-load-tags)
(add-hook 'tog-annotate-hook #'tog-timer-update)
(tog)

;; ------------------------
;; Setup for intent tagging
;; ------------------------
(setq tog-conv-types '("some-intent" "another-intent"))
(setq tog-conv-method 'boolean)
(tog-conv-load-from-json "./conv-intent.json")
(tog-load-tags)
(add-hook 'tog-annotate-hook #'tog-timer-update)
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

(defun make-location-tag (record)
  "Make a tog-type from given json item."
  (let* ((id (alist-get 'id record))
         (main-text (concat (parse-location--value (alist-get 'key record)) " " (alist-get 'city record)))
         (ignore-keys '(google_place_id city key location))
         (rest-values (mapcar #'cdr (cl-remove-if (lambda (kv) (member (car kv) ignore-keys)) record)))
         (rest-text (s-join " " (cl-remove-if #'null (mapcar #'parse-location--value rest-values)))))
    (cons (format "id %d: %s\n\n%s" id main-text rest-text)
          (number-to-string id))))

(defun make-city-tags (records)
  "Create tags for city names from given records."
  (let ((city-alist (mapcar (lambda (rec) (cons (alist-get 'city rec)
                                           (alist-get 'city_id rec)))
                            records)))
    (mapcar (lambda (pair) (cons (format "city-id %d: %s" (cdr pair) (car pair))
                            (format "c%d" (cdr pair))))
            (-uniq city-alist))))

(let* ((file "./location.json")
       (json-array-type 'list)
       (records (json-read-file file)))
  (setq tog-conv-types (append (mapcar #'make-location-tag records)
                          (make-city-tags records)
                          ;; NOTE: -1 means there is no record match
                          (list (cons "id -1: NA" "-1")))))

(setq tog-conv-prefill-prompt t)
(setq tog-conv-method 'ranged)
(tog-conv-load-from-json "./conv-location.json.gz")
(tog-load-tags)
(add-hook 'tog-annotate-hook #'tog-timer-update)
(tog)
