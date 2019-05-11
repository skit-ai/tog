;; Basic init file for working with standalone `cask emacs'

(add-to-list 'load-path default-directory)
(setq make-backup-files nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'wombat)

(require 'tog)
(require 'tog-stats)

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

;; Cache dir for audios
(setq tog-player-cache (expand-file-name "./audios/"))

;; NOTE: Here is a pipeline for tagging entities
(setq tog-types '("PEOPLE" "DATE" "TIME" "NUMBER" "DATETIME"))
(setq tog-conv-method 'ranged)

;; First we load the data file with items to tag
(tog-stats-count "./alts-dt-num.json")
(tog-conv-load-from-json "./alts-dt-num.json")
;; Next, optionally, load the already done tags
(tog-load-tags)

;; NOTE: Here is a pipeline for single intent +/- tagging.
(setq tog-types '("LOCATION-PRESENT"))
(setq tog-conv-method 'boolean)
(tog-stats-count "./alts-location.json")
(tog-conv-load-from-json "./alts-location.json")
(tog-load-tags)

;; Hook for fast jumps
(defun tog-conv-go-go ()
  (tog-next)
  (tog-conv-play)
  (tog-conv-tag))

(add-hook 'tog-conv-after-tag-hook #'tog-conv-go-go)
