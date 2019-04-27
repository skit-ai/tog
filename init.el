;; Basic init file for working with standalone `cask emacs'

(add-to-list 'load-path default-directory)
(setq make-backup-files nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'wombat)

(require 'tog)

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

;; NOTE: Here is a pipeline for tagging entities
(setq tog-types '("PEOPLE" "DATE" "TIME" "NUMBER" "DATETIME"))
(setq tog-method 'ranged)

;; First we load the data file with items to tag
(tog-conv-load-from-json "./alts-dt-num.json")
;; Next, optionally, load the already done tags
(tog-load)

;; NOTE: Here is a pipeline for single intent +/- tagging.
(setq tog-types '("LOCATION-PRESENT"))
(setq tog-method 'boolean)
(tog-conv-load-from-json "./alts-location.json")

(tog-load)
