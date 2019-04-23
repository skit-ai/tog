;; Basic init file for working with standalone `cask emacs'

(add-to-list 'load-path default-directory)
(setq make-backup-files nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'wombat)

(require 'tog)
;; TODO: Change me to enable a certain kind of tagging

(tog-conv-load-from-json "./alts-dt-num.json")
(setq tog-types '("people" "date" "time" "number" "datetime"))

;; Keys
(define-key tog-mode-map (kbd "t") 'tog-conv-tag)
(define-key tog-mode-map (kbd "f") 'tog-next)
(define-key tog-mode-map (kbd "b") 'tog-prev)
(define-key tog-mode-map (kbd "p") 'tog-conv-play)
(define-key tog-mode-map (kbd "q") 'tog-quit)
(define-key tog-mode-map (kbd "C-x C-s") 'tog-save)
