;; Basic init file for working with standalone `cask emacs'

(add-to-list 'load-path default-directory)
(setq make-backup-files nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'wombat)

(require 'tog)
(add-hook 'tog-tag-update-hook 'tog-save-tags)
(define-key tog-mode-map (kbd "DEL") 'tog-untag)
