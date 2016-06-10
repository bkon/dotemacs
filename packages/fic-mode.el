;; fic-mode
;; http://www.emacswiki.org/emacs/fic-mode.el

(setq-default fic-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE" "TBD"))

(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)
