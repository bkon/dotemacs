;; fic-mode
;; http://www.emacswiki.org/emacs/fic-mode.el

(setq-default fic-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE" "TBD"))
(add-hook 'prog-mode-hook 'turn-on-fic-mode)
