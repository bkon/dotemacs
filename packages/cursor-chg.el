;; cursor-chg
;; http://www.emacswiki.org/emacs/cursor-chg.el

(require 'cursor-chg)
(change-cursor-mode 1)
(toggle-cursor-type-when-idle 1)

(setq-default curchg-default-cursor-type 'box
              curchg-default-cursor-color "Forest green"
              curchg-overwrite/read-only-cursor-type 'hbar)
