;; ac-ispell
;; https://github.com/syohex/emacs-ac-ispell

(setq-default ac-ispell-requires 3)

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
