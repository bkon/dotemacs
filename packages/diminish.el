;; diminish
;; http://www.emacswiki.org/emacs/DiminishedModes

(defun bkon/diminish-after-load (package-name mode)
  (eval-after-load package-name
    `(diminish ,mode)))

(mapc
 #'(lambda
     (data)
     (bkon/diminish-after-load (car data) (cdr data)))
 '(
   (abbrev . 'abbrev-mode)
   (anzu . 'anzu-mode)
   (auto-complete . 'auto-complete-mode)
   (auto-fill . 'auto-fill-function)
   (fic-mode . 'fic-mode)
   (flymake . 'flymake-mode)
   (flyspell . 'flyspell-mode)
   (helm . 'helm-mode)
   (git-gutter+ . 'git-gutter+-mode)
   (page-break-lines . 'page-break-lines-mode)
   (projectile . 'projectile-mode)
   (robe . 'robe-mode)
   (yasnippet . 'yas-minor-mode)))
