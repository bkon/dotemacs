;; helm-flycheck

(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-x a m") 'helm-flycheck))
