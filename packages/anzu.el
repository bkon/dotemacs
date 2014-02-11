;; anzu
;; https://github.com/syohex/emacs-anzu

(global-anzu-mode +1)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-c f r") 'anzu-replace-at-cursor-thing)
