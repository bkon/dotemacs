;; visual-regexp
;; https://github.com/benma/visual-regexp.el

(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key esc-map (kbd "C-M-r") 'vr/isearch-backward)
(define-key esc-map (kbd "C-M-s") 'vr/isearch-forward)
