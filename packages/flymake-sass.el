;; flymake-sass
;; https://github.com/purcell/flymake-sass

;; requires: sass [gem]

(add-hook 'sass-mode-hook 'flymake-sass-load)
(add-hook 'scss-mode-hook 'flymake-sass-load)
