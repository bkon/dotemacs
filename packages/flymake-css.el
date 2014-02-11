;; flymake-css
;; https://github.com/purcell/flymake-css

;; requires: csslint [npm]

(add-hook 'css-mode-hook 'flymake-css-load)
