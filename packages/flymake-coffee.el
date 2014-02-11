;; flymake-coffee
;; https://github.com/purcell/flymake-coffee

;; requires: coffeelint [npm], coffee-script [npm]

(add-hook 'coffee-mode-hook 'flymake-coffee-load)
