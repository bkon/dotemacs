;; flymake-json
;; https://github.com/purcell/flymake-json

;; requires: jsonlint [npm]

(add-hook 'json-mode 'flymake-json-load)
(add-hook 'js-mode-hook 'flymake-json-maybe-load)
