;; flymake-jslint
;; https://github.com/purcell/flymake-jslint

;; requires: jslint [npm]

(add-hook 'js-mode-hook 'flymake-jslint-load)
