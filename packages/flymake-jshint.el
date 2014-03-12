;; flymake-jshint
;; https://github.com/Wilfred/flymake-jshint.el

;; requires: jshint [npm]

(add-hook 'js-mode-hook 'flymake-jshint-load)
(add-hook 'js2-mode-hook 'flymake-jshint-load)
