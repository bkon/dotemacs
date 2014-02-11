;; ac-js2
;; https://github.com/ScottyB/ac-js2

;; Causes skewer-mode to hang.
;; (setq-default ac-js2-evaluate-calls t)

(add-hook 'js2-mode-hook 'ac-js2-mode)
