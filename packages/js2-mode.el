;; js2-mode
;; https://github.com/mooz/js2-mode

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq-default js2-indent-on-enter-key t
              js2-auto-indent-p t
              js2-bounce-indent-p t
              ;; Idle timeout before reparsing buffer
              js2-idle-timer-delay 0.5
              ;; load browser-specific functions
              js2-include-browser-externs t
              ;; Support Node.js
              js2-include-node-externs t
              js2-skip-preprocessor-directives t
              ;; Disable error parsing in favor of Flycheck
              js2-show-parse-errors nil
              js2-strict-missing-semi-warning nil)
