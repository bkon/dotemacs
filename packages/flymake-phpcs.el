;; flymake-phpcs
;; https://github.com/illusori/emacs-flymake-phpcs

;; requires: [php] phpcs

(setq flymake-phpcs-command "phpcs")
(setq flymake-phpcs-standard "PSR2")
(setq flymake-phpcs-show-rule t)

(require 'flymake-phpcs)
