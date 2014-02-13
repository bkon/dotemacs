;; flymake-puppet
;; https://github.com/purcell/flymake-php

;; requires: puppet-lint [gem]

(eval-after-load "puppet-mode"
  (lambda ()
    (add-hook puppet-mode-hook 'flymake-puppet-load)))
