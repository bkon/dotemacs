;; flymake-puppet
;; https://github.com/purcell/flymake-php

;; requires: puppet-lint [gem]

(add-hook puppet-mode-hook 'flymake-puppet-load)
