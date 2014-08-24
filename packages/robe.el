;; robe
;; https://github.com/dgutov/robe
;;
;; required gems: pry, pry-doc, method_source
;;
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ac-sources (cons 'ac-source-robe ac-sources))
            ))
