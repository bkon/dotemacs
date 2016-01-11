;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck)
;; (require 'flycheck-flow)
(flycheck-add-mode 'javascript-eslint 'jsx-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)
