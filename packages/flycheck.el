;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'jsx-mode)
