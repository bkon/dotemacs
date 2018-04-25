;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck)
;; (require 'flycheck-flow)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
                  (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
        (setq-local flycheck-javascript-eslint-executable eslint)))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(flycheck-def-config-file-var flycheck-typescript-tsconfig
    typescript-tslint "tslint.json"
  :safe #'stringp
  :package-version '(flycheck . "27"))

(flycheck-add-mode 'javascript-eslint 'jsx-mode)
