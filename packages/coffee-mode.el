;; coffee-mode
;; https://github.com/defunkt/coffee-mode
;;
;; C-m, Return          Insert newline and indent line
;; C-c C-<, backtab     Indent line or region to left
;; C-c C->              Indent line or region to right
;; C-M-a                Move to beginning of defun
;; C-M-e                Move to end of defun
;; C-M-h                Mark this defun
;; A-r, C-c C-k         Compile buffer to JavaScript
;; A-R                  Compile content of region to JavaScript
;; A-M-r, C-c C-z       Run CoffeeScript REPL
;; C-c C-l              Send this line to REPL buffer
;; C-c C-r              Send content of region to REPL buffer
;; C-c C-b              Send content of buffer to REPL buffer
;; C-c C-o C-s          Enable coffee-cos-mode
;;
;; requires coffee-script [npm]
;;
;; Default tab width for Coffeescript files
(setq-default coffee-tab-width 4)

(add-hook 'coffee-mode-hook 'linum-mode)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)
