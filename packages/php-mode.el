;; php-mode
;; https://github.com/ejmr/php-mode

(define-auto-insert
  '(php-mode . "Basic PHP header")
  ['(nil "<?php")
   autoinsert-yas-expand])

;; indent multiline function parameter lists using a single
;; indent, e.g.:
;;
;; function fun($param1,
;;     $param2,
;;     $param3)
;; {
;; ...
;;
(add-hook
 'php-mode-hook
 (lambda ()
   (progn
     (c-set-offset 'arglist-cont-nonempty
                   '(c-lineup-cascaded-calls +))
     (c-set-offset 'arglist-cont
                   '(c-lineup-cascaded-calls))
     )))

(add-hook 'php-mode-hook 'linum-mode)
