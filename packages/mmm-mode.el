;; mmm-mode
;; https://github.com/purcell/mmm-mode

(require 'mmm-mode)

(mmm-add-group
 'fancy-html
 '((html-php-tagged
    :submode php-mode
    :face mmm-code-submode-face
    :front "<[?]php"
    :back "[?]>")
   (html-css-embedded
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "\]*>"
    :back "")
   (html-css-attribute
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "\\bstyle=\\s-*\""
    :back "\"")
   (html-javascript-attribute
    :submode javascript-generic-mode
    :face mmm-code-submode-face
    :front "\\bon\\w+=\\s-*\""
    :back "\"")))

(mmm-add-group
 'html-css
 '((css-cdata
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</script>" @)))
   (css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>[ \t]*\n?"
    :back "[ \t]*</style>"
    :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                 @ "\n" _ "\n" @ "</style>" @)))
   (css-inline
    :submode css-mode
    :face mmm-code-submode-face
    :front "style=\""
    :back "\"")))

;; Enable support for mmm extensions
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-js))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil embedded-css))
(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))
