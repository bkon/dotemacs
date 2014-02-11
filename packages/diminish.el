;; diminish
;; http://www.emacswiki.org/emacs/DiminishedModes

(mapc #'(lambda (mode) 
	  (eval-after-load 
	      (symbol-name mode) 
	    (lambda () (diminish mode))))
	  '(
	    abbrev-mode
	    anzu-mode
	    auto-complete-mode
	    auto-fill-function
	    fic-mode
	    flymake-mode
	    flyspell-mode
	    git-gutter+-mode
	    helm-mode
	    page-break-lines-mode
	    projectile-mode
	    robe-mode
	    yas-minor-mode
))
