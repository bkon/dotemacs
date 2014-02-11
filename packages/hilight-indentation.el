;; highlight-indentation
;; http://www.emacswiki.org/emacs/HighlightIndentation

(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-indentation-current-column-mode 1)
            ))
