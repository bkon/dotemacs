;; dired+
;; http://www.emacswiki.org/emacs/DiredPlus

(setq-default
 ;; Always copy recursively without asking.
 dired-recursive-copies 'always
 ;; Ask once when recursively deleting a directory.
 dired-recursive-deletes 'top)
