;; bookmark+
;; http://www.emacswiki.org/emacs/BookmarkPlus

;; C-x r l - list bookmarks
;; C-x p m - add bookmark
;; C-x j j - jump to bookmark

(require 'bookmark+)

(setq-default bmkp-auto-light-when-jump 'any-bookmark)
(setq-default bmkp-auto-light-when-set 'any-bookmark)
(setq-default bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
