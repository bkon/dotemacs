;; git-gutter-fringe+
;; https://github.com/nonsequitur/git-gutter-fringe-plus

;; Display GitGutter icons in the right fringe
(setq-default git-gutter-fr+-side 'right-fringe)

;; Enable GitGutter+ fringe mode
(require 'git-gutter-fringe+)
(git-gutter+-enable-fringe-display-mode)
