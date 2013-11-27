(require 'cl)

;; MELPA package archive
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(let ((packages '(anzu
                  bookmark+
                  browse-kill-ring
                  bundler
                  butler
                  coffee-mode
                  col-highlight
                  csv-mode
                  cursor-chg
                  dired+
                  diminish
                  dtrt-indent
                  expand-region
                  feature-mode
                  fill-column-indicator
                  flymake-coffee
                  flymake-css
                  flymake-cursor
                  flymake-haml
                  flymake-jshint
                  flymake-jslint
                  flymake-json
                  flymake-php
                  flymake-puppet
                  flymake-ruby
                  flymake-sass
                  flymake-shell
                  flymake-yaml
                  flyspell-lazy
                  frame-restore
                  gist
                  git-gutter+
                  git-gutter-fringe+
                  graphviz-dot-mode
                  haml-mode
                  icicles
                  js2-mode
                  js2-refactor
                  json-mode
                  magit
                  markdown-mode
                  nginx-mode
                  page-break-lines
                  php-mode
                  popup
                  popwin
                  powerline
                  projectile
                  puppet-mode
                  rainbow-mode
                  revive
                  rspec-mode
                  sass-mode
                  scss-mode
                  smart-mode-line
                  smart-operator
                  sql-indent
                  syslog-mode
                  visual-regexp
                  visual-regexp-steroids
                  yaml-mode
                  yasnippet
                  zenburn-theme
                  zencoding-mode)))

  (unless (every #'package-installed-p packages)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (mapc #'(lambda (package)
              (unless (package-installed-p package)
                (package-install package)))
          packages)))

;; == Key bindings

;; Move between windows using Shift + Arrows
(windmove-default-keybindings)

;; Kill Ring browsing (M-y displays candidates)
(browse-kill-ring-default-keybindings)

;; Hippie expand
(global-set-key [C-tab] 'hippie-expand)

;; Indent on pressing RET (instead of just newline)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; == Global behavior

;; Enable several advanced features which are diabled by default
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Default coding system
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Delete trailing whitespace when file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Icicles (http://www.emacswiki.org/emacs/Icicles)
(icy-mode 1)

;; Disable backups
(setq backup-inhibited t)

;; Disable auto save
(setq auto-save-default nil)

;; Midnight mode (cleans up old unused buffers)
(setq midnight-mode t)

;; Scroll by 1 line instead of jumping around
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t ; rename after killing
      uniquify-ignore-buffers-re "^\\*")

;; Recent files
(recentf-mode 1)

;; Use simple y/n prompt
(fset 'yes-or-no-p 'y-or-n-p)

;; == Session management

;; revive hooks for automatically saving and restoring window configuration
(add-hook 'kill-emacs-hook 'save-current-configuration)
(add-hook 'after-init-hook 'resume)

;; Save buffers / locations (desktop)
(desktop-save-mode 1)

;; Save buffer position
(setq-default save-place t)

;; == Generic appearance

;; Maximized
(setq-default initial-frame-alist (quote ((fullscreen . maximized))))

;; Turn off toolbar
(tool-bar-mode -1)

;; Turn off menu bar
(menu-bar-mode -1)

;; Hide scrollbars
(scroll-bar-mode -1)

;; Default color theme
(load-theme 'zenburn t)

;; Enable line number in the status line
(line-number-mode)

;; Enable column number in the status bar
(column-number-mode)

;; Enable percentage position display in the status bar
(size-indication-mode)

;; Hide left fringe (vertical line along  the side of the frame), keep
;; right fringe
(fringe-mode '(0 . 10))

;; set  left margin  width instead  of left  fringe, so  content won't
;; stick to the left edge without line numbers; linum mode resets left
;; margin
(setq-default left-margin-width 1)

;; Show keystrokes being typed almost immediately
(setq echo-keystrokes 0.1)

;; == Editor behavior

;; Don't use tabs
(setq-default indent-tabs-mode nil)

;; Show matching paren
(show-paren-mode 1)

;; Enable autopairing / smart quotes
(electric-pair-mode 1)

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace 1)
;; ... and don't do that in eshell mode
(add-hook 'eshell-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))
;; ... and don't do that in help mode
(add-hook 'help-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

;; Highlight current line
(global-hl-line-mode)

;; Show current defun in status bar
(which-func-mode 1)

;; Make M-q treat bulleted lists (starting with -*) as paragraphs
(setq paragraph-start "\f\\|[ \t]*$\\|[-*] +.+$"
      paragraph-separate "$")

;; Navigate using subwords for CamelCaseIdentifiers
(global-subword-mode 1)

;; YASnippet is enabled globally
(yas-global-mode 1)

;; Popup+autocompletion for YASnippet

(require 'popup)
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-no-prompt))

;; Simple yas helper function
(defun uncapitalize (str)
  (concat
   (downcase (substring str 0 1))
   (substring str 1)  ))

;; Use yasnippet for file skeletons
(defun autoinsert-yas-expand()
  (progn
    (interactive)
    (yas-expand)
    ))

;; Methods to use when doing hippie expand
(setq-default hippie-expand-try-functions-list
              '(yas-hippie-try-expand ;; YASnippet expansion is tried first
                try-complete-file-name-partially
                try-complete-file-name
                try-expand-all-abbrevs
                try-expand-list
                try-expand-line
                try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol))

;; File templates
(setq-default auto-insert-query nil)
(auto-insert-mode)

;; == Modes

;; ==== CoffeeScript

(add-hook 'coffee-mode-hook 'flymake-coffee-load)

;; Default tab width for Coffeescript files
(setq coffee-tab-width 4)

;; ==== CSS

(add-hook 'css-mode-hook 'flymake-css-load)
(add-hook 'css-mode-hook
          '(lambda () (rainbow-mode 1)))

;; ==== Haml

(add-hook 'haml-mode-hook 'flymake-haml-load)

;; ==== HTML/SGML
(add-hook 'sgml-mode-hook 'zencoding-mode)

;; ==== JS

(add-hook 'js-mode-hook 'flymake-jshint-load)
(add-hook 'js-mode-hook 'flymake-jslint-load)

;; ==== JSON

(add-hook 'json-mode 'flymake-json-load)

;; ==== PHP

(define-auto-insert
  '(php-mode . "Basic PHP header")
  ['(nil "<?php")
   autoinsert-yas-expand])

(add-hook 'php-mode-hook 'flymake-php-load)
(add-hook 'php-mode-hook 'dtrt-indent-mode)

;; enable fill column indicator
;;
;; - disabled due Emacs bug
;; https://github.com/alpaker/Fill-Column-Indicator/issues/31
;;
;; (add-hook 'php-mode-hook 'fci-mode)

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

;; ==== Python

(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (column-highlight-mode 1) ; highlight current column
              )))

;; ==== Ruby

;; Recognize Guardfile as Ruby file
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

;; Recognize Gemfile as Ruby file
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))

;; Recognize *.gemspec as Ruby file
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))

(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; ==== SASS

(add-hook 'sass-mode-hook 'flymake-sass-load)

;; ==== SCSS

(add-hook 'scss-mode-hook 'flymake-sass-load)

;; ==== Shell

(add-hook 'sh-set-shell-hook 'flymake-shell-load)

;; ==== YAML

(add-hook 'yaml-mode-hook 'flymake-yaml-load)

;; == Anzu

(global-anzu-mode +1)

;; == Flyspell

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq-default
     ispell-program-name "aspell"
     ispell-list-command "--list"
     ispell-extra-args '("--sug-mode=ultra"))

;; == GutGutter+

(eval-after-load 'git-gutter+
  '(progn
     (define-key git-gutter+-mode-map (kbd "C-x g n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x g p") 'git-gutter+-previous-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x g v") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x g r") 'git-gutter+-revert-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x g s") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x g c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x g C") 'git-gutter+-stage-and-commit)

     ;; Display GitGutter icons in the right fringe
     (setq-default git-gutter-fr+-side 'right-fringe)

     ;; Enable GitGutter+ fringe mode
     (require 'git-gutter-fringe+)
     (git-gutter+-enable-fringe-display-mode)
     )
  )

;; Enable GitGutter by default
(global-git-gutter+-mode)

;; == Visual regexp steroids

(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key esc-map (kbd "C-M-r") 'vr/isearch-backward)
(define-key esc-map (kbd "C-M-s") 'vr/isearch-forward)

;; == Magit

(setq-default magit-diff-refine-hunk t)

;; == Line numbers

;; Line numbers are displayed in:

;; haml
(add-hook 'haml-mode-hook 'linum-mode)
;; html
(add-hook 'html-mode-hook 'linum-mode)
;; js
(add-hook 'js-mode-hook 'linum-mode)
;; lisp
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
;; php
(add-hook 'php-mode-hook 'linum-mode)
;; ruby
(add-hook 'ruby-mode-hook 'linum-mode)
;; shell
(add-hook 'sh-mode-hook 'linum-mode)

;; Line  numbers mode  customization:  numbers  are right-aligned  and
;; width of the linum column is calculated automatically
;;
;; Note:  whitespace-mode highlights  spaces in  margin, so  there's a
;; workaround for this: invisible leading zeros

(eval-after-load 'linum
  '(progn
     ;; Make leading zeros invisible (fg = bg)
     (defface linum-leading-zero
       `((t :inherit 'linum
            :foreground ,(face-attribute 'linum :background nil t)))
       "Face for displaying leading zeroes for line numbers in display margin."
       :group 'linum)

     ;; Pad line number with zeros using :linum-leading-zero face
     (defun linum-format-func (line)
       (let ((w
              (1+
               (length
                (number-to-string (count-lines (point-min) (point-max)))))))
         (concat
          ;; leading zeros
          (propertize (make-string (- w (length (number-to-string line))) ?0)
                      'face 'linum-leading-zero)
          ;; formatted number
          (propertize (number-to-string line) 'face 'linum))))

     (setq linum-format 'linum-format-func)))

;; == Page break lines

(global-page-break-lines-mode)

;; == Diminish

(diminish 'page-break-lines-mode)
(diminish 'git-gutter+-mode)
(diminish 'yas-minor-mode)
(diminish 'anzu-mode)
(diminish 'flyspell-mode)

;; == expand-region

(global-set-key (kbd "C-=") 'er/expand-region)

;; == Popwin

;; Makes temporary buffers "popup" windows which can be closed by pressing C-g

(require 'popwin)
(popwin-mode 1)

;; == Powerline

(require 'powerline)
(setq-default powerline-arrow-shape 'slant)
(powerline-default-theme)

;; == Automated customizations

;; Custom  line  number face  (linum):  slightly  smaller size,  fixed
;; normal weight
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:height 0.8 :weight normal))))
 '(show-paren-match ((t (:background "OliveDrab4" :foreground "black" :weight bold))))
 '(show-paren-mismatch ((t (:background "firebrick" :foreground "black" :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-auto-light-when-jump (quote any-bookmark))
 '(bmkp-auto-light-when-set (quote any-bookmark))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(safe-local-variable-values (quote ((js-indent-level . 2)))))
