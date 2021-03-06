;;; j0ni-ruby.el

;; (require 'ruby-mode)
(autoload 'ruby-mode "ruby-mode" nil t)

(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))

(use-package robe
  :defer t
  :hook (ruby-mode))

(defun ruby-mode-custom ()
  (interactive)
  (setq ruby-deep-arglist t
        ruby-deep-indent-paren nil
        show-trailing-whitespace nil)
  (inf-ruby-minor-mode t)
  (electric-layout-mode t)
  (electric-pair-mode t))

(add-hook 'ruby-mode-hook 'ruby-mode-custom)

;; (packages-require
;;  '(autopair
;;    ruby-additional))

;; (eval-after-load 'ruby-mode
;;   '(progn
;;      (require 'ruby-mode-indent-fix)
;;      (require 'ruby-additional)))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(use-package rspec-mode
  :defer t)

(use-package inf-ruby
  :defer t
  :commands (inf-ruby-minor-mode inf-ruby-switch-setup)
  :config
  (add-to-list 'inf-ruby-implementations '("tux" . "tux")))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(setq inf-ruby-default-implementation "pry")

;; extra stuff, mostly for rails
;; (packages-require
;;  '(feature-mode
;;    web-mode
;;    rinari
;;    ruby-electric
;;    ruby-tools
;;    chruby))

(package-require 'rhtml-mode)
(add-hook 'rhtml-mode-hook 'turn-off-auto-fill)
(add-hook 'ruby-mode 'turn-off-auto-fill)

;; set up yaml mode, for want of a better place
;; (package-require 'yaml-mode)
;; (autoload 'yaml-mode "yaml-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(provide 'j0ni-ruby)
