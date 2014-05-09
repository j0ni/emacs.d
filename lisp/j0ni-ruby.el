;;; j0ni-ruby.el

(package-require 'ruby-mode)
(autoload 'ruby-mode "ruby-mode" nil t)

(package-require 'ruby-dev)
(autoload 'turn-on-ruby-dev "ruby-dev" nil t)

(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))

(package-require 'smartparens)
(defun ruby-mode-custom ()
  (interactive)
  (setq ruby-deep-arglist t
        ruby-deep-indent-paren nil
        show-trailing-whitespace nil)
  (smartparens-mode +1)
  ;; (turn-on-ruby-dev)
  )

(add-hook 'ruby-mode-hook 'ruby-mode-custom)

(packages-require
 '(autopair
   ruby-additional))

(eval-after-load 'ruby-mode
  '(progn
     (require 'ruby-mode-indent-fix)
     (require 'ruby-additional)))

(package-require 'rspec-mode)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; extra stuff, mostly for rails
(packages-require
 '(rinari
   feature-mode
   web-mode
   robe
   ruby-electric
   ruby-tools
   rbenv))

(package-require 'rhtml-mode)
(add-hook 'rhtml-mode-hook 'turn-off-auto-fill)

;; set up yaml mode, for want of a better place
(package-require 'yaml-mode)
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(provide 'j0ni-ruby)
