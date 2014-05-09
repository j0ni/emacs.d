;;; j0ni-complete.el -- Auto completion

(package-require 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-\\") 'company-complete)

(setq company-global-modes '(not term-mode))

(setq company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-align-annotations t)

;; Javascript
(package-require 'company-tern)
(add-to-list 'company-backends 'company-tern)

;; Cider
(package-require 'company-cider)
(add-to-list 'company-backends 'company-cider)

;; Go Lang
(package-require 'company-go)
(add-to-list 'company-backends 'company-go)

;; Python
(package-require 'company-inf-python)
(add-to-list 'company-backends 'company-inf-python)

;; Python
(package-require 'company-inf-ruby)
(add-to-list 'company-backends 'company-inf-ruby)


(provide 'j0ni-complete)