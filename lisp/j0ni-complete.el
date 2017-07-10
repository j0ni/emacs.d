;;; j0ni-complete.el -- Auto completion

(package-require 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; (global-set-key (kbd "C-\\") 'company-complete)
(diminish 'company-mode)

(setq company-global-modes '(not term-mode org-mode))

(setq company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-align-annotations t
      company-idle-delay 1)

;; Javascript
(package-require 'company-tern)
(add-to-list 'company-backends 'company-tern)

;; Go Lang
(package-require 'company-go)
(add-to-list 'company-backends 'company-go)

;; Python
(package-require 'company-anaconda)
(add-to-list 'company-backends '(company-anaconda :with company-capf))

;; Ruby
(package-require 'company-inf-ruby)
(add-to-list 'company-backends 'company-inf-ruby)

;; Haskell
(package-require 'company-ghc)
(add-to-list 'company-backends 'company-ghc)

;; Rust
(package-require 'company-racer)
(add-to-list 'company-backends 'company-racer)

(provide 'j0ni-complete)
