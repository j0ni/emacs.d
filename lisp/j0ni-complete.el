;;; j0ni-complete.el -- Auto completion

(use-package company
  :diminish nil
  :config
  (global-company-mode)
  :init
  (setq company-global-modes '(not term-mode org-mode))
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers nil
        company-tooltip-align-annotations t
        company-tooltip-idle-delay nil
        ;; company-tooltip-idle-delay 0.5
        company-idle-delay nil
        ;; company-idle-delay 0.5
        ;; company-frontends '()
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-echo-metadata-frontend)))

;; Javascript
(use-package company-tern
  :init
  (add-to-list 'company-backends 'company-tern))

;; Go Lang
(use-package company-go
  :init
  (add-to-list 'company-backends 'company-go))

;; Python
(use-package company-anaconda
  :init
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; Ruby
(use-package company-inf-ruby
  :init
  (add-to-list 'company-backends 'company-inf-ruby))

;; Haskell
(use-package company-ghc
  :init
  (add-to-list 'company-backends 'company-ghc))

;; Rust
(use-package company-racer
  :init
  (add-to-list 'company-backends 'company-racer))

;; Etags
;; (package-require 'company-etags)
;; (require 'company-etags)
;; (add-to-list 'company-backends 'company-etags)

(use-package merlin
  :init
  (add-to-list 'company-backends 'merlin-company-backend))


(provide 'j0ni-complete)
