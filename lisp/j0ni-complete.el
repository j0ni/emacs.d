;;; j0ni-complete.el -- Auto completion

(use-package company
  :bind (("M-\\" . company-complete))
  :commands (global-company-mode)
  :diminish nil
  :config
  (global-company-mode)
  :init
  (setq company-global-modes '(not term-mode org-mode))
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers nil
        company-tooltip-align-annotations t
        company-tooltip-idle-delay 0.5
        company-idle-delay 0.5
        ;; company-tooltip-idle-delay nil
        ;; company-idle-delay nil
        company-selection-wrap-around t
        company-auto-complete 'company-explicit-action-p
        ;; company-frontends '()
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-if-just-one-frontend
                            company-echo-metadata-frontend)))

;; Javascript
(use-package company-tern
  :after (company)
  :init
  (add-to-list 'company-backends 'company-tern))

;; Go Lang
(use-package company-go
  :after (company)
  :init
  (add-to-list 'company-backends 'company-go))

;; Ruby
(use-package company-inf-ruby
  :after (company)
  :init
  (add-to-list 'company-backends 'company-inf-ruby))

;; Haskell
(use-package company-ghc
  :after (company)
  :init
  (add-to-list 'company-backends 'company-ghc))

(use-package merlin
  :after (company)
  :init
  (add-to-list 'company-backends 'merlin-company-backend))

(provide 'j0ni-complete)
