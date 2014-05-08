;;; j0ni-go.el

(packages-require
 '(go-mode
   go-direx
   go-eldoc
   go-errcheck
   go-play
   go-snippets))

(dolist (pc j0ni-go-path)
  (setenv "GOPATH" (concat (concat pc ":") (getenv "GOPATH"))))

;; Use Brad Fitz's better version of gofmt which fixes imports
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(package-require 'smartparens)
(add-hook 'go-mode-hook 'smartparens-mode)

(package-require 'flycheck)
(add-hook 'go-mode-hook 'flycheck-mode)

(provide 'j0ni-go)
