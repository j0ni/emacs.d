;;; j0ni-go.el

(packages-require
 '(go-mode
   go-direx
   go-eldoc
   go-errcheck
   ;; go-play
   go-snippets
   smartparens
   flycheck))

(setenv "GOPATH" (mapconcat 'identity j0ni-go-path ":"))

;; Use Brad Fitz's better version of gofmt which fixes imports
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'godef-jump)
            (smartparens-mode 1)
            (flycheck-mode 1)
            (go-eldoc-setup)))

(provide 'j0ni-go)
