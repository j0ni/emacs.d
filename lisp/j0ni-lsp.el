;;; j0ni-lsp.el -- Language server support

(use-package eglot
  :defer t
  :commands (eglot-ensure))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-enable-indentation nil)
  (setq lsp-auto-configure t)
  (setq lsp-enable-xref t)
  (setq lsp-enable-snippet nil)
  (setq lsp-prefer-flymake t)
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-signature-render-all t))

(use-package company-lsp
  :after (company)
  :commands company-lsp
  :init (push 'company-lsp company-backends))

(use-package lsp-ivy)
(use-package dap-mode)

(provide 'j0ni-lsp)
