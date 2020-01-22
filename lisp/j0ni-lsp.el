;;; j0ni-lsp.el -- Language server support

(use-package eglot
  :commands (eglot-ensure)

  ;; :init
  ;; (add-hook 'rust-mode-hook 'eglot-ensure)

  ;; :init
  ;; (setq eglot-put-doc-in-help-buffer nil)
  ;; (setq eglot-auto-display-help-buffer t)
  )

;; (use-package eldoc-box
;;   :after (eglot)
;;   :diminish nil

;;   :commands (eldoc-box-hover-mode
;;              eldoc-box-eglot-help-at-point)

;;   :config
;;   (setq eldoc-box-max-pixel-width 1600)
;;   (setq eldoc-box-max-pixel-height 1000)
;;   (setq eldoc-box-cleanup-interval 1)
;;   (setq eldoc-box-only-multiline t)

;;   :init
;;   ;; (defun turn-on-eldoc-box-hover-mode ()
;;   ;;   (interactive)
;;   ;;   (eldoc-box-hover-mode t)
;;   ;;   (diminish 'eldoc-box-hover-mode))
;;   ;; (diminish 'eldoc-box-hover-mode nil)
;;   ;; (add-hook 'intero-mode-hook 'turn-on-eldoc-box-hover-mode)
;;   ;; (add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-mode t)
;;   )

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
  (setq lsp-signature-render-all t)
  )

;; (use-package lsp-ui :commands lsp-ui-mode :defer t)
(use-package company-lsp
  :commands company-lsp
  :init (push 'company-lsp company-backends))
(use-package lsp-ivy)
(use-package dap-mode)

;; languages
;; (use-package lsp-haskell)

(provide 'j0ni-lsp)
