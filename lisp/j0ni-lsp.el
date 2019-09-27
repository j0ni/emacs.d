;;; j0ni-lsp.el -- Language server support

;; (use-package eglot
;;   :commands (eglot-ensure)

;;   ;; :init
;;   ;; (add-hook 'rust-mode-hook 'eglot-ensure)

;;   :config
;;   (setq eglot-put-doc-in-help-buffer nil)
;;   (setq eglot-auto-display-help-buffer t)
;;   )

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
  (setq lsp-auto-configure t)
  (setq lsp-enable-xref t)
  (setq lsp-prefer-flymake t)
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-signature-render-all t)
  )

;; (use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
;; (use-package dap-mode)

(provide 'j0ni-lsp)
