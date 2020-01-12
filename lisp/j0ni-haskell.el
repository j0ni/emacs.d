;;; j0ni-haskell.el

(use-package lsp-haskell)
(use-package haskell-mode
  :commands haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook #'lsp)
  (setq haskell-mode-stylish-haskell-path "brittany"))

(use-package lsp-haskell)
;; Put ghc-show-info in a popup
;; (use-package popup)
;; (defun ghc-show-info-popup ()
;;   (interactive)
;;   (popup-tip (ghc-get-info (ghc-things-at-point))
;;              :around t :scroll-bar t))

;; (define-key haskell-mode-map (kbd "C-c TAB") 'ghc-show-info-popup)
;; (define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info-popup)
;; (define-key haskell-mode-map (kbd "C-c C-S-i") 'ghc-show-info)

;; Use standard keybinding for inferior-haskell-find-definition
;; (define-key haskell-mode-map (kbd "M-.")
;;   (lambda () (interactive)
;;     (inferior-haskell-find-definition (haskell-ident-at-point))))

;; Run test suite
(defun haskell-mode-run-test-suite ()
  (interactive)
  (require 'compile)
  (compile (concat "cd " (projectile-project-root) "; cabal test")))
;; (define-key haskell-mode-map (kbd "C-c C-,") 'haskell-mode-run-test-suite)

;; Flycheck addons
;; (package-require 'flycheck-haskell)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;; Idris (for want of a better place to put it)
;; (use-package idris-mode)
;; (add-to-list 'auto-mode-alist '("\\.idr$" . idris-mode))

(provide 'j0ni-haskell)
