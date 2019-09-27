;;; j0ni-evil.el -- Evil-mode setup

;; (use-package evil-cleverparens
;;   :commands evil-cleverparens-mode
;;   :init
;;   (setq evil-move-beyond-eol t)
;;   :hook ((paredit-mode . evil-cleverparens-mode)
;;          (lispy-mode . evil-cleverparens-mode)))

(use-package evil)

(use-package evil-smartparens
  :after (:all smartparens evil)
  :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(provide 'j0ni-evil)
