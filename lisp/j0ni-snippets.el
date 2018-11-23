;;; j0ni-snippets.el --- Yasnippets

;; (package-require 'popup)
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs (list (concat dotfiles-dir "snippets")))
  (require 'snippet-helpers)
  :config (yas-reload-all))

(defun yasnippet-setup ()
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package aws-snippets
  :ensure t
  :after yasnippet)

(use-package buster-snippets
  :ensure t
  :after yasnippet)

(use-package clojure-snippets
  :defer t
  :after yasnippet)

(use-package common-lisp-snippets
  :defer t
  :after yasnippet)

(use-package go-snippets
  :defer t
  :after yasnippet)

(use-package haskell-snippets
  :defer t
  :after yasnippet)

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(yasnippet-setup)

(provide 'j0ni-snippets)
