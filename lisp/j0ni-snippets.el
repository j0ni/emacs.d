;;; j0ni-snippets.el --- Yasnippets

(package-require 'popup)
(package-require 'yasnippet)
(require 'yasnippet)
(setq yas-snippet-dirs (concat dotfiles-dir "snippets"))
;; (yas-global-mode 1)

;; Load Magnar's snippet helpers
(require 'snippet-helpers)
(require 'buster-snippet-helpers)

(provide 'j0ni-snippets)
