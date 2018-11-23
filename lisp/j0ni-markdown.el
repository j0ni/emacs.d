;;; j0ni-markdown.el -- Markdown setup

(use-package markdown-mode
  :ensure t
  :init
  (setq-default markdown-command "pandoc -s --self-contained -f markdown+smart -t html5 ")
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode)))

(use-package markdown-toc
  :ensure t)

(provide 'j0ni-markdown)
