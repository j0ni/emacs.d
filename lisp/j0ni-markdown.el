;;; j0ni-markdown.el -- Markdown setup

(use-package markdown-mode
  :init
  (setq-default markdown-command "pandoc -s --self-contained -f markdown+smart -t html5 ")
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook #'visual-line-mode))

(use-package markdown-toc)

(provide 'j0ni-markdown)
