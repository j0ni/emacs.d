;;; j0ni-julia.el

(packages-require '(julia-mode julia-shell))

(defun my-julia-mode-setup ()
  (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
  (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go))

(add-hook 'julia-mode-hook 'my-julia-mode-setup)

(provide 'j0ni-julia)
