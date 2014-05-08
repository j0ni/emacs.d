;;; j0ni-python.el -- Python configuration

;; Require, not autoload, to override Emacs bundled python.el
(packages-require
 '(python-mode
   virtualenv))

;; Pytest bindings
(package-require 'pytest)
(add-hook
 'python-mode-hook
 (lambda ()
   (define-key python-mode-map (kbd "C-c C-,") 'pytest-run-file)))

(package-require 'py-autopep8)
(add-hook 'before-save-hook 'py-autopep8-before-save)

(package-require 'smartparens)
(add-hook 'python-mode-hook 'turn-on-smartparens-mode)

(package-require 'elpy)
(elpy-enable)
(elpy-clean-modeline)

(delete 'highlight-indentation-mode elpy-default-minor-modes)
(delete 'auto-complete-mode elpy-default-minor-modes)


(provide 'j0ni-python)
