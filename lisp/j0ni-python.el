;;; j0ni-python.el -- Python configuration

(packages-require
 '(virtualenv
   pytest))

(defun my-python-mode-hook ()
  (define-key python-mode-map (kbd "C-c C-,") 'pytest-run-file)
  (setq indent-tabs-mode nil
        py-smart-indentation nil
        py-indent-offset 4))

(add-hook 'python-mode-hook 'my-python-mode-hook)

(package-require 'py-autopep8)
(add-hook 'before-save-hook 'py-autopep8-before-save)

(package-require 'smartparens)
(add-hook 'python-mode-hook 'turn-on-smartparens-mode)


(provide 'j0ni-python)
