;;; j0ni-python.el -- Python configuration

(packages-require
 '(virtualenv
   pytest
   pyenv-mode
   ein
   ob-ipython
   anaconda-mode))

(require 'python)

(add-hook 'python-mode-hook 'pyenv-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(add-to-list 'python-shell-extra-pythonpaths "/home/joni/Scratch/TheGuild/AGLP2P/SAMpy")

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

(defun my-anaconda-mode-hook ()
  (define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-go-back)
  (define-key anaconda-mode-map (kbd "M-*") 'anaconda-mode-find-assignments))

(add-hook 'anaconda-mode-hook 'my-anaconda-mode-hook)

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
(add-hook 'inferior-python-mode-hook 'turn-on-smartparens-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)


(provide 'j0ni-python)
