;;; j0ni-python.el -- Python configuration

;; for AGL - gotta find a better place for this
(setenv "AGL_ENV" "local")

(packages-require
 '(virtualenv
   pytest
   pyenv-mode
   ein
   ob-ipython
   anaconda-mode
   conda))

(require 'python)

(use-package blacken
  :hook (python-mode . blacken-mode))

(setq python-check-command "pylint")
(setq python-fill-docstring-style 'pep-257-nn)

;; (add-hook 'python-mode-hook 'pyenv-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; (add-to-list 'python-shell-extra-pythonpaths "/home/joni/Scratch/TheGuild/AGLP2P/SAMpy")

;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "--classic --profile=emacs -i")
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")

;; this only gets called in python-mode, so we're good
(declare-function python-shell-calculate-exec-path "python")

(defun flycheck-virtualenv-executable-find (executable)
  "Find an EXECUTABLE in the current virtualenv if any."
  (if (bound-and-true-p python-shell-virtualenv-root)
      (let ((exec-path (python-shell-calculate-exec-path)))
        (executable-find executable))
    (executable-find executable)))

(defun flycheck-virtualenv-setup (&rest args)
  "Setup Flycheck for the current virtualenv."
  (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find))

;; (use-package py-autopep8
;;   :init
;;   (setq py-autopep8-options '("--max-line-length=120"))
;;   :hook (python-mode . py-autopep8-enable-on-save))

(package-require 'smartparens)
(add-hook 'python-mode-hook 'turn-on-smartparens-mode)
(add-hook 'inferior-python-mode-hook 'turn-on-smartparens-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

;; (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(add-hook 'ein:notebook-mode-hook 'turn-on-smartparens-mode)

;; switch these on to work with python
(setq conda-anaconda-home "/home/joni/miniconda3")
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
;; (conda-env-autoactivate-mode t)

(add-hook 'anaconda-mode-hook 'conda-env-autoactivate-mode)

(provide 'j0ni-python)
