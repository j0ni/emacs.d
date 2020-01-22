;;; j0ni-python.el -- Python configuration

;; for AGL - gotta find a better place for this
;; (setenv "AGL_ENV" "local")

;; (packages-require
;;  '(virtualenv
;;    pytest
;;    pyenv-mode))

;; (use-package lispy)
;; (require 'lpy)
;; (add-hook 'python-mode-hook 'lpy-mode)

;; (use-package virtualenv)
(use-package pytest)
(use-package pyvenv
  :commands (pyvenv-activate pyvenv-workon))
;; global mode for picking up local `pyvenv-workon` settings
;; (pyvenv-mode 1)
;; (use-package pyenv-mode)

;; (use-package elpy
;;   :init
;;   (elpy-enable)
;;   (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)))

(require 'python)
(add-hook 'python-mode-hook #'lsp)

(use-package blacken
  :hook (python-mode . blacken-mode))

;; (use-package anaconda-mode
;;   :init
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

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

(add-hook 'python-mode-hook #'turn-on-smartparens-mode)
(add-hook 'inferior-python-mode-hook #'turn-on-smartparens-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

;; (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

(use-package conda
  :init
  (setq conda-anaconda-home "~/.conda")
  (setq-default conda-project-env-name "userbase")
  :config
  ;; (conda-env-autoactivate-mode t)
  )

(add-hook 'python-mode-hook (lambda ()
                              (setq lsp-pyls-plugins-yapf-enabled nil)
                              (lsp)))

(provide 'j0ni-python)
