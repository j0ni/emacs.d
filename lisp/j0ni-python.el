;;; j0ni-python.el -- Python configuration

;; for AGL - gotta find a better place for this
(setenv "AGL_ENV" "local")

(packages-require
 '(virtualenv
   pytest
   pyenv-mode
   ein
   ob-ipython
   anaconda-mode))

(require 'python)

(setq python-check-command "pylint")
(setq python-fill-docstring-style 'pep-257-nn)

(add-hook 'python-mode-hook 'pyenv-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(add-to-list 'python-shell-extra-pythonpaths "/home/joni/Scratch/TheGuild/AGLP2P/SAMpy")

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

;; (add-hook 'python-mode-hook #'flycheck-virtualenv-setup)
;; also advise the pyenv-mode-set fn
;; (advice-add 'pyenv-mode-set :after #'flycheck-virtualenv-setup)

;; (defun set-flychecker-executables ()
;;   "Configure virtualenv for flake8 and lint."
;;   ;; (when (get-current-buffer-flake8)
;;   ;;   (flycheck-set-checker-executable (quote python-flake8)
;;   ;;                                    (get-current-buffer-flake8)))
;;   (when (get-current-buffer-pylint)
;;     (flycheck-set-checker-executable (quote python-pylint)
;;                                      (get-current-buffer-pylint))))

;; (add-hook 'flycheck-before-syntax-check-hook
;;           #'set-flychecker-executables 'local)

(defun my-anaconda-mode-hook ()
  ;; (flycheck-mode +1)
  (define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-go-back)
  (define-key anaconda-mode-map (kbd "M-*") 'anaconda-mode-find-assignments))

(add-hook 'anaconda-mode-hook 'my-anaconda-mode-hook)

(package-require 'py-autopep8)

;; (defun my-python-mode-hook ()
;;   (define-key python-mode-map (kbd "C-c C-,") 'pytest-run-file)
;;   (setq indent-tabs-mode nil
;;         py-smart-indentation nil
;;         py-indent-offset 4)
;;   ;; (add-hook 'before-save-hook 'py-autopep8-before-save)
;;   (flycheck-mode 1))

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

(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(add-hook 'ein:notebook-mode-hook 'turn-on-smartparens-mode)

(provide 'j0ni-python)
