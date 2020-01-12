;;; j0ni-rust.el -- configuration for the rust programming language

(require 'subr-x)

(use-package toml-mode)

(use-package flycheck-rust
  :commands (flycheck-rust-setup))

(use-package cargo
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode)

  :commands (cargo-minor-mode))

(defun get-rust-root ()
  (string-trim
   (with-output-to-string
     (shell-command "rustc --print sysroot" standard-output))))

(defvar j0ni-rust-root)
(defun set-rust-root ()
  (interactive)
  (setq j0ni-rust-root (get-rust-root)))

(use-package rust-mode
  :init
  (setq company-tooltip-align-annotations t)
  (setq rust-format-on-save t)
  (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

  :config
  ;; (add-hook 'rust-mode-hook #'eglot-ensure)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'electric-pair-mode)
  (add-hook 'rust-mode-hook #'electric-layout-mode)
  (add-hook 'rust-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'rust-mode-hook #'set-rust-root)
  )

;; (use-package racer
;;   :commands racer-mode
;;   :init
;;   (setenv "CARGO_HOME" (concat-home ".cargo"))
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   :config
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   (add-hook 'racer-mode-hook #'company-mode)

;;   :bind (("C-c C-d" . racer-describe)))

;; (use-package company-racer
;;   :config
;;   (add-to-list 'company-backends 'company-racer))

(provide 'j0ni-rust)
