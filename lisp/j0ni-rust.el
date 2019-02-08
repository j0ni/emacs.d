;;; j0ni-rust.el -- configuration for the rust programming language

(require 'subr-x)

(use-package toml-mode)

(defvar j0ni-rust-root
  (string-trim
   (with-output-to-string
     (shell-command "rustc --print sysroot" standard-output))))

(setenv "LD_LIBRARY_PATH" (concat j0ni-rust-root "/lib"))
(setenv "RUST_SRC_PATH" (concat j0ni-rust-root "/lib/rustlib/src/rust/src"))

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(use-package rust-mode
  :init
  (setq company-tooltip-align-annotations t)
  (setq rust-format-on-save t)
  ;; (setq comment-auto-fill-only-comments t)
  ;; (setq rust-match-angle-brackets nil)
  (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

  :config
  (defun rust-mode-setup ()
    (electric-pair-mode 1)
    (electric-layout-mode 1)
    (flycheck-mode 1))
  (add-hook 'rust-mode-hook 'rust-mode-setup))

(use-package racer
  :hook (rust-mode . racer-mode)

  :init
  (setq racer-rust-src-path (concat j0ni-rust-root "/lib/rustlib/src/rust/src"))

  :config
  (defun racer-mode-setup ()
    (eldoc-mode 1)
    (company-mode 1))
  (add-hook 'racer-mode-hook 'racer-mode-setup)

  :bind (("C-c C-d" . racer-describe)))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package company-racer
  :config
  (add-to-list 'company-backends 'company-racer))

(provide 'j0ni-rust)
