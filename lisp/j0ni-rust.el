;;; j0ni-rust.el -- configuration for the rust programming language

(packages-require '(rust-mode
                    racer
                    company
                    company-racer
                    flycheck
                    flycheck-rust
                    rustfmt))

;; Set path to racer binary
;; (setq racer-cmd (concat-home
;; "Scratch/rust/racer/target/release/racer"))

(setenv "RUST_SRC_PATH" (concat-home ".rust/src"))

;; Set path to rust src directory
;; (setq racer-rust-src-path (concat-home ".rust/src"))

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;; (defun rust-mode-setup ()
;;   (racer-activate)
;;   (racer-turn-on-eldoc)
;;   )

(defun rust-mode-setup ()
  (interactive)
  (racer-mode 1)
  (smartparens-mode 1)
  (define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer)
  (define-key rust-mode-map (kbd "M-q") #'rustfmt-format-buffer))

(add-hook 'rust-mode-hook 'rust-mode-setup)

(defun racer-mode-setup ()
  (eldoc-mode 1)
  (company-mode 1))

(add-hook 'racer-mode-hook 'racer-mode-setup)

;; rustfmt is in its infancy - there's a lot of discussion about
;; specific brokenness here:
;;   https://users.rust-lang.org/t/try-rustfmt-on-your-code/3588
;; when its settled, we can have it run like gofmt

;; (add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

(provide 'j0ni-rust)
