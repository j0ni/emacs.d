;;; j0ni-erlang.el --- Erlang configuration

(packages-require '(erlang
                    company-erlang
                    ivy-erlang-complete
                    flycheck-rebar3))

(defun erlang-mode-setup ()
  (smartparens-mode 1)
  (flycheck-mode 1))

;; not a fan of `erlang-electric-gt`
(setq erlang-electric-commands '(erlang-electric-comma
                                 erlang-electric-semicolon
                                 erlang-electric-newline))

(add-hook 'erlang-mode-hook 'erlang-mode-setup)

(provide 'j0ni-erlang)
