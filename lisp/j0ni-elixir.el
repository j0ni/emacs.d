;;; j0ni-elixir.el -- Elixir configuration

(packages-require '(elixir-mode
                    alchemist))

(setq alchemist-mix-command "/usr/bin/mix")
(setq alchemist-iex-program-name "/usr/bin/iex")
(setq alchemist-execute-command "/usr/bin/elixir")
(setq alchemist-compile-command "/usr/bin/elixirc")

(setq alchemist-hooks-test-on-save t)
(setq alchemist-hooks-compile-on-save t)

(provide 'j0ni-elixir)
