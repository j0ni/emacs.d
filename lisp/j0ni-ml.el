;;; j0ni-ml.el --- ocaml, reasonml etc

(add-to-list 'load-path "/home/joni/.opam/default/share/emacs/site-lisp/")
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")

(setq tuareg-indent-align-with-first-arg nil)
(setq tuareg-match-patterns-aligned t)

(provide 'j0ni-ml)
