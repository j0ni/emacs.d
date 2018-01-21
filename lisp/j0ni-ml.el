;;; j0ni-ml.el --- ocaml, reasonml etc

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; reason-mode
;; (quelpa '(reason-mode :repo "reasonml-editor/reason-mode" :fetcher github :stable t))

;;----------------------------------------------------------------------------
;; Reason setup
;;----------------------------------------------------------------------------

;; (defun shell-cmd (cmd)
;;   "Returns the stdout output of a shell command or nil if the
;;    command returned an error"
;;   (car (ignore-errors (apply 'process-lines (split-string cmd)))))

;; (let* ((refmt-bin (or (shell-cmd "refmt ----where")
;;                       (shell-cmd "which refmt")))
;;        (merlin-bin (or (shell-cmd "ocamlmerlin ----where")
;;                        (shell-cmd "which ocamlmerlin")))
;;        (merlin-base-dir (when merlin-bin
;;                           (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
;;   ;; Add npm merlin.el to the emacs load path and tell emacs where to find
;;   ;; ocamlmerlin
;;   (when merlin-bin
;;     (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
;;     (setq merlin-command merlin-bin))

;;   (when refmt-bin
;;     (setq refmt-command refmt-bin)))

;; (require 'reason-mode)
;; (require 'merlin)
;; (add-hook 'reason-mode-hook (lambda ()
;;                               (add-hook 'before-save-hook 'refmt-before-save)
;;                               (merlin-mode)))

;; (setq merlin-ac-setup nil)
;; ;; (setq merlin-ac-setup t)

(package-require 'utop)

;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")

;; (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
;; (add-hook 'tuareg-mode-hook #'utop-minor-mode)
;; (add-hook 'reason-mode-hook #'utop-minor-mode)
(setq tuareg-indent-align-with-first-arg nil)
(setq tuareg-match-patterns-aligned t)

(provide 'j0ni-ml)
