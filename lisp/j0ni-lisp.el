;;; j0ni-lisp.el -- Lisps

;; initially copied from bodil's emacs config

(require 'j0ni-defuns)

(packages-require '(clojure-mode
                    cider
                    inf-clojure
                    racket-mode))

(setq j0ni-lisp-modes '(scheme-mode
                        emacs-lisp-mode
                        lisp-mode
                        clojure-mode
                        cider-repl-mode
                        inf-clojure-mode
                        racket-mode))

(defun add-lisp-hook (func)
  (add-hooks j0ni-lisp-modes func))

;; Setup C-c v to eval whole buffer in all lisps
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)
(define-key lisp-mode-shared-map (kbd "C-c C-v") 'eval-buffer)

(package-require 'highlight-parentheses)
;; (add-lisp-hook 'highlight-parentheses-mode)

;; Make em light up
(require 'highlight)
(packages-require '(eval-sexp-fu hl-sexp))
(require 'eval-sexp-fu)
;; Highlight sexp under cursor
;; (add-lisp-hook 'hl-sexp-mode)


;; Paredit for all lisps
(packages-require '(paredit diminish))

(eval-after-load 'paredit
  '(progn
     (defun paredit-barf-all-the-way-backward ()
       (interactive)
       (paredit-split-sexp)
       (paredit-backward-down)
       (paredit-splice-sexp))

     (defun paredit-barf-all-the-way-forward ()
       (interactive)
       (paredit-split-sexp)
       (paredit-forward-down)
       (paredit-splice-sexp)
       (if (eolp) (delete-horizontal-space)))

     (defun paredit-slurp-all-the-way-backward ()
       (interactive)
       (catch 'done
         (while (not (bobp))
           (save-excursion
             (paredit-backward-up)
             (if (eq (char-before) ?\()
                 (throw 'done t)))
           (paredit-backward-slurp-sexp))))

     (defun paredit-slurp-all-the-way-forward ()
       (interactive)
       (catch 'done
         (while (not (eobp))
           (save-excursion
             (paredit-forward-up)
             (if (eq (char-after) ?\))
                 (throw 'done t)))
           (paredit-forward-slurp-sexp))))

     (nconc paredit-commands
            '("Extreme Barfage & Slurpage"
              (("C-M-)")
               paredit-slurp-all-the-way-forward
               ("(foo (bar |baz) quux zot)"
                "(foo (bar |baz quux zot))")
               ("(a b ((c| d)) e f)"
                "(a b ((c| d)) e f)"))
              (("C-M-}" "M-F")
               paredit-barf-all-the-way-forward
               ("(foo (bar |baz quux) zot)"
                "(foo (bar|) baz quux zot)"))
              (("C-M-(")
               paredit-slurp-all-the-way-backward
               ("(foo bar (baz| quux) zot)"
                "((foo bar baz| quux) zot)")
               ("(a b ((c| d)) e f)"
                "(a b ((c| d)) e f)"))
              (("C-M-{" "M-B")
               paredit-barf-all-the-way-backward
               ("(foo (bar baz |quux) zot)"
                "(foo bar baz (|quux) zot)"))))

     (paredit-define-keys)
     (paredit-annotate-mode-with-examples)
     (paredit-annotate-functions-with-examples)))

(defun enable-paren-handling ()
  (when (fboundp 'autopair-mode)
    (autopair-mode -1))
  (paredit-mode 1)
  (diminish 'paredit-mode "par"))

(add-lisp-hook 'enable-paren-handling)

;; Make paredit play nice with eldoc
(eval-after-load "eldoc"
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round))

;; Rainbow delimiters
(package-require 'rainbow-delimiters)
;; (add-lisp-hook 'rainbow-delimiters-mode)

;; Lambdas
;; (defun lambda-as-lambda (mode pattern)
;;   (font-lock-add-keywords
;;    mode `((,pattern
;;            (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                      "λ" 'decompose-region)))))))

;;; Emacs Lisp

;; (lambda-as-lambda 'emacs-lisp-mode "(\\(\\<lambda\\>\\)")

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

;; (define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(packages-require '(elisp-slime-nav diminish))

(defun elisp-slime-nav-mode-setup ()
  (elisp-slime-nav-mode 1))

(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode-setup)
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;;; Clojure

(packages-require '(align-cljlet cider-profile clj-refactor))
(setq clojure-defun-style-default-indent nil)

;; (add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
;; (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "λ")
;;                                nil))))))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("\\(#\\)("
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "ƒ")
;;                                nil))))))

;; (eval-after-load 'clojure-mode
;;   '(font-lock-add-keywords
;;     'clojure-mode `(("\\(#\\){"
;;                      (0 (progn (compose-region (match-beginning 1)
;;                                                (match-end 1) "∈")
;;                                nil))))))

;; (lambda-as-lambda 'clojure-mode "(\\(\\<fn\\>\\)")

(defun my-clojure-mode-hook ()
  (yas-minor-mode 1)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  )

;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(eval-after-load 'clojure-mode
  '(progn
     ;; Make compojure routes look nice
     (define-key clojure-mode-map (kbd "C-M-z") 'align-cljlet)
     (define-clojure-indent
       (defroutes 'defun)
       ;; (defrecord 'defun)
       (GET 2)
       (POST 2)
       (PUT 2)
       (DELETE 2)
       (HEAD 2)
       (ANY 2)
       (context 2)
       (GET* 2)
       (POST* 2)
       (PUT* 2)
       (DELETE* 2)
       (HEAD* 2)
       (ANY* 2)
       (query 2)
       (insert! 2)
       (update! 2)
       (delete! 2))

     ;; I can't make my mind up about this. It makes sense
     ;; semantically, but it looks shit when the first value is an
     ;; expression spanning multiple lines. Maybe that's supposed to
     ;; guide better practice?

     (put-clojure-indent '-> 1)
     (put-clojure-indent '->> 1)
     (put-clojure-indent 'some-> 1)
     (put-clojure-indent 'some->> 1)

     ;; Treat hyphens as a word character when transposing words
     (defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
       (let ((st (make-syntax-table clojure-mode-syntax-table)))
         (modify-syntax-entry ?- "w" st)
         st))))

;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
;; inf-clojure, less bloated repl
;; (package-require 'inf-clojure)
;; FIXME I'm not sure if it's clearer to have the package requires at
;; the top or inline like ^^.
;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; nRepl/cider
(packages-require '(cider-eval-sexp-fu))

;; (eval-after-load 'clojure-mode
;;   ;; why?
;;   '(progn
;;      (require 'cider)
;;      (require 'cider-eval-sexp-fu)))

(setq cider-repl-pop-to-buffer-on-connect t
      cider-repl-use-clojure-font-lock    t
      cider-use-overlays                  t ; display eval results inline
      cider-overlays-use-font-lock        t ; font lock the results
      cider-show-error-buffer             t
      cider-popup-stacktraces             t
      cider-buffer-name-show-port         t
      cider-repl-history-size             10000
      cider-repl-result-prefix            "=> "
      cider-prompt-for-symbol             nil
      cider-known-endpoints               '(("circle-dev" "localhost" "6005")
                                            ("circle-staging" "localhost" "6002")
                                            ("circle-prod" "localhost" "6001"))
      cider-repl-history-file             (concat-home ".cider-repl-history")
      nrepl-buffer-name-show-port         t
      cider-words-of-inspiration          '(""))

(add-hook 'cider-mode-hook (lambda ()
                             (require 'cider-eval-sexp-fu)
                             (cider-turn-on-eldoc-mode)
                             (diminish 'eldoc-mode)
                             (diminish 'cider-mode)))

;; Run tests in nRepl
(defun nrepl-run-tests (ns)
  (interactive (list (nrepl-current-ns)))
  (save-buffer)
  (nrepl-load-current-buffer)
  (with-current-buffer "*nrepl*"
    (nrepl-send-string (format "(clojure.test/run-tests '%s)" ns)
                       nrepl-buffer-ns
                       (nrepl-handler (current-buffer)))))

(eval-after-load "clojure-mode"
  '(define-key clojure-mode-map (kbd "C-c C-,") 'nrepl-run-tests))

(defun cider-repl-customizations ()
  (interactive)
  (define-key cider-repl-mode-map (kbd "C-r") 'cider-repl-previous-matching-input)
  (define-key cider-repl-mode-map (kbd "C-s") 'cider-repl-next-matching-input)
  (define-key cider-repl-mode-map (kbd "C-c C-M-r") 'isearch-backward-regexp)
  (define-key cider-repl-mode-map (kbd "C-c C-M-s") 'isearch-forward-regexp)
  (subword-mode 1)
  (diminish 'subword-mode))

(add-hook 'cider-repl-mode-hook 'cider-repl-customizations)

;; Let's try to get eval-sexp sending to the REPL

(defun my-eval-defun-in-repl (&optional prefix)
  (interactive "P")
  (save-current-buffer
    (let ((start-pos (point))
          (form (cider-defun-at-point)))
      (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
        (setq form (replace-match "" t t form)))
      (with-current-buffer (cider-current-repl-buffer)
        (end-of-buffer)
        (newline)
        (insert form)
        (indent-region start-pos (point))
        (unless prefix
          (cider-repl-return))))
    (when prefix
      (cider-switch-to-current-repl-buffer))))

;; (define-key cider-mode-map (kbd "C-c C-c") 'cider-eval-defun-in-repl)

;; (defun custom-cider-bindings ()
;;   (interactive)
;;   (define-key cider-mode-map (kbd "C-c C-c") 'my-eval-defun-in-repl)
;;   ;; (define-key cider-mode-map (kbd "C-M-x") 'my-eval-defun-in-repl)
;;   )

;; (add-hook 'cider-mode-hook 'custom-cider-bindings)

;; Kibit
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

;; Cljsbuild
(package-require 'cljsbuild-mode)

;; clj-refactor

;; TODO implement cljr-magic-require-namespaces
;; (dolist (mapping '(("user" . "circle.model.user")
;;                    ("project" . "circle.model.project")
;;                    ("plan" . "circle.model.plan")
;;                    ("build" . "circle.model.build")
;;                    ("org" . "circle.model.organization")
;;                    ("settings" . "circle.model.settings")))
;;   (add-to-list 'cljr-magic-require-namespaces mapping t))

(setq cljr-favor-prefix-notation nil
      cljr-favor-private-functions nil)

;; LispyScript

(add-to-list 'auto-mode-alist '("\\.ls$" . clojure-mode))


;;; Various inferior lisps

;; Clojure REPL
(defun clojure-repl ()
  (interactive)
  (run-clojure "lein repl"))

;; ClojureScript REPL
(defun clojurescript-repl ()
  (interactive)
  (run-clojure "lein trampoline noderepl"))

;; ClojureScript REPL
(defun clojurescript-rhino-repl ()
  (interactive)
  (run-clojure "lein trampoline cljsbuild repl-rhino"))

;; Figwheel REPL
(defun figwheel-repl ()
  (interactive)
  (run-clojure "lein figwheel"))

;; Switch a Clojure nrepl to ClojureScript

(defun nrepl-start-noderepl ()
  (interactive)
  (save-excursion
    (nrepl-switch-to-repl-buffer nil)
    (insert "(require 'cljs.repl.node) (cljs.repl.node/run-node-nrepl)")
    (nrepl-send-input)))

;; Slime for common lisp

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(slime-setup '(slime-fancy slime-asdf slime-tramp slime-banner))

(defun slime-repl-mode-custom ()
  (paredit-mode t)
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'slime-repl-mode-custom)

;; Racket

(package-require 'racket-mode)
(setq racket-racket-program "/Applications/Racket v6.3/bin/racket"
      racket-raco-program "/Applications/Racket v6.3/bin/raco"
      racket-smart-open-bracket-enable t)

(defun my-racket-mode-hook ()
  (define-key racket-mode-map (kbd "C-M-z") 'racket-align))

(defun my-racket-repl-mode-hook ()
  (enable-paren-handling))

(add-hook 'racket-mode-hook 'my-racket-mode-hook)
(add-hook 'racket-repl-mode-hook 'my-racket-repl-mode-hook)

(provide 'j0ni-lisp)
