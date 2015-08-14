;;; bodil-lisp.el -- Lisps

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

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(packages-require '(elisp-slime-nav diminish))
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;;; Clojure

(packages-require '(align-cljlet cider-profile clj-refactor))
(setq clojure-defun-style-default-indent nil)

(add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

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
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(eval-after-load 'clojure-mode
  '(progn
     ;; Make compojure routes look nice
     (define-key clojure-mode-map (kbd "C-M-z") 'align-cljlet)
     (define-clojure-indent
       (defroutes 'defun)
       (defrecord 'defun)
       (GET 2)
       (POST 2)
       (PUT 2)
       (DELETE 2)
       (HEAD 2)
       (ANY 2)
       (context 2))
     (put 'defrecord 'clojure-backtracking-indent '(4 4 (2)))

     ;; Treat hyphens as a word character when transposing words
     (defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
       (let ((st (make-syntax-table clojure-mode-syntax-table)))
         (modify-syntax-entry ?- "w" st)
         st))))

;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
;; inf-clojure, less bloated repl
;; (package-require 'inf-clojure)
(defun start-inf-clojure ()
  (interactive)
  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
  (inf-clojure))

;; nRepl/cider
(packages-require '(cider-eval-sexp-fu))

(eval-after-load 'clojure-mode
  ;; why?
  '(progn
     (require 'cider)
     (require 'cider-eval-sexp-fu)))

(setq cider-repl-use-clojure-font-lock t
      cider-show-error-buffer          nil
      cider-popup-stacktraces          nil
      cider-buffer-name-show-port      t
      cider-repl-history-size          10000
      cider-repl-result-prefix         "=> "
      cider-prompt-for-symbol          nil
      cider-known-endpoints            '(("circle-dev" "localhost" "6005")
                                         ("circle-staging" "localhost" "6002")
                                         ("circle-prod" "localhost" "6001"))
      cider-repl-history-file          (concat-home ".cider-repl-history")
      nrepl-buffer-name-show-port      t
      cider-words-of-inspiration       '(""))

(add-hook 'cider-mode-hook (lambda ()
                             (cider-turn-on-eldoc-mode)
                             (diminish 'eldoc-mode)
                             (diminish 'cider-mode)))

;; Run tests in nRepl
(defun nrepl-run-tests (ns)
  (interactive (list (nrepl-current-ns)))
  (save-buffer)
  (nrepl-load-current-buffer)
  (with-current-buffer "*nrepl*"
    (nrepl-send-string
     (format "(clojure.test/run-tests '%s)" ns)
     nrepl-buffer-ns (nrepl-handler (current-buffer)))))

(eval-after-load "clojure-mode"
  '(define-key clojure-mode-map (kbd "C-c C-,") 'nrepl-run-tests))

(defun custom-cider-repl-bindings ()
  (interactive)
  (define-key cider-repl-mode-map (kbd "C-c C-M-r") 'cider-repl-previous-matching-input)
  (define-key cider-repl-mode-map (kbd "C-c C-M-s") 'cider-repl-next-matching-input)
  (subword-mode 1)
  (diminish 'subword-mode))

(add-hook 'cider-repl-mode-hook 'custom-cider-repl-bindings)

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
  (run-lisp "lein repl"))

;; ClojureScript REPL
(defun clojurescript-repl ()
  (interactive)
  (run-lisp "lein trampoline noderepl"))

;; ClojureScript REPL
(defun clojurescript-rhino-repl ()
  (interactive)
  (run-lisp "lein trampoline cljsbuild repl-rhino"))

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

;; (package-require 'racket-mode)

(provide 'j0ni-lisp)
