;;; bodil-lisp.el -- Lisps

(require 'j0ni-defuns)

(setq j0ni-lisp-modes
      '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode cider-repl-mode))

(defun add-lisp-hook (func)
  (add-hooks j0ni-lisp-modes func))

;; Setup C-c v to eval whole buffer in all lisps
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

;; Highlight sexp under cursor
(package-require 'highlight-parentheses)
;; (add-lisp-hook 'highlight-parentheses-mode)

;; Make em light up
(require 'highlight)
(packages-require '(eval-sexp-fu hl-sexp))
(require 'eval-sexp-fu)
;; (add-lisp-hook 'hl-sexp-mode)


;; Paredit for all lisps
(packages-require '(paredit diminish))

(add-lisp-hook (lambda ()
                 (when (fboundp 'autopair-mode)
                   (autopair-mode -1))
                 (paredit-mode 1)
                 (diminish 'paredit-mode "Par")))

;; Make paredit play nice with eldoc
(eval-after-load "eldoc"
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round))

;; Rainbow delimiters
(package-require 'rainbow-delimiters)
(add-lisp-hook 'rainbow-delimiters-mode)

;; Lambdas
(defun lambda-as-lambda (mode pattern)
  (font-lock-add-keywords
   mode `((,pattern
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     "λ" 'decompose-region)))))))

;;; Emacs Lisp

(lambda-as-lambda 'emacs-lisp-mode "(\\(\\<lambda\\>\\)")

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
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;;; Clojure

(packages-require '(clojure-mode align-cljlet))
(setq clojure-defun-style-default-indent nil)

(add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

;; (lambda-as-lambda 'clojure-mode "(\\(\\<fn\\>\\)")

(eval-after-load 'clojure-mode
  '(progn
     ;; Make compojure routes look nice
     (define-key clojure-mode-map (kbd "C-M-z") 'align-cljlet)
     (define-clojure-indent
       (defroutes 'defun)
       (GET 2)
       (POST 2)
       (PUT 2)
       (DELETE 2)
       (HEAD 2)
       (ANY 2)
       (context 2))
     ;; Treat hyphens as a word character when transposing words
     (defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
       (let ((st (make-syntax-table clojure-mode-syntax-table)))
         (modify-syntax-entry ?- "w" st)
         st))))

;; nRepl/cider - pin it to the stable version
(package-require 'cider)
(eval-after-load "clojure-mode" '(require 'cider))
(setq nrepl-lein-command "lein"
      nrepl-server-command "echo \"lein repl :headless\" | $SHELL -l"
      cider-popup-stacktraces nil
      cider-buffer-name-show-port t
      cider-repl-history-size 10000
      cider-repl-history-file ".cider-repl-history")
(add-hook 'cider-mode-hook (lambda ()
                             (cider-turn-on-eldoc-mode)
                             (diminish 'eldoc-mode)
                             (diminish 'cider-mode)
                             (live-esf-initialize-cider)))

;; Stolen from Sam Aaron to get eval-sexp-fu working with clojure
(defun live-bounds-of-preceding-sexp ()
  "Return the bounds of sexp before the point. Copies semantics
   directly from the fn preceding-sexp to ensure highlighted area
   is identical to that which is evaluated."
  (let ((opoint (point))
        ignore-quotes
        expr)
    (save-excursion
      (with-syntax-table emacs-lisp-mode-syntax-table
        ;; If this sexp appears to be enclosed in `...'
        ;; then ignore the surrounding quotes.
        (setq ignore-quotes
              (or (eq (following-char) ?\')
                  (eq (preceding-char) ?\')))
        (forward-sexp -1)
        ;; If we were after `?\e' (or similar case),
        ;; use the whole thing, not just the `e'.
        (when (eq (preceding-char) ?\\)
          (forward-char -1)
          (when (eq (preceding-char) ??)
            (forward-char -1)))

        ;; Skip over hash table read syntax.
        (and (> (point) (1+ (point-min)))
             (looking-back "#s" (- (point) 2))
             (forward-char -2))

        ;; Skip over `#N='s.
        (when (eq (preceding-char) ?=)
          (let (labeled-p)
            (save-excursion
              (skip-chars-backward "0-9#=")
              (setq labeled-p (looking-at "\\(#[0-9]+=\\)+")))
            (when labeled-p
              (forward-sexp -1))))

        (save-restriction
          ;; vladimir@cs.ualberta.ca 30-Jul-1997: skip ` in
          ;; `variable' so that the value is returned, not the
          ;; name
          (if (and ignore-quotes
                   (eq (following-char) ?`))
              (forward-char))
          (cons (point) opoint))))))

(defun live-bounds-of-defun ()
  "Return the bounds of the defun around point. Copies semantics
   directly from the fn eval-defun-2 to ensure highlighted area
   is identical to that which is evaluated."
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (setq beg (point))
    (read (current-buffer))
    (setq end (point))
    (cons beg end)))

;; fix up esf to highlight exactly what emacs evaluates
(defun live-esf-initialize-elisp ()
  (define-eval-sexp-fu-flash-command eval-last-sexp
    (eval-sexp-fu-flash (when (ignore-errors (preceding-sexp))
                          (with-esf-end-of-sexp
                            (live-bounds-of-preceding-sexp)))))
  (define-eval-sexp-fu-flash-command eval-defun
    (eval-sexp-fu-flash (live-bounds-of-defun))))

(live-esf-initialize-elisp)

;; cider extensions


(defun live-bounds-of-cider-last-sexp ()
  "Return the bounds of the defun around point. Copies semantics
   directly from the fn cider-last-sexp to ensure highlighted
   area is identical to that which is evaluated."
  (cons (save-excursion (backward-sexp) (point)) (point)))

(defun live-esf-initialize-cider ()
  (define-eval-sexp-fu-flash-command cider-eval-last-sexp
    (eval-sexp-fu-flash (live-bounds-of-cider-last-sexp)))

  (define-eval-sexp-fu-flash-command cider-pprint-eval-last-sexp
    (eval-sexp-fu-flash (live-bounds-of-cider-last-sexp)))

  (define-eval-sexp-fu-flash-command cider-eval-defun-at-point
    (eval-sexp-fu-flash (let ((bounds (cider--region-for-defun-at-point)))
                          (cons (first bounds) (second bounds)))))


  (progn
    ;; Defines:
    ;; `eval-sexp-fu-cider-sexp-inner-list',
    ;; `eval-sexp-fu-cider-sexp-inner-sexp'
    ;; and the pprint variants respectively.
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-eval-sexp
      cider-eval-last-sexp)
    (define-eval-sexp-fu-eval-sexp eval-sexp-fu-cider-pprint-eval-sexp
      cider-pprint-eval-last-sexp)))

;; end of Sam's code

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
  (define-key cider-repl-mode-map (kbd "C-c C-M-s") 'cider-repl-next-matching-input))

(add-hook 'cider-repl-mode-hook 'custom-cider-repl-bindings)

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

(package-require 'racket-mode)

(provide 'j0ni-lisp)
