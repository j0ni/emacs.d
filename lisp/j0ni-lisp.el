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
(add-lisp-hook 'highlight-parentheses-mode)

;; Make em light up
(require 'highlight)
(packages-require '(eval-sexp-fu hl-sexp))
(add-lisp-hook 'hl-sexp-mode)


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

(package-require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))

(lambda-as-lambda 'clojure-mode "(\\(\\<fn\\>\\)")

(eval-after-load 'clojure-mode
  '(progn
     ;; Make compojure routes look nice
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


;; nRepl/cider
(package-require 'cider)
(eval-after-load "clojure-mode" '(require 'cider))
(setq nrepl-lein-command "lein"
      nrepl-server-command "echo \"lein repl :headless\" | $SHELL -l"
      cider-popup-stacktraces nil
      cider-buffer-name-show-port t)
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

;;Kibit
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



(provide 'j0ni-lisp)
