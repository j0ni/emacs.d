;;; j0ni-clojure.el -- Clojure

(require 'j0ni-lisp)
(require 'j0ni-misc)

;; don't do both of these things, or there will be much confusion. decisions,
;; decisions

;; (add-hook 'clojure-mode-hook #'clojure-mode-setup--spiral)
;; (add-hook 'clojure-mode-hook #'clojure-mode-setup--inf-clojure)
(add-hook 'clojure-mode-hook #'clojure-mode-setup--cider)

;; Unrepl.el

(use-package spiral
  :ensure t

  :init
  (add-hook 'spiral-repl-mode-hook #'spiral-repl-setup))

(defun spiral-repl-setup ()
  (enable-paren-handling)
  (eldoc-mode 1))

(defun clojure-mode-setup--spiral ()
  (clojure-mode-common-setup)
  (spiral-mode t))

;; Inf-Clojure

(packages-require '(clojure-mode
                    inf-clojure
                    clj-refactor
                    paredit
                    rainbow-delimiters
                    cljsbuild-mode))

(setq clojure-defun-style-default-indent nil)
(setq inf-clojure-prompt-read-only nil)

(defun clojure-mode-setup--inf-clojure ()
  (clojure-mode-common-setup)
  (inf-clojure-minor-mode 1))

(defun inf-clojure-mode-setup ()
  (enable-paren-handling)
  (eldoc-mode t)
  ;; (setq completion-at-point-functions nil)
  )

(add-hook 'inf-clojure-mode-hook #'inf-clojure-mode-setup)

;; Cider
(packages-require '(clojure-mode
                    cider
                    eval-sexp-fu
                    cider-eval-sexp-fu
                    clj-refactor
                    indent-guide
                    paredit
                    rainbow-delimiters
                    cljsbuild-mode))

(setq cider-repl-pop-to-buffer-on-connect        t
      cider-repl-use-clojure-font-lock           t
      cider-save-file-on-load                    t
      cider-repl-display-help-banner             nil
      cider-font-lock-dynamically                nil ;; '(macro core function var) ;; nil
      cider-use-overlays                         t ; display eval results inline
      cider-overlays-use-font-lock               t ; font lock the results
      cider-show-error-buffer                    t
      cider-popup-stacktraces                    t
      cider-buffer-name-show-port                t
      cider-repl-history-size                    10000
      cider-repl-result-prefix                   "=> "
      cider-prompt-for-symbol                    nil
      cider-known-endpoints                      nil
      cider-repl-history-file                    (concat-home ".cider-repl-history")
      nrepl-buffer-name-show-port                t
      cider-words-of-inspiration                 '("")
      cider-prefer-local-resources               t
      cider-inject-dependencies-at-jack-in       t
      ;; cider-lein-parameters                      "nrepl :middleware \"['cider.nrepl/cider-middleware]\""
      cider-eldoc-display-context-dependent-info t
      cider-pprint-fn                            'fipp
      cider-clojure-cli-global-options           "-A:dev"
      cider-jdk-src-paths                        '("~/Scratch/java8-src"
                                                   "~/Scratch/clojure1.9-src")
      cider-cljs-lein-repl                       "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

(defun clojure-mode-setup--cider ()
  (clojure-mode-common-setup)
  (cider-mode 1))

(defun cider-mode-setup ()
  (require 'cider-eval-sexp-fu)
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (diminish 'cider-mode))

(add-hook 'cider-mode-hook 'cider-mode-setup)

(defun cider-repl-customizations ()
  (interactive)
  (define-key cider-repl-mode-map (kbd "C-r") 'cider-repl-previous-matching-input)
  (define-key cider-repl-mode-map (kbd "C-s") 'swiper)
  (define-key cider-repl-mode-map (kbd "C-c C-M-r") 'isearch-backward-regexp)
  (define-key cider-repl-mode-map (kbd "C-c M-c") 'my-cider-make-connection-default)
  ;; (define-key cider-repl-mode-map (kbd "C-j") 'cider-repl-return)
  (define-key cider-repl-mode-map (kbd "RET") 'cider-repl-newline-and-indent)
  (subword-mode 1)
  (diminish 'subword-mode)
  (enable-paren-handling))

(add-hook 'cider-repl-mode-hook 'cider-repl-customizations)

;; Common

(setq clojure-indent-style :align-arguments)
;; (setq clojure-indent-style :always-align)

(setq cljr-warn-on-eval nil)
(setq cljr-suppress-middleware-warnings t)
(setq cljr-favor-prefix-notation nil)
(setq cljr-favor-private-functions nil)

(defun clojure-mode-common-setup ()
  (enable-paren-handling)
  (rainbow-delimiters-mode 1)
  (indent-guide-mode 1)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (turn-on-eldoc-mode))

(eval-after-load 'clojure-mode
  '(progn
     (dolist (form '(test
                     tests
                     ;; Red Lobster
                     defer waitp let-realised when-realised
                     ;; core.logic
                     run run* fresh conde
                     ;; core.match
                     match))
       (put-clojure-indent form 'defun))
     ;; Make compojure routes look nice
     ;; (define-key clojure-mode-map (kbd "C-M-z") 'align-cljlet)
     (define-clojure-indent
       (defroutes 'defun)
       (GET 2)
       (POST 2)
       (PUT 2)
       (DELETE 2)
       (HEAD 2)
       (ANY 2)
       (context 2)
       (insert! 2)
       (update! 2)
       (delete! 2))

     ;; I can't make my mind up about this. It makes sense
     ;; semantically, but it looks shit when the first value is an
     ;; expression spanning multiple lines.

     ;; note I no longer think this looks shit, and I prefer it, but
     ;; it may not be popular.
     (defun unpopular-macro-indentation ()
       (interactive)
       (put-clojure-indent '-> 1)
       (put-clojure-indent '->> 1)
       (put-clojure-indent 'some-> 1)
       (put-clojure-indent 'some->> 1))

     ;; Treat hyphens as a word character when transposing words
     (defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
       (let ((st (make-syntax-table clojure-mode-syntax-table)))
         (modify-syntax-entry ?- "w" st)
         st))))


(provide 'j0ni-clojure)
