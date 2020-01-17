;;; j0ni-clojure.el Clojure

(require 'j0ni-lisp)
(require 'j0ni-misc)

;; don't do both of these things, or there will be much confusion. decisions,
;; decisions

;; (add-hook 'clojure-mode-hook #'clojure-mode-setup--inf-clojure)
;; (add-hook 'clojure-mode-hook #'cider-mode)

;; Inf-Clojure

(straight-use-package 'flycheck-clj-kondo)

(use-package clojure-mode
  ;; :after (cider clj-refactor)
  :defer t
  :init
  (setq clojure-indent-style 'align-arguments)
  ;; (setq clojure-indent-style 'always-align)

  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  ;; (add-hook 'clojure-mode-hook #'indent-guide-mode)
  (add-hook 'clojure-mode-hook #'hl-sexp-mode)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  (add-hook 'clojure-mode-hook #'turn-on-eldoc-mode)
  (dolist (form '(test
                  tests
                  ;; core.logic
                  run run* fresh conde
                  ;; core.match
                  match))
    (put-clojure-indent form 'defun))

  (define-clojure-indent
    (for-all 1)
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2))

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
      st))

  ;; linting
  (require 'flycheck-clj-kondo))

;; (use-package monroe
;;   :init
;;   (setq monroe-detail-stacktraces t)
;;   (add-hook 'clojure-mode-hook #'clojure-enable-monroe)

;;   :config
;;   (defun mhc-monroe-run ()
;;     (interactive)
;;     (monroe-eval-buffer)
;;     (monroe-input-sender (get-buffer-process monroe-repl-buffer) "(-main)"))

;;   (define-key monroe-interaction-mode-map (kbd "C-x C-j") 'mhc-monroe-run))

(use-package inf-clojure
  ;; :defer t

  :init
  (setq inf-clojure-prompt-read-only nil)
  ;; (setq inf-clojure-generic-cmd "planck -d")

  :config
  ;; (add-hook 'inf-clojure-mode-hook #'enable-paredit-mode)
  ;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
  (add-hook 'inf-clojure-mode-hook #'turn-on-eldoc-mode))

(use-package paredit
  :diminish nil)
(use-package rainbow-delimiters
  :diminish nil)

(defvar dogma (with-temp-buffer
                (insert-file-contents (concat-home ".mutt/dogma"))
                (split-string (buffer-string) "\n" t)))

(use-package cider
  :diminish nil

  :init
  (setq cider-repl-pop-to-buffer-on-connect        t
        cider-repl-use-clojure-font-lock           nil
        cider-save-file-on-load                    t
        cider-repl-display-help-banner             nil
        cider-font-lock-dynamically                nil ;; '(macro core function var) ;; nil
        cider-use-overlays                         t ; display eval results inline
        cider-overlays-use-font-lock               nil ; font lock the results
        cider-show-error-buffer                    t ; 'except-in-repl
        cider-stacktrace-default-filters           '(project)
        cider-buffer-name-show-port                t
        cider-repl-history-size                    10000
        cider-repl-result-prefix                   ""
        cider-prompt-for-symbol                    nil
        cider-known-endpoints                      nil
        cider-repl-history-file                    (concat-home ".cider-repl-history")
        nrepl-buffer-name-show-port                t
        cider-words-of-inspiration                 dogma
        cider-prefer-local-resources               t
        cider-inject-dependencies-at-jack-in       t
        cider-eldoc-display-context-dependent-info t
        cider-clojure-cli-global-options           "-A:dev:bench:measure:inspect"
        ;; cider-jdk-src-paths                        '("~/Scratch/java8-src"
        ;;                                              "~/Scratch/clojure1.9-src")
        cider-lein-parameters                      "with-profile -user repl :headless :host localhost"
        )

  (defun cider-repl-customizations ()
    (interactive)
    (define-key cider-repl-mode-map (kbd "C-r") 'cider-repl-previous-matching-input)
    (define-key cider-repl-mode-map (kbd "C-s") 'swiper)
    (define-key cider-repl-mode-map (kbd "C-c C-M-r") 'isearch-backward-regexp)
    (define-key cider-repl-mode-map (kbd "C-c M-c") 'my-cider-make-connection-default)
    ;; (define-key cider-repl-mode-map (kbd "C-j") 'cider-repl-return)
    (define-key cider-repl-mode-map (kbd "RET") 'cider-repl-newline-and-indent)
    (subword-mode 1)
    ;; (diminish 'subword-mode)
    (make-local-variable 'scroll-conservatively)
    (setq scroll-conservatively 0)
    ;; (paredit-mode 1)
    )

  (add-hook 'cider-repl-mode-hook #'cider-repl-customizations)

  :config
  (use-package eval-sexp-fu)
  (use-package cider-eval-sexp-fu)
  (add-hook 'cider-mode-hook #'turn-on-eldoc-mode)
  ;; (setq eldoc-echo-area-use-multiline-p nil)
  ;; (diminish 'eldoc-mode)
  )

(use-package clj-refactor
  :after (cider)

  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")

  :init
  (setq cljr-warn-on-eval nil)
  (setq cljr-suppress-middleware-warnings t)
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-favor-private-functions nil)
  (setq cljr-inject-dependencies-at-jack-in t)
  (setq cljr-eagerly-build-asts-on-startup t)
  (setq cljr-ignore-analyzer-errors t))

(use-package indent-guide)

;; What is this and where does it come from?
;; (setq clojure-defun-style-default-indent nil)

(provide 'j0ni-clojure)
