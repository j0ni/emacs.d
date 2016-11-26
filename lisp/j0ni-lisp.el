;;; j0ni-lisp.el -- Lisps

;; initially copied from bodil's emacs config

(require 'j0ni-defuns)

(packages-require '(clojure-mode
                    cider
                    inf-clojure
                    ;; racket-mode
                    geiser))

(setq j0ni-lisp-modes '(scheme-mode
                        ;; racket-mode
                        geiser
                        emacs-lisp-mode
                        lisp-mode
                        clojure-mode
                        cider-repl-mode
                        inf-clojure-mode))

(defun add-lisp-hook (func)
  (add-hooks j0ni-lisp-modes func))

;; Setup C-c v to eval whole buffer in all lisps
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)
(define-key lisp-mode-shared-map (kbd "C-c C-v") 'eval-buffer)

(package-require 'highlight-parentheses)
;; (add-lisp-hook 'highlight-parentheses-mode)
(add-lisp-hook 'indent-guide-mode)

;; Make em light up
(require 'highlight)
(packages-require '(eval-sexp-fu hl-sexp))
(require 'eval-sexp-fu)
;; Highlight sexp under cursor
;; (add-lisp-hook 'hl-sexp-mode)
;; (require 'hl-sexp)
;; (eval-after-load 'hl-sexp-mode
;;   (set-face-background 'hl-sexp-face "gray95"))

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
(defun lambda-as-lambda (mode pattern)
  (font-lock-add-keywords
   mode `((,pattern
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     "λ" 'decompose-region)))))))

;;; Emacs Lisp

(lambda-as-lambda 'emacs-lisp-mode "(\\(\\<lambda\\>\\)")
(lambda-as-lambda 'scheme-mode "(\\(\\<lambda\\>\\)")

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

;; GNOME won't allow C-M-x for some stupid reason
(require 'edebug)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'edebug-eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)

;;; Clojure

(packages-require '(cider-profile clj-refactor))
(setq clojure-defun-style-default-indent nil)
;; (setq clojure-indent-style :align-arguments)
(setq clojure-indent-style :always-align)
(setq cider-repl-display-help-banner nil)
(setq cljr-warn-on-eval nil)

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
  (interactive)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(setq cljr-suppress-middleware-warnings t)
;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(eval-after-load 'clojure-mode
  '(progn
     (dolist (form '(test tests
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
       (delete! 2)

       ;; Om
       (defui '(1 nil (1)))

       (dom/a 1)
       (dom/abbr 1)
       (dom/address 1)
       (dom/area 1)
       (dom/article 1)
       (dom/aside 1)
       (dom/audio 1)
       (dom/b 1)
       (dom/base 1)
       (dom/bdi 1)
       (dom/bdo 1)
       (dom/big 1)
       (dom/blockquote 1)
       (dom/body 1)
       (dom/br 1)
       (dom/button 1)
       (dom/canvas 1)
       (dom/caption 1)
       (dom/cite 1)
       (dom/code 1)
       (dom/col 1)
       (dom/colgroup 1)
       (dom/data 1)
       (dom/datalist 1)
       (dom/dd 1)
       (dom/del 1)
       (dom/details 1)
       (dom/dfn 1)
       (dom/dialog 1)
       (dom/div 1)
       (dom/dl 1)
       (dom/dt 1)
       (dom/em 1)
       (dom/embed 1)
       (dom/fieldset 1)
       (dom/figcaption 1)
       (dom/figure 1)
       (dom/footer 1)
       (dom/form 1)
       (dom/h1 1)
       (dom/h2 1)
       (dom/h3 1)
       (dom/h4 1)
       (dom/h5 1)
       (dom/h6 1)
       (dom/head 1)
       (dom/header 1)
       (dom/hr 1)
       (dom/html 1)
       (dom/i 1)
       (dom/iframe 1)
       (dom/img 1)
       (dom/ins 1)
       (dom/kbd 1)
       (dom/keygen 1)
       (dom/label 1)
       (dom/legend 1)
       (dom/li 1)
       (dom/link 1)
       (dom/main 1)
       (dom/map 1)
       (dom/mark 1)
       (dom/menu 1)
       (dom/menuitem 1)
       (dom/meta 1)
       (dom/meter 1)
       (dom/nav 1)
       (dom/noscript 1)
       (dom/object 1)
       (dom/ol 1)
       (dom/optgroup 1)
       (dom/output 1)
       (dom/p 1)
       (dom/param 1)
       (dom/picture 1)
       (dom/pre 1)
       (dom/progress 1)
       (dom/q 1)
       (dom/rp 1)
       (dom/rt 1)
       (dom/ruby 1)
       (dom/s 1)
       (dom/samp 1)
       (dom/script 1)
       (dom/section 1)
       (dom/small 1)
       (dom/source 1)
       (dom/span 1)
       (dom/strong 1)
       (dom/style 1)
       (dom/sub 1)
       (dom/summary 1)
       (dom/sup 1)
       (dom/table 1)
       (dom/tbody 1)
       (dom/td 1)
       (dom/tfoot 1)
       (dom/th 1)
       (dom/thead 1)
       (dom/time 1)
       (dom/title 1)
       (dom/tr 1)
       (dom/track 1)
       (dom/u 1)
       (dom/ul 1)
       (dom/var 1)
       (dom/video 1)
       (dom/wbr 1)
       (dom/circle 1)
       (dom/clipPath 1)
       (dom/ellipse 1)
       (dom/g 1)
       (dom/line 1)
       (dom/mask 1)
       (dom/path 1)
       (dom/pattern 1)
       (dom/polyline 1)
       (dom/rect 1)
       (dom/svg 1)
       (dom/text 1)
       (dom/defs 1)
       (dom/linearGradient 1)
       (dom/polygon 1)
       (dom/radialGradient 1)
       (dom/stop 1)
       (dom/tspan 1)
       (dom/use 1))

     ;; I can't make my mind up about this. It makes sense
     ;; semantically, but it looks shit when the first value is an
     ;; expression spanning multiple lines. Maybe that's supposed to
     ;; guide better practice?

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

;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
(add-hook 'inf-clojure-mode-hook #'eldoc-mode)


;; nRepl/cider
(package-require 'cider-eval-sexp-fu)

(setq cider-repl-pop-to-buffer-on-connect  t
      cider-repl-use-clojure-font-lock     t
      cider-font-lock-dynamically          nil
      cider-use-overlays                   t ; display eval results inline
      cider-overlays-use-font-lock         t ; font lock the results
      cider-show-error-buffer              t
      cider-popup-stacktraces              t
      cider-buffer-name-show-port          t
      cider-repl-history-size              10000
      cider-repl-result-prefix             "=> "
      cider-prompt-for-symbol              nil
      cider-known-endpoints                nil
      ;; cider-known-endpoints                '(("circle-dev" "localhost" "6005")
      ;;                                        ("circle-staging" "localhost" "6002")
      ;;                                        ("circle-prod" "localhost" "6001")
      ;;                                        ("frontend-dev" "dev.circlehost" "8230"))
      cider-repl-history-file              (concat-home ".cider-repl-history")
      nrepl-buffer-name-show-port          t
      cider-words-of-inspiration           '("")
      cider-prefer-local-resources         t
      cider-inject-dependencies-at-jack-in t)

(defun cider-mode-setup ()
  (require 'cider-eval-sexp-fu)
  (turn-on-eldoc-mode)
  (diminish 'eldoc-mode)
  (diminish 'cider-mode))

(add-hook 'cider-mode-hook 'cider-mode-setup)

(defun my-cider-make-connection-default ()
  (interactive)
  (cider-make-connection-default)
  (message "Set default connection"))

(defun cider-repl-customizations ()
  (interactive)
  (define-key cider-repl-mode-map (kbd "C-r") 'cider-repl-previous-matching-input)
  (define-key cider-repl-mode-map (kbd "C-s") 'cider-repl-next-matching-input)
  (define-key cider-repl-mode-map (kbd "C-c C-M-r") 'isearch-backward-regexp)
  (define-key cider-repl-mode-map (kbd "C-c C-M-s") 'isearch-forward-regexp)
  (define-key cider-repl-mode-map (kbd "C-c M-c") 'my-cider-make-connection-default)
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
(setq inferior-lisp-program "/usr/bin/sbcl")
;; (setq inferior-lisp-program "/usr/local/bin/lisp")
(slime-setup '(slime-fancy slime-asdf slime-tramp slime-banner))

(defun slime-repl-mode-custom ()
  (paredit-mode t)
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'slime-repl-mode-custom)

;; Racket

;; (package-require 'racket-mode)
;; (setq racket-racket-program "/Applications/Racket v6.5/bin/racket"
;;       racket-raco-program "/Applications/Racket v6.5/bin/raco"
;;       racket-smart-open-bracket-enable t)

;; (defun my-racket-mode-hook ()
;;   (define-key racket-mode-map (kbd "C-M-z") 'racket-align))

;; (defun my-racket-repl-mode-hook ()
;;   (enable-paren-handling))

;; (add-hook 'racket-mode-hook 'my-racket-mode-hook)
;; (add-hook 'racket-repl-mode-hook 'my-racket-repl-mode-hook)

;; Geiser

(setq geiser-active-implementations '(racket chicken guile))
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; baseline scheme
(setq scheme-program-name "csi -:c")

;; tell scheme-mode about the test extension
(put 'test-group 'scheme-indent-function 1)

;; Shen
(package-require 'shen-mode)

(provide 'j0ni-lisp)
