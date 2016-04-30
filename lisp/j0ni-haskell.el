;;; j0ni-haskell.el

(package-require 'haskell-mode)
(package-require 'ghc)
(package-require 'shm)
;; (package-require 'auto-complete)

;; auto-complete source using ghc-doc
;; (defun ac-haskell-candidates ()
;;   (let ((pattern (buffer-substring (ghc-completion-start-point) (point)))
;;         (symbols (ghc-select-completion-symbol)))
;;     (all-completions pattern symbols)))

;; Setup auto-complete for haskell-mode
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (add-to-list 'ac-modes 'haskell-mode)
;;      (ac-define-source ghc
;;        '((candidates . ac-haskell-candidates)))))

;; Setup haskell-mode hooks
(eval-after-load "haskell-mode"
  '(custom-set-variables
    '(haskell-mode-hook
      '(turn-on-haskell-indentation
        turn-on-haskell-doc-mode
        ghc-init
        ;; structured-haskell-mode
        ))))

(require 'j0ni-defuns)
;; (defun setup-haskell-arrows (mode mode-map)
;;   (font-lock-replace-symbol mode "\\(->\\)" "→")
;;   (font-lock-replace-symbol mode "\\(<-\\)" "←")
;;   (font-lock-replace-symbol mode "\\(=>\\)" "⇒")

;;   (define-key mode-map (kbd "→") (lambda () (interactive) (insert "->")))
;;   (define-key mode-map (kbd "←") (lambda () (interactive) (insert "<-")))
;;   (define-key mode-map (kbd "⇒") (lambda () (interactive) (insert "=>"))))

;; (eval-after-load "haskell-mode"
;;   '(setup-haskell-arrows 'haskell-mode haskell-mode-map))

;; Add a keybinding for (inferior-haskell-type t) to insert
;; inferred type signature for function at point
;; (define-key haskell-mode-map (kbd "C-c C-s")
;;   (lambda () (interactive)
;;     (let ((sym (haskell-ident-at-point)))
;;       (inferior-haskell-type sym t))))

;; Put ghc-show-info in a popup
(package-require 'popup)
(defun ghc-show-info-popup ()
  (interactive)
  (popup-tip (ghc-get-info (ghc-things-at-point))
             :around t :scroll-bar t))
;; (define-key haskell-mode-map (kbd "C-c TAB") 'ghc-show-info-popup)
;; (define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info-popup)
;; (define-key haskell-mode-map (kbd "C-c C-S-i") 'ghc-show-info)

;; Use standard keybinding for inferior-haskell-find-definition
;; (define-key haskell-mode-map (kbd "M-.")
;;   (lambda () (interactive)
;;     (inferior-haskell-find-definition (haskell-ident-at-point))))

;; Run test suite
(defun haskell-mode-run-test-suite ()
  (interactive)
  (require 'compile)
  (compile (concat "cd " (projectile-project-root) "; cabal test")))
;; (define-key haskell-mode-map (kbd "C-c C-,") 'haskell-mode-run-test-suite)

;; Flycheck addons
(package-require 'flycheck-haskell)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;; Idris (for want of a better place to put it)
(package-require 'idris-mode)
(add-to-list 'auto-mode-alist '("\\.idr$" . idris-mode))


;;; PureScript
(package-require 'purescript-mode)
;; (define-derived-mode purescript-mode haskell-mode "PureScript"
;;   "Major mode for PureScript")
(add-to-list 'auto-mode-alist (cons "\\.purs\\'" 'purescript-mode))

(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker pulp
       "Use Pulp to flycheck PureScript code."
       :command ("pulp" "build" "--monochrome")
       :error-patterns
       ((error line-start
               (or (and "Error at " (file-name)    " line " line ", column " column ":"
                        (zero-or-more " "))
                   (and "\"" (file-name) "\" (line " line ", column " column "):"))
               (message (one-or-more (not (in "*"))))
               line-end))
       :modes purescript-mode)
     (add-to-list 'flycheck-checkers 'pulp)))

;; Generate a likely module name from the current file path
(package-require 'f)
(package-require 's)
(require 'f)
(require 's)
(defun purescript-module-name-from-current-buffer-file ()
  (let ((path (f-split (f-relative
                        (f-base (buffer-file-name))
                        (f-join (projectile-project-root) "src"))))
        (testpath (f-split (f-relative
                            (f-base (buffer-file-name))
                            (f-join (projectile-project-root) "test")))))
    (if (string= ".." (car path))
        (if (string= ".." (car testpath)) "Main" (s-join "." (cons "Test" testpath)))
      (s-join "." path))))

(provide 'j0ni-haskell)


