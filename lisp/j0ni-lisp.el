;;; j0ni-lisp.el Lisps

;; initially copied from bodil's emacs config

;; (use-package ediprolog
;;   :ensure t
;;   :commands ediprolog-dwim
;;   :init
;;   (setq ediprolog-program "/usr/bin/swipl")
;;   :bind (("C-c C-e" . ediprolog-dwim)))

(require 'j0ni-defuns)

(packages-require '(elein
                    kibit-helper
                    ;; geiser
                    hy-mode
                    indent-guide))

(setq j0ni-lisp-modes '(scheme-mode
                        faceup
                        emacs-lisp-mode
                        lisp-mode
                        hy-mode))

(defun add-lisp-hook (func)
  (add-hooks j0ni-lisp-modes func))

(defun set-hyperspec-browser ()
  (setq-local browse-url-browser-function 'eww-browse-url))

(add-hook 'lisp-mode-hook #'set-hyperspec-browser)
(add-hook 'lisp-mode-hook #'enable-eros-mode)
(add-hook 'slime-repl-mode-hook #'set-hyperspec-browser)

;; Setup C-c v to eval whole buffer in all lisps
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)
(define-key lisp-mode-shared-map (kbd "C-c C-v") 'eval-buffer)

(add-lisp-hook 'indent-guide-mode)

;; Make em light up
(require 'highlight)
(use-package eval-sexp-fu)

;; Highlight sexp under cursor
;; (add-lisp-hook 'hl-sexp-mode)
;; (require 'hl-sexp)
;; (eval-after-load 'hl-sexp-mode
;;   (set-face-background 'hl-sexp-face "gray95"))

;; Paredit for all lisps
(use-package smartparens
  :config
  (when (fboundp 'autopair-mode)
    (autopair-mode -1))
  (require 'smartparens-config)
  ;; (smartparens-global-mode 1)
  (sp-use-paredit-bindings)
  ;; (show-smartparens-global-mode 1)
  (add-to-list 'sp-ignore-modes-list #'org-mode)
  (add-to-list 'sp-ignore-modes-list #'org-agenda-mode)
  ;; (smartparens-global-strict-mode 1)
  :bind
  ("C-k" . sp-kill-hybrid-sexp))

(use-package paredit
  :diminish "par"

  :config
  (add-lisp-hook #'turn-off-smartparens-mode)
  (add-lisp-hook #'paredit-mode)

  (progn
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

;; (use-package lispy)

(defun enable-paren-handling ()
  (interactive)
  (paredit-mode 1))

;; (add-lisp-hook 'enable-paren-handling)

;; Make paredit play nice with eldoc
;; (eval-after-load "eldoc"
;;   '(eldoc-add-command
;;     paredit-backward-delete
;;     paredit-close-round))

;; Rainbow delimiters
(package-require 'rainbow-delimiters)
(add-lisp-hook 'rainbow-delimiters-mode)

;; Lambdas
(defun lambda-as-lambda (mode pattern)
  (font-lock-add-keywords
   mode `((,pattern
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     "λ" 'decompose-region)))))))

;; Keyword lists suck
;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
;; (eval-after-load "lisp-mode"
;;   '(defun lisp-indent-function (indent-point state)
;;      "This function is the normal value of the variable `lisp-indent-function'.
;; The function `calculate-lisp-indent' calls this to determine
;; if the arguments of a Lisp function call should be indented specially.
;; INDENT-POINT is the position at which the line being indented begins.
;; Point is located at the point to indent under (for default indentation);
;; STATE is the `parse-partial-sexp' state for that position.
;; If the current line is in a call to a Lisp function that has a non-nil
;; property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
;; it specifies how to indent.  The property value can be:
;; * `defun', meaning indent `defun'-style
;;   \(this is also the case if there is no property and the function
;;   has a name that begins with \"def\", and three or more arguments);
;; * an integer N, meaning indent the first N arguments specially
;;   (like ordinary function arguments), and then indent any further
;;   arguments like a body;
;; * a function to call that returns the indentation (or nil).
;;   `lisp-indent-function' calls this function with the same two arguments
;;   that it itself received.
;; This function returns either the indentation to use, or nil if the
;; Lisp function does not specify a special indentation."
;;      (let ((normal-indent (current-column))
;;            (orig-point (point)))
;;        (goto-char (1+ (elt state 1)))
;;        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
;;        (cond
;;         ;; car of form doesn't seem to be a symbol, or is a keyword
;;         ((and (elt state 2)
;;               (or (not (looking-at "\\sw\\|\\s_"))
;;                   (looking-at ":")))
;;          (if (not (> (save-excursion (forward-line 1) (point))
;;                      calculate-lisp-indent-last-sexp))
;;              (progn (goto-char calculate-lisp-indent-last-sexp)
;;                     (beginning-of-line)
;;                     (parse-partial-sexp (point)
;;                                         calculate-lisp-indent-last-sexp 0 t)))
;;          ;; Indent under the list or under the first sexp on the same
;;          ;; line as calculate-lisp-indent-last-sexp.  Note that first
;;          ;; thing on that line has to be complete sexp since we are
;;          ;; inside the innermost containing sexp.
;;          (backward-prefix-chars)
;;          (current-column))
;;         ((and (save-excursion
;;                 (goto-char indent-point)
;;                 (skip-syntax-forward " ")
;;                 (not (looking-at ":")))
;;               (save-excursion
;;                 (goto-char orig-point)
;;                 (looking-at ":")))
;;          (save-excursion
;;            (goto-char (+ 2 (elt state 1)))
;;            (current-column)))
;;         (t
;;          (let ((function (buffer-substring (point)
;;                                            (progn (forward-sexp 1) (point))))
;;                method)
;;            (setq method (or (function-get (intern-soft function)
;;                                           'lisp-indent-function)
;;                             (get (intern-soft function) 'lisp-indent-hook)))
;;            (cond ((or (eq method 'defun)
;;                       (and (null method)
;;                            (> (length function) 3)
;;                            (string-match "\\`def" function)))
;;                   (lisp-indent-defform state indent-point))
;;                  ((integerp method)
;;                   (lisp-indent-specform method state
;;                                         indent-point normal-indent))
;;                  (method
;;                   (funcall method indent-point state)))))))))

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

(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook #'enable-eros-mode)

(defun enable-eros-mode ()
  (eros-mode 1))

(packages-require '(elisp-slime-nav diminish eros))

(defun elisp-slime-nav-mode-setup ()
  (elisp-slime-nav-mode 1))

(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode-setup)
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;; GNOME won't allow C-M-x for some stupid reason
(require 'edebug)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'edebug-eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c d") 'toggle-debug-on-error)

;; Slime for common lisp

;; (use-package slime
;;   :defer t
;;   :init
;;   (setq inferior-lisp-program
;;         "/usr/bin/sbcl --no-linedit --dynamic-space-size=5120 --no-inform")
;;   (setq slime-contribs '(slime-quicklisp
;;                          slime-fancy
;;                          slime-hyperdoc
;;                          slime-asdf
;;                          slime-banner
;;                          slime-repl)))

;; (use-package slime-docker :defer t)
;; (use-package slime-company
;;   :defer t
;;   :requires company
;;   :init
;;   (add-to-list 'company-backends 'company-slime)
;;   :hook
;;   (slime))

(use-package sly
  :defer t
  :init
  (setq inferior-lisp-program
        "/usr/bin/sbcl --no-linedit --dynamic-space-size=5120 --no-inform"))

(use-package sly-quicklisp
  :defer t
  :requires sly)

;; Racket

(use-package racket-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))
  (setq racket-smart-open-bracket-enable t)
  (setq racket-program "/home/joni/racket/bin/racket")
  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (add-hook 'racket-mode-hook      'my-racket-mode-hook)
  (add-hook 'racket-repl-mode-hook 'my-racket-repl-mode-hook))


(defun my-racket-mode-hook ()
  (interactive)
  (enable-paren-handling)
  (rainbow-delimiters-mode 1)
  (define-key racket-mode-map (kbd "C-c SPC") 'racket-align))

(defun my-racket-repl-mode-hook ()
  (enable-paren-handling))


;; Geiser

(use-package geiser
  :init
  (setq geiser-active-implementations '(chicken guile chez))
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
  (setq geiser-scheme-implementation 'chicken))

;; baseline scheme
(setq scheme-program-name "csi -:c")
;; (setq scheme-program-name "chez-scheme --optimize-level 0 --debug-on-exception")
;; geiser-implementations-alist
;; tell scheme-mode about the test extension
(put 'test-group 'scheme-indent-function 1)

(use-package scheme-complete
  :after scheme
  :bind (:map scheme-mode-map
         ("TAB" . scheme-complete-or-indent))

  :init
  (setq lisp-indent-function 'scheme-smart-indent-function)
  (add-hook 'scheme-mode-hook
            (lambda ()
              (make-local-variable 'eldoc-documentation-function)
              (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
              (eldoc-mode))))

;; Shen
;; (package-require 'shen-mode)

(provide 'j0ni-lisp)
