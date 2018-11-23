;;; j0ni-js.el

(packages-require
 '(js2-mode
   json-reformat
   smartparens
   js-comint))

(package-require 'flycheck)
(setq-default flycheck-jshintrc (concat dotfiles-dir ".jshintrc"))

(defun js2-mode-custom ()
  (interactive)
  (smartparens-mode +1)
  (dolist (s '("describe"
               "it"
               "before"
               "beforeEach"
               "after"
               "afterEach"
               "angular"
               "define"))
    (add-to-list 'js2-additional-externs s)))

(setq js2-indent-on-enter-key t
      js2-enter-indents-newline t
      js2-auto-indent nil
      js2-mirror-mode nil
      js2-cleanup-whitespace t
      js2-include-node-externs t
      js2-pretty-multiline-declarations t
      js2-allow-keywords-as-property-names t
      js2-mode-assume-strict t)

(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-strict-var-hides-function-arg-warning nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning nil)
(setq-default js2-strict-cond-assign-warning nil)
(setq-default js2-strict-var-redeclaration-warning nil)

;; Tern
;; (package-require 'tern)
;; (add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; Skewer
;; (package-require 'skewer-mode)
;; (add-hook 'js2-mode-hook 'skewer-mode)

;; Jade
(package-require 'jade-mode)

(defun js2-mode-custom-setup ()
  (jade-interaction-mode 1)
  (turn-on-smartparens-mode)
  (local-set-key (kbd "C-c C-c") 'jade-eval-buffer))


(eval-after-load 'js2-mode
  '(progn
     (add-hook 'js2-mode-hook 'js2-mode-custom-setup)
     (add-hook 'js2-post-parse-callbacks 'js2-mode-custom)
     (add-hook 'js2-post-parse-callbacks 'my-js2-parse-global-vars-decls)
     (defun my-js2-parse-global-vars-decls ()
       (let ((btext (replace-regexp-in-string
                     ": *true" " "
                     (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
         (setq js2-additional-externs
               (split-string
                (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext)
                    (match-string-no-properties 1 btext) "")
                " *, *" t))))))

;; add buffer-local indicator for whether prog-mode-hook has run.
(defun my-set-pmh-ran ()
  (set (make-local-variable 'my-pmh-ran) t))

(add-hook 'prog-mode-hook 'my-set-pmh-ran)
(add-hook 'js2-init-hook 'my-run-pmh-if-not-ran)

(defun my-run-pmh-if-not-ran ()
  (unless (bound-and-true-p my-pmh-ran)
    (run-hooks 'prog-mode-hook)))

(package-require 'json-mode)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.amethyst$" . json-mode))

(provide 'j0ni-js)
