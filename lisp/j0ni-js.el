;;; j0ni-js.el

(packages-require
 '(js2-mode
   json-reformat
   smartparens))

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
(package-require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)

(eval-after-load 'js2-mode
  '(progn
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

(autoload 'js2-mode "js2-mode" nil t)
;; (autoload 'autopair-mode "autopair" nil t)

(add-to-list 'auto-mode-alist '("\\.jsx?$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . js2-jsx-mode))

(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

(package-require 'json-mode)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.amethyst$" . json-mode))

(eval-after-load 'js
  '(progn
     (define-key js-mode-map "{" 'paredit-open-curly)
     (define-key js-mode-map "}" 'paredit-close-curly)
     (add-hook 'js-mode-hook 'esk-paredit-nonlisp)))

(add-hook 'coffee-mode-hook
          (lambda ()
            (define-key coffee-mode-map (kbd "M-r") 'coffee-compile-buffer)
            (define-key coffee-mode-map (kbd "M-R") 'coffee-compile-region)
            (define-key coffee-mode-map (kbd "<tab>") 'coffee-indent)
            (define-key coffee-mode-map (kbd "<backtab>") 'coffee-unindent)))

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cson$" . coffee-mode))


;; Coffeescript
(package-require 'coffee-mode)

(setq coffee-tab-width 2)

(defun coffee-mode-custom ()
  (highlight-parentheses-mode t)
  (turn-on-smartparens-mode))

(add-hook 'coffee-mode-hook 'coffee-mode-custom)

;; Use js2-mode for displaying compiled CS
(setq coffee-js-mode 'js2-mode)

;; Patch coffee-mode so coffee-compile-region pops up a new
;; non-focused window instead of replacing the current buffer.
(eval-after-load "coffee-mode"
  '(defun coffee-compile-region (start end)
     "Compiles a region and displays the JS in another buffer."
     (interactive "r")
     (let ((buffer (get-buffer coffee-compiled-buffer-name)))
       (when buffer (kill-buffer buffer)))
     (call-process-region start end coffee-command nil
                          (get-buffer-create coffee-compiled-buffer-name) nil "-s" "-p" "--bare")
     (let ((buffer (get-buffer coffee-compiled-buffer-name)))
       (with-current-buffer buffer
         (funcall coffee-js-mode)
         (goto-char (point-min)))
       (display-buffer buffer))))

;; Handle backtabs and indenting regions
(defun coffee-indent-block ()
  (shift-region coffee-tab-width)
  (setq deactivate-mark nil))

(defun coffee-unindent-block ()
  (shift-region (- coffee-tab-width))
  (setq deactivate-mark nil))

(defun coffee-indent ()
  (interactive)
  (if (and (boundp 'ac-trigger-command-p) (ac-trigger-command-p last-command))
      (auto-complete)
    (if mark-active
        (coffee-indent-block)
      (indent-for-tab-command))))

(defun coffee-unindent ()
  (interactive)
  (if mark-active
      (coffee-unindent-block)
    (progn
      (indent-line-to (- (current-indentation) coffee-tab-width)))))


(provide 'j0ni-js)


