;;; j0ni-codestyle.el -- Indentation styles et al for all modes in one central location

;; Tab indentation is a disease; a cancer of this planet.
(set-default 'indent-tabs-mode nil)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Default indentation
(setq-default tab-width 4)
;; Javascript
(setq-default js2-basic-offset 2)
;; JSON
(setq-default js-indent-level 2)
;; Coffeescript
(setq coffee-tab-width 2)
;; Ruby
(setq-default ruby-indent-level 2)
;; Python
(setq-default py-indent-offset 4)
;; XML
(setq-default nxml-child-indent 2)
;; CSS
(setq-default css-indent-level 2)
(setq-default css-indent-offset 2)

;; Default formatting style for C based modes
(setq c-default-style "java")
(setq-default c-basic-offset 2)

;; I mean, _seriously_...
(setq sentence-end-double-space nil)

;; Enforce proper whitespace
(package-require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;; (setq require-final-newline nil)
(setq mode-require-final-newline nil)

(setq ethan-wspace-disabled-modes '(go-mode makefile-mode makefile-bsdmake-mode org-journal-mode markdown-mode))
(add-hooks ethan-wspace-disabled-modes (lambda () (ethan-wspace-mode -1)))

;; Clojure indentation rules
(eval-after-load "clojure-mode"
  '(dolist (form '(test tests
                   ;; Red Lobster
                   defer waitp let-realised when-realised
                   ;; core.logic
                   run run* fresh conde
                   ;; core.match
                   match))
     (put-clojure-indent form 'defun)))

(provide 'j0ni-codestyle)
