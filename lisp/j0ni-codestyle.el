;;; j0ni-codestyle.el -- Indentation styles et al for all modes in one central location

;; Tab indentation is a disease; a cancer of this planet.
(set-default 'indent-tabs-mode nil)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Aggressively indent
(package-require 'aggressive-indent)
;; sadly new clojure indentation code slows this down
(add-hook 'prog-mode-hook (lambda () (aggressive-indent-mode 1)))

;; indent-guide - don't switch it on though, just have it there fore
;; when I need it
(package-require 'indent-guide)
(eval-after-load 'indent-guide
  '(progn
     (set-face-foreground 'indent-guide-face "gray80")
     (setq indent-guide-char ":")
     (setq indent-guide-recursive nil)))
(diminish 'indent-guide-mode)

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

;; Whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Enforce proper whitespace
;; (package-require 'ethan-wspace)
;; (global-ethan-wspace-mode 1)

;; (setq require-final-newline nil)
;; (setq mode-require-final-newline nil)

;; (setq ethan-wspace-disabled-modes '(go-mode makefile-mode makefile-bsdmake-mode org-journal-mode markdown-mode))
;; (add-hooks ethan-wspace-disabled-modes (lambda () (ethan-wspace-mode -1)))

(package-require 'ws-butler)
(ws-butler-global-mode 1)

(provide 'j0ni-codestyle)
