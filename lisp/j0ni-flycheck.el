;;; j0ni-flycheck.el --- Flycheck setup

(packages-require '(s f flycheck flycheck-clojure flycheck-pos-tip))

(add-hook 'find-file-hook
          (lambda ()
            (when (not (equal 'emacs-lisp-mode major-mode clojure-mode))
              (flycheck-mode))))

;; (global-set-key (kbd "M-n") 'next-error)
;; (global-set-key (kbd "M-p") 'previous-error)

(package-require 'flycheck-color-mode-line)

(eval-after-load 'flycheck
  '(progn
     (setq flycheck-highlighting-mode nil
           flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
     (flycheck-clojure-setup)
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
     ; (set-face-background 'flycheck-error "red")
     ; (set-face-foreground 'flycheck-error "black")
     ; (set-face-background 'flycheck-warning "orange")
     ; (set-face-foreground 'flycheck-warning "black")
     ))

(provide 'j0ni-flycheck)
