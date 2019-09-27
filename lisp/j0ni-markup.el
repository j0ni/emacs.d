;;; markup.el -- HTML and friends

;; textile-mode
;; (use-package textile-mode)
;; (require 'textile-mode)
;; (add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; less + css
;; (use-package less-css-mode)

;; set up css-mode
;; (use-package css-mode)

;; disable scss compilation
;; (use-package scss-mode
;;   :init
;;   (setq scss-compile-at-save nil))


;; (use-package smartparens)
;; (add-hook 'scss-mode-hook 'turn-on-smartparens-mode)

;; Colourise colour names in certain modes
(use-package rainbow-mode
  :hook (html-mode nxhtml-mode nxhtml-mumamo-mode))

;; ;; nXhtml
;; (autoload 'nxhtml-mumamo-mode "autostart" nil t)
;; (add-to-list 'auto-mode-alist '("\\.\\(html\\|ejs\\|jsp\\)$" . nxhtml-mumamo-mode))
;; (eval-after-load "nxhtml-mode"
;;   '(setq mumamo-chunk-coloring 1
;;      rng-nxml-auto-validate-flag nil
;;      nxhtml-skip-welcome t))

;; ;; Patch a mumamo bug which keeps giving annoying warnings
;; (eval-after-load "mumamo"
;;   '(setq mumamo-per-buffer-local-vars (delq 'buffer-file-name mumamo-per-buffer-local-vars)))

;; Some paredit for HTML
(use-package tagedit)
(eval-after-load "sgml-mode"
  '(progn
     (define-key sgml-mode-map (kbd "C-<right>") 'tagedit-forward-slurp-tag)
     (define-key sgml-mode-map (kbd "C-<left>") 'tagedit-forward-barf-tag)
     (define-key sgml-mode-map (kbd "M-S-)") 'tagedit-forward-slurp-tag)
     (define-key sgml-mode-map (kbd "M-k") 'tagedit-kill-attribute)
     (define-key sgml-mode-map (kbd "C-k") 'tagedit-kill)
     (define-key sgml-mode-map (kbd "M-r") 'tagedit-raise-tag)))

;; Key for renaming tags
(eval-after-load "sgml-mode"
  '(define-key sgml-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair))

;; Engage web-mode

(use-package web-mode)
;; (setq web-mode-engines-alist
;;       '(("underscorejs" . "\\.ejs\\'")))
;; (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rjs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; Skewer
(use-package skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(add-hook
 'web-mode-hook
 (lambda ()
   ;; (setq web-mode-indent-style 2) ;; Indent content of HTML tags
   (setq web-mode-markup-indent-offset 2)
   (setq web-mode-css-indent-offset 2)
   (setq web-mode-code-indent-offset 2)
   (turn-off-auto-fill)))

(provide 'j0ni-markup)
