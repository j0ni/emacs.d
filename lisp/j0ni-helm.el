;;; j0ni-helm.el --- Helm will steer you in the right direction. Duh.

(package-require 'helm)
(require 'helm-config)

(setq
 ;; fuzzy matching
 helm-recentf-fuzzy-match              t
 helm-buffers-fuzzy-matching           t
 helm-locate-fuzzy-match               t
 helm-M-x-fuzzy-match                  t
 helm-semantic-fuzzy-match             t
 helm-imenu-fuzzy-match                t
 helm-apropos-fuzzy-match              t
 helm-lisp-fuzzy-completion            t
 helm-mode-fuzzy-match                 t
 helm-completion-in-region-fuzzy-match t
 ;; setup from the guide
 helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
 helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
 helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
 helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
 helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "C-c h")    'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x")      'helm-M-x)
;; (global-set-key (kbd "C-x C-f")  'helm-find-files)
(global-set-key (kbd "C-x b")    'helm-mini)

(define-key helm-command-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-command-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-command-map (kbd "C-z")   'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(helm-mode 1)

;; Projectile support
(package-require 'helm-projectile)
;; (global-set-key (kbd "C-c <print>") 'helm-projectile)

;; Company mode
(package-require 'helm-company)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map   (kbd "C-\\") 'helm-company)
     (define-key company-active-map (kbd "C-\\") 'helm-company)))

;; Backup
(package-require 'helm-backup)
(add-hook 'after-save-hook 'helm-backup-versioning)
(global-set-key (kbd "C-c b") 'helm-backup)
(setq helm-backup-path "~/.emacs.d/.helm-backup")

(provide 'j0ni-helm)
