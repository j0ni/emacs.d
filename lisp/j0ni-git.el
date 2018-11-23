;;; j0ni-git.el

;; magit
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-diff-refine-hunk t)
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)))

;; (require 'magithub)
;; (magithub-feature-autoinject t)

;; (setq magit-popup-use-prefix-argument 'popup)



;; file edit modes
(packages-require '(gitignore-mode
                    gitconfig-mode))

;; contributed bits
(add-to-list 'load-path j0ni-git-contrib-dir)
(require 'git)

;; git gutter
(packages-require '(git-messenger
                    diminish))

(use-package diff-hl
  :ensure t
  :defer t
  :diminish "dhl"
  :commands
  (diff-hl-mode
   diff-hl-dired-mode
   diff-hl-amend-mode
   diff-hl-flydiff-mode
   diff-hl-margin-mode
   global-diff-hl-mode)
  :init
  (add-hook 'emacs-startup-hook 'global-diff-hl-mode)
  ;; (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  ;; (add-hook 'org-mode-hook 'turn-on-diff-hl-mode)
  ;; (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  :config
  (eval-after-load 'magit
    '(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  ;; (global-diff-hl-mode)
  (diff-hl-flydiff-mode 1)
  )

;; (global-diff-hl-mode 1)

;; git-messenger
(setq git-messenger:show-detail t)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(package-require 'git-timemachine)

(package-require 'browse-at-remote)
;; (global-set-key (kbd "C-c g g") 'browse-at-remote)

;; (packages-require '(pcache
;;                     logito
;;                     gh
;;                     gist))

(packages-require '(git-link
                    github-browse-file))

(setq git-link-open-in-browser t)

(provide 'j0ni-git)
