;;; j0ni-git.el

;; magit
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-diff-refine-hunk t)
  (setq magit-bury-buffer-function 'magit-mode-quit-window)
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)))

;; (require 'magithub)
;; (magithub-feature-autoinject t)

;; (setq magit-popup-use-prefix-argument 'popup)
(use-package forge :after magit)

;; file edit modes
(use-package gitignore-mode)
(use-package gitconfig-mode)

;; contributed bits
;; (add-to-list 'load-path j0ni-git-contrib-dir)
;; (require 'git)

;; git gutter
(use-package git-messenger)

(use-package diff-hl
  :diminish "dhl"
  :commands
  (diff-hl-mode
   diff-hl-dired-mode
   diff-hl-amend-mode
   diff-hl-flydiff-mode
   diff-hl-margin-mode
   global-diff-hl-mode)
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'org-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)
  :config
  ;; (global-diff-hl-mode 1)
  (eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  ;; (diff-hl-flydiff-mode 1)
  )

;; git-messenger
(setq git-messenger:show-detail t)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(use-package git-timemachine)
(use-package browse-at-remote)

;; (global-set-key (kbd "C-c g g") 'browse-at-remote)

;; (packages-require '(pcache
;;                     logito
;;                     gh
;;                     gist))

(use-package git-link)
(use-package github-browse-file)

(setq git-link-open-in-browser t)

(provide 'j0ni-git)
