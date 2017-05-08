;;; j0ni-git.el

;; magit
(packages-require '(magit
                    magithub))

;; (require 'magithub)
;; (magithub-feature-autoinject t)

(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")
;; (setq magit-popup-use-prefix-argument 'popup)
(setq magit-completing-read-function 'ivy-completing-read)
(setq magit-diff-refine-hunk t)

;; file edit modes
(packages-require '(gitignore-mode
                    gitconfig-mode))

;; contributed bits
(add-to-list 'load-path j0ni-git-contrib-dir)
(require 'git)

;; git gutter
(packages-require '(git-gutter
                    git-gutter-fringe
                    git-messenger
                    diminish))

;; (require 'git-gutter-fringe)

(global-git-gutter-mode +1)

;; (git-gutter:linum-setup)

;; (custom-set-variables
;;  '(git-gutter:modified-sign "~")
;;  '(git-gutter:added-sign "+")
;;  '(git-gutter:deleted-sign "-"))

(diminish 'git-gutter-mode "gg")

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
