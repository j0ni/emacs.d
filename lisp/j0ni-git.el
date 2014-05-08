;;; j0ni-git.el

;; magit
(package-require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; file edit modes
(packages-require
 '(gitignore-mode
   git-commit-mode
   gitconfig-mode))

;; contributed bits
(add-to-list 'load-path j0ni-git-contrib-dir)
(require 'git)

;; git gutter
(packages-require
 '(git-gutter
   git-gutter-fringe
   git-messenger
   diminish))

(require 'git-gutter-fringe)
(global-git-gutter-mode +1)
(diminish 'git-gutter-mode "GG")

;; git-messenger
(setq git-messenger:show-detail t)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(provide 'j0ni-git)
