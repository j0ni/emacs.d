;;; j0ni-misc --- stuff that doesn't go anywhere else

(packages-require
 '(diminish
   dash
   ssh-config-mode
   adaptive-wrap
   auto-compile
   browse-kill-ring
   ;; apache-mode
   log4j-mode
   highlight-symbol
   csv-mode
   http-twiddle
   paredit-everywhere
   debbugs
   terraform-mode))

;; (use-package smart-tab
;;   ;; :config (global-smart-tab-mode)
;;   )

;; (use-package multiple-cursors
;;   :bind (("C-S-c C-S-c" . mc/edit-lines)
;;          ("C-<" . mc/mark-previous-like-this)
;;          ("C->" . mc/mark-next-like-this)
;;          ("C-<" . mc/mark-previous-like-this)))

;; get the `r` key in the debugger for nice formatting and frame selection
(require 'pp-debug)

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  :bind
  (("M-p" . drag-stuff-up)
   ("M-n" . drag-stuff-down)))

(use-package format-sql)
(use-package sql-indent
  :hook
  ((sql-mode . sqlind-minor-mode)))

(use-package imenu-list
  :bind
  ("C-," . imenu-list-smart-toggle)

  :init
  (setq imenu-list-focus-after-activation t))

(use-package pinentry
  :init
  (setq epa-pinentry-mode 'loopback)

  :config
  (setenv "GPG_AGENT_INFO" nil)

  :init
  (pinentry-start))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)

         ;; lets see how this goes
         ("M-." . dumb-jump-go)
         ("M-," . dumb-jump-back))

  :init
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'ag)

  :config (dumb-jump-mode))

;; manage history better
;; (add-hook 'after-init-hook 'session-initialize)

;; Let's see what we're running on
(setq on-console (null window-system))

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; find-file-in-project
(package-require 'find-file-in-project)
(setq ffip-full-paths t)

(use-package all-the-icons-dired
  ;; M-x all-the-icons-install-fonts
  :commands (all-the-icons-dired-mode))

;; Useful for figuring out complicated old code
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(progn
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

;; undo-tree
(use-package undo-tree
  :diminish nil
  :config
  (global-undo-tree-mode t))

;; projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :init
  (setq projectile-completion-system 'ivy)
  :diminish projectile-mode)

;; wc-mode
(package-require 'wc-mode)
(require 'wc-mode)

;; Show column numbers in modeline
(setq column-number-mode t)

;; Make it easy
(defun turn-off-auto-fill ()
  (auto-fill-mode -1))

;; In general we only want this for comments - esk-local-comment-auto-fill
;; should take care of this. But I'm not sure I want auto-fill at all tbh.
;; (setq-default comment-auto-fill-only-comments t)

(package-require 'smartparens)
(require 'smartparens-config)

;; Docker
(package-require 'dockerfile-mode)

;; map start of file and end of file commands to nicer key combos
(global-set-key (read-kbd-macro "M-[") 'beginning-of-buffer)
(global-set-key (read-kbd-macro "M-]") 'end-of-buffer)

;; pipe down
(setq bell-volume 0
      sound-alist nil)

;; deal with broken find-file-at-point
(setq ffap-machine-p-known 'reject)

;; nice scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; don't eat my shell
(setq-default comint-prompt-read-only t)

;; disable backup
(setq backup-inhibited t)

;; don't auto-save
(setq auto-save-default nil)

;; which-key
(use-package which-key
  :config (which-key-mode 1)
  :diminish nil)

;; silver searcher
(use-package ag
  :init
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t)
  (setq ag-highlight-search t)
  :hook
  (ag-mode . toggle-truncate-lines))

;; kotlin
(use-package kotlin-mode)

;; puppet-mode sucks right now
(package-require 'puppet-mode)
(add-hook 'puppet-mode 'turn-on-smartparens-mode)

;; winner restores unintentionally closed windows
(winner-mode t)

;; tramp
(setq tramp-default-method "ssh")

;; info
(package-require 'sicp)
;; (add-to-list 'Info-directory-list "/usr/local/share/info/")

;; clock in the mode-line
(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)

(package-require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(use-package window-number
  :config
  (window-number-meta-mode 1))

;; don't allow popup windows
(setq pop-up-windows nil
      pop-up-frames nil
      split-height-threshold nil
      split-width-threshold nil)

(setq message-fill-column 72
      user-mail-address "j@lollyshouse.ca"
      user-full-name "J Irving")

;; for making eww usable
;; (setq shr-color-visible-luminance-min 100)
;; (setq shr-color-visible-distance-min 100)

(provide 'j0ni-misc)
