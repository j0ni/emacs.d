;;; j0ni-misc --- stuff that doesn't go anywhere else

(use-package ssh-config-mode)
(use-package paredit-everywhere)
(use-package terraform-mode)
(use-package csv-mode)

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

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

(use-package fish-mode)

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

;; (use-package imenu-list
;;   :bind
;;   ("C-," . imenu-list-smart-toggle)

;;   :init
;;   (setq imenu-list-auto-resize t)
;;   (setq imenu-list-focus-after-activation t))

(use-package pinentry
  :init
  (setq epa-pinentry-mode 'loopback)

  :config
  (setenv "GPG_AGENT_INFO" nil)

  :init
  (pinentry-start))

;; (use-package dumb-jump
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;;          ("M-g j" . dumb-jump-go)
;;          ("M-g i" . dumb-jump-go-prompt)
;;          ("M-g x" . dumb-jump-go-prefer-external)
;;          ("M-g z" . dumb-jump-go-prefer-external-other-window)

;;          ;; lets see how this goes
;;          ("M-." . dumb-jump-go)
;;          ("M-," . dumb-jump-back))

;;   :init
;;   (setq dumb-jump-selector 'ivy)
;;   (setq dumb-jump-force-searcher 'ag)

;;   :config (dumb-jump-mode))

;; manage history better
;; (add-hook 'after-init-hook 'session-initialize)

;; Let's see what we're running on
(setq on-console (null window-system))

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; find-file-in-project
;; (use-package find-file-in-project
;;   :init
;;   (setq ffip-full-paths t))

;; (use-package all-the-icons-dired
;;   ;; M-x all-the-icons-install-fonts
;;   :commands (all-the-icons-dired-mode))

;; Useful for figuring out complicated old code
(use-package highlight-symbol
  :init
  (setq hi-lock-auto-select-face t)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :bind
  (("C-<f3>" . highlight-symbol)
   ("<f3>" . highlight-symbol-next)
   ("S-<f3>" . highlight-symbol-prev)
   ("M-<f3>" . highlight-symbol-query-replace)))


;; undo-tree
(use-package undo-tree
  :diminish nil
  :config
  (global-undo-tree-mode t))

;; projectile
(use-package projectile
  :demand t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t)
  :init
  (setq projectile-mode-line-function
        '(lambda ()
           (format " project[%s]" (projectile-project-name))))
  ;; set elsewhere
  ;; (setq projectile-completion-system 'ivy)
  (setq projectile-completion-system 'ido)

  :diminish projectile-mode)

(use-package projectile-ripgrep)

(use-package deadgrep)

(add-to-list 'load-path "~/Scratch/emacs/emacs-libvterm")
(require 'vterm)

;; Show column numbers in modeline
(setq column-number-mode t)

;; Make it easy
(defun turn-off-auto-fill ()
  (auto-fill-mode -1))

;; In general we only want this for comments - esk-local-comment-auto-fill
;; should take care of this. But I'm not sure I want auto-fill at all tbh.
;; (setq-default comment-auto-fill-only-comments t)

(use-package smartparens
  :config
  (require 'smartparens-config))

;; Docker
(use-package dockerfile-mode)

;; map start of file and end of file commands to nicer key combos
(global-set-key (read-kbd-macro "M-[") 'beginning-of-buffer)
(global-set-key (read-kbd-macro "M-]") 'end-of-buffer)

;; pipe down
(setq bell-volume 0
      sound-alist nil)

;; deal with broken find-file-at-point
(setq ffap-machine-p-known 'reject)

;; (use-package sublimity
;;   :init
;;   (setq sublimity-scroll-weight 5)
;;   (setq sublimity-scroll-drift-length 10)
;;   ;; (setq sublimity-map-size 20)
;;   ;; (setq sublimity-map-fraction 0.3)
;;   ;; (setq sublimity-map-text-scale -7)
;;   :config
;;   (require 'sublimity)
;;   (require 'sublimity-scroll)
;;   ;; (require 'sublimity-map)
;;   ;; (require 'sublimity-attractive)
;;   (sublimity-mode 1))

;; nice scrolling
;; (use-package smooth-scrolling
;;   :init
;;   (setq smooth-scroll-margin 2)

;;   :config
;;   (smooth-scrolling-mode))

(setq scroll-step 0)
(setq scroll-margin 2)
(setq auto-window-vscroll nil)
;; be sure to set this to 0 in any auto-scrolling buffers
(setq scroll-conservatively 100000)
;; (setq scroll-conservatively 1)
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed t)              ; don't accelerate scrolling

;; whitespace
(setq whitespace-line-column 100)
(setq whitespace-style '(face trailing lines-tail tabs))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; don't hassle me about following symlinks
(setq vc-follow-symlinks t)

;; don't eat my shell
(setq-default comint-prompt-read-only t)

;; disable backup
(setq backup-inhibited t)
(setq backup-by-copying t)

;; let's [edit to add: NOT] try auto-saving into the current file
(setq auto-save-default nil)
;; (auto-save-visited-mode 1)
;; (setq auto-save-interval 500)
;; (setq auto-save-visited-interval 300)

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

(grep-apply-setting
 'grep-find-command
 '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))

(use-package rg)

;; kotlin
(use-package kotlin-mode
  :config (require 'kotlin-mode-lexer))
(use-package gradle-mode
  :hook (kotlin-mode))

;; puppet-mode sucks right now
;; (use-package puppet-mode)
;; (add-hook 'puppet-mode 'turn-on-smartparens-mode)

;; winner restores unintentionally closed windows
(winner-mode t)

;; tramp
(setq tramp-default-method "ssh")

;; info
(use-package sicp)
;; (add-to-list 'Info-directory-list "/usr/local/share/info/")

;; clock in the mode-line
(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil)

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

(use-package yaml-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(use-package vterm)

(provide 'j0ni-misc)
