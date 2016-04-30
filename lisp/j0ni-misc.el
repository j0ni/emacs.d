;;; j0ni-misc --- stuff that doesn't go anywhere else

(packages-require
 '(diminish
   dash
   ssh-config-mode
   dropdown-list
   adaptive-wrap
   auto-compile
   browse-kill-ring
   apache-mode
   log4j-mode
   highlight-symbol
   highlight-parentheses
   smart-tab
   csv-mode
   scala-mode2
   csv-nav
   http-twiddle
   paredit-everywhere
   session))

;; manage history better
(add-hook 'after-init-hook 'session-initialize)

;; time tracking
(require 'timeclock)
(define-key ctl-x-map "ti" 'timeclock-in)
(define-key ctl-x-map "to" 'timeclock-out)
(define-key ctl-x-map "tc" 'timeclock-change)
(define-key ctl-x-map "tr" 'timeclock-reread-log)
(define-key ctl-x-map "tu" 'timeclock-update-mode-line)
(define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)
(timeclock-mode-line-display)

;; Let's see what we're running on
(setq on-console (null window-system))

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; find-file-in-project
(package-require 'find-file-in-project)
(setq ffip-full-paths t)

;; Neotree
(package-require 'neotree)
(setq neo-theme 'nerd)

;; Useful for figuring out complicated old code
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(progn
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))

(global-set-key (kbd "C-c C-p") 'neotree-project-dir)

;; undo-tree
(package-require 'undo-tree)
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)

;; projectile
(package-require 'projectile)
;; (setq projectile-completion-system 'ido)
(setq projectile-completion-system 'ivy)
(projectile-global-mode)
(diminish 'projectile-mode)

;; wc-mode
(package-require 'wc-mode)
(require 'wc-mode)

;; Show current function in modeline
;; (which-function-mode)

;; Show line numbers in buffers
;; (global-linum-mode t)

;; Show column numbers in modeline
(setq column-number-mode t)

;; Redefine linum-on to ignore terminal buffers, because just turning
;; it off in term-mode-hook doesn't work.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode org-mode
                  nrepl-mode cider-repl-mode lisp-interaction-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))
;; (add-hook 'prog-mode-hook 'linum-on)

;; Make it easy
(defun turn-off-auto-fill ()
  (auto-fill-mode -1))

;; In general we only want this for comments
(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)

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

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

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
(package-require 'which-key)
(which-key-mode 1)
(diminish 'which-key-mode)

;; ack
(package-require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; silver searcher
(package-require 'ag)
(setq ag-reuse-buffers t)
(setq ag-reuse-window t)
(setq ag-highlight-search t)
(add-hook 'ag-mode-hook 'toggle-truncate-lines)

;; java
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

;; puppet-mode sucks right now
(package-require 'puppet-mode)
(add-hook 'puppet-mode 'turn-on-smartparens-mode)

;; winner restores unintentionally closed windows
(winner-mode t)

;; tramp
(setq tramp-default-method "ssh")

;; info
(package-require 'sicp)
(add-to-list 'Info-directory-list "~/Scratch/Lisp/on-lisp/")
(add-to-list 'Info-directory-list "/usr/local/share/info/")

;; clock in the mode-line
(setq display-time-format "%H:%M")
(display-time-mode t)

;; for sr-speedbar in 24.3.90+
;; (defun ad-advised-definition-p (definition)
;;   "Return non-nil if DEFINITION was generated from advice information."
;;    (if (or (ad-lambda-p definition) (macrop definition) (ad-compiled-p definition))
;;        (let ((docstring (ad-docstring definition)))
;;          (and (stringp docstring)
;;               (get-text-property 0 'dynamic-docstring-function docstring)))))

(package-require 'sr-speedbar)
(setq speedbar-use-images nil)

;; vagrant method for tramp
(package-require 'vagrant-tramp)

(package-require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; (package-require 'win-switch)
;; (setq win-switch-feedback-background-color "DeepPink3")
;; (setq win-switch-feedback-foreground-color "black")
;; (setq win-switch-window-threshold 1)
;; (setq win-switch-idle-time 0.7)

;; disable majority of shortcuts
;; (win-switch-set-keys '() 'up)
;; (win-switch-set-keys '() 'down)
;; (win-switch-set-keys '() 'left)
;; (win-switch-set-keys '() 'right)
;; (win-switch-set-keys '("o") 'next-window)
;; (win-switch-set-keys '("p") 'previous-window)
;; (win-switch-set-keys '() 'enlarge-vertically)
;; (win-switch-set-keys '() 'shrink-vertically)
;; (win-switch-set-keys '() 'shrink-horizontally)
;; (win-switch-set-keys '() 'enlarge-horizontally)
;; (win-switch-set-keys '() 'other-frame)
;; (win-switch-set-keys '("C-g") 'exit)
;; (win-switch-set-keys '() 'split-horizontally)
;; (win-switch-set-keys '() 'split-vertically)
;; (win-switch-set-keys '() 'delete-window)
;; (win-switch-set-keys '("\M-\C-g") 'emergency-exit)

;; (global-set-key (kbd "C-x o") 'win-switch-dispatch)

(package-require 'window-number)
;; (autoload 'window-number-mode "window-number"
;;   "A global minor mode that enables selection of windows according to
;; numbers with the C-x C-j prefix.  Another mode,
;; `window-number-meta-mode' enables the use of the M- prefix."
;;   t)
;; (window-number-mode 1)

(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  t)
(window-number-meta-mode 1)

;; Rudel, for sharing buffers

;; (package-require 'rudel)

(package-require 'beacon)
(setq beacon-blink-when-point-moves-vertically 35)
(setq beacon-blink-when-focused t)
(beacon-mode 1)
(diminish 'beacon-mode)

;; don't allow popup windows
(setq pop-up-windows nil
      split-height-threshold nil
      split-width-threshold nil)

(setq message-default-headers "Cc: \nBcc: jonathan@circleci.com\n"
      message-fill-column 72
      user-mail-address "jonathan@circleci.com"
      user-full-name "Jonathan Irving")

;; for making eww usable
(setq shr-color-visible-luminance-min 75)

;; centering text in a single window
(package-require 'centered-window-mode)

;; dired
(setq insert-directory-program "/usr/local/bin/gls")

;; sml
(package-require 'smart-mode-line)
(setq sml/theme nil)
(sml/setup)

(provide 'j0ni-misc)
