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
   wc-mode
   smooth-scrolling
   diminish

   scala-mode2

   csv-nav
   figlet
   http-twiddle
   paredit-everywhere))

;; find-file-in-project
(package-require 'find-file-in-project)
(setq ffip-full-paths t)

;; undo-tree
(package-require 'undo-tree)
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)

;; projectile
(package-require 'projectile)
(projectile-global-mode)
(diminish 'projectile-mode)

;; wc-mode
(require 'wc-mode)

;; linum-mode in all programming buffers
(add-hook 'prog-mode-hook 'linum-mode)

(defun turn-off-auto-fill ()
  (auto-fill-mode -1))

(package-require 'smartparens)
(require 'smartparens-config)

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
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; don't eat my shell
(setq-default comint-prompt-read-only t)

;; disable backup
(setq backup-inhibited t)

;; don't auto-save
(setq auto-save-default nil)

;; ack
(package-require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; java
(progn
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode)))

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


(provide 'j0ni-misc)
