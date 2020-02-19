;;; j0ni-gui.el --- stuff for GUI only

(use-package eyebrowse
  :config (eyebrowse-mode 1))

(when (fboundp 'fringe-mode)
  (fringe-mode 8))

(use-package indent-guide
  :init
  (setq indent-guide-char ":")
  (setq indent-guide-recursive nil))

(use-package zoom
  :init
  (setq zoom-size '(0.618 . 0.618))
  (setq temp-buffer-resize-mode t)
  :config
  (zoom-mode 1))


(use-package beacon
  :config
  (beacon-mode 1))

(require 'color)

;; I keep switching between dark and light themes; dark is nice, there are
;; more usable variants, but light is better for my eyes I think. Light looks
;; a bit garish with flux. So let's put the things I always have to change in
;; one place and make it magic.
(defun theme-is-one-of (themes)
  (when (boundp 'j0ni-theme)
    (seq-some (lambda (theme) (eq theme j0ni-theme)) themes)))

(defun set-indent-guide-face (color)
  (interactive)
  (if (featurep 'indent-guide)
      (set-face-foreground 'indent-guide-face color)
    (eval-after-load 'indent-guide
      `(set-face-foreground 'indent-guide-face ,color))))

(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$"  ; reload tramp paths
      desktop-load-locked-desktop nil)

(defun boot-desktop ()
  "Load the desktop and enable autosaving"
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (desktop-read desktop-dirname)
    (desktop-save-mode 1)))

(defun set-font-dwim (&rest size font ln-spc antialias weight)
  (interactive)
  (let* ((ln-spc (or ln-spc j0ni-line-spacing))
         (font (or font j0ni-font-face))
         (size (or size j0ni-font-size))
         (antialias (or antialias j0ni-antialias))
         (weight (or weight j0ni-font-weight))
         (font-name (concat font "-" (format "%d" size)))
         (spec (font-spec :weight weight
                          :name font-name
                          :antialias antialias)))
    (set-frame-font spec nil t)
    (setq-default line-spacing ln-spc)
    ;; (setq-default cursor-type 'box)
    font-name))

(defun j0ni-inc-font-size ()
  (interactive "*")
  (setq j0ni-font-size (+ j0ni-font-size 1))
  (set-font-dwim))
(defun j0ni-dec-font-size ()
  (interactive "*")
  (setq j0ni-font-size (- j0ni-font-size 1))
  (set-font-dwim))

(define-key global-map (kbd "C-+") 'j0ni-inc-font-size)
(define-key global-map (kbd "C-=") 'j0ni-inc-font-size)
(define-key global-map (kbd "C--") 'j0ni-dec-font-size)

;; (use-package minions
;;   :config
;;   (minions-mode 1))

(use-package doom-themes
  :init
  (setq doom-outrun-electric-brighter-modeline nil)
  (setq doom-outrun-electric-brighter-comments nil)
  (setq doom-outrun-electric-comment-bg nil)
  (setq doom-themes-enable-bold t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package dracula-theme :no-require t)
(use-package zenburn-theme :no-require t)
(use-package reverse-theme :no-require t)
(use-package leuven-theme :no-require t)
(use-package gruvbox-theme :no-require t)
(use-package zerodark-theme :no-require t)

(use-package redo-plus
  :no-require t
  :init
  (require 'redo+))

(use-package find-file-in-project)
(use-package all-the-icons)
(use-package ghub
  :config (require 'ghub))
(use-package async
  :config (require 'async))
(use-package doom-modeline
  :init
  ;; Nope
  (setq doom-modeline-major-mode-icon nil)
  ;; (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-icon nil)
  (setq doom-modeline-height 25)
  (setq doom-modeline-github t)
  (setq doom-modeline-vcs-max-length 30)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-lsp t)
  ;; or `find-in-project' if it's installed
  (setq doom-modeline-project-detection 'ffip)

  :config
  (doom-modeline-def-modeline 'mhcat-line
    '(bar
      workspace-name
      window-number
      modals
      matches
      buffer-info
      remote-host
      buffer-position
      word-count
      parrot
      selection-info)
    '(objed-state
      misc-info
      persp-name
      grip
      irc
      mu4e
      gnus
      github
      debug
      lsp
      minor-modes
      input-method
      indent-info
      buffer-encoding
      major-mode
      process
      vcs
      checker))

  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'mhcat-line 'default))

  ;; (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
  )

(use-package rainbow-mode
  :init
  (setq rainbow-html-colors nil)
  (setq rainbow-x-colors nil)
  :hook
  (html-mode nxhtml-mode nxhtml-mumamo-mode))

(use-package solaire-mode
  :hook
  (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
   (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-global-mode 1)
  (solaire-mode-swap-bg))

(straight-use-package 'hl-line-plus)
(hl-line-toggle-when-idle)
(global-set-key (kbd "C-`") 'hl-line-flash)

(defvar j0ni-theme)
(defvar j0ni-light-theme)
(defvar j0ni-dark-theme)

(use-package circadian
  :after (doom-modeline)
  :commands (circadian-setup)
  :init
  ;; (setq j0ni-light-theme `(leuven light SlateGray2 (100 . 100)))
  (setq j0ni-light-theme `(doom-nord-light light SlateGray2 (100 . 100)))
  (setq j0ni-dark-theme `(doom-wilmersdorf respectful grey30 (100 . 100)))
  ;; (setq j0ni-dark-theme `(doom-outrun-electric respectful grey30 (100 . 100)))
  (setq j0ni-light-theme `(doom-nova respectful grey30 (100 . 100)))
  ;; (setq j0ni-dark-theme `(doom-nord respectful grey30 (100 . 100)))

  ;; Leeds
  ;; (setq calendar-latitude 53.835711)
  ;; (setq calendar-longitude -1.509389)
  ;; (setq calendar-location-name "Leeds")
  ;; Osaka
  ;; (setq calendar-latitude 34.69374)
  ;; (setq calendar-longitude 135.50218)
  ;; (setq calendar-location-name "Osaka")
  ;; Toronto
  (setq calendar-latitude 43.671780)
  (setq calendar-longitude -79.322891)
  (setq calendar-location-name "Toronto, Canada")

  (setq circadian-themes `((:sunrise . ,(cl-first j0ni-light-theme))
                           ;; ("08:00" . ,(cl-first j0ni-dark-theme))
                           (:sunset  . ,(cl-first j0ni-dark-theme))))

  ;; note that this executes in a black hole, so if it fails, there will be no
  ;; messaging, or indication as to where it failed.
  (defun after-circadian-load-theme (&optional theme)
    (doom-modeline-mode t)
    (set-font-dwim)
    (message "end of circadian hook"))

  (add-hook 'circadian-after-load-theme-hook #'after-circadian-load-theme))

(circadian-setup)

;; (use-package celestial-mode-line
;;   :after (:all circadian smart-mode-line)
;;   :init
;;   (require 'solar)
;;   :config
;;   (if (null global-mode-string)
;;       (setq global-mode-string '("" celestial-mode-line-string))
;;     (append global-mode-string '(celestial-mode-line-string)))
;;   (celestial-mode-line-start-timer))

;; (use-package smart-mode-line
;;   :init
;;   (setq sml/position-percentage-format "%p")
;;   (setq sml/theme 'light)
;;   :config
;;   ;; for default theme
;;   ;; (set-indent-guide-face "grey80")
;;   ;; (sml/setup)
;;   )

(use-package dash)
(use-package tco)

(use-package fold-dwim
  :commands
  (fold-dwim-toggle fold-dwim-hide-all fold-dwim-show-all)

  :init
  (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
  (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
  (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)
  ;; (global-set-key (kbd "<M-`>") #'fold-dwim-toggle)
  ;; (global-set-key (kbd "<M-~>") #'fold-dwim-show-all)
  )

;; take care of stupid eww behaviour
(advice-add
 #'shr-colorize-region
 :around (defun shr-no-colourise-region (&rest ignore)))

(use-package darkroom)

(provide 'j0ni-gui)
