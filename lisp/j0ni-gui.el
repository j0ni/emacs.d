;;; j0ni-gui.el --- stuff for GUI only

;; I guess this kind of counts as GUI
;; (package-require 'eyebrowse)
;; (eyebrowse-mode t)

;; (packages-require j0ni-installed-themes)

(if (fboundp 'fringe-mode)
    (fringe-mode 8))

;; (custom-theme-set-faces
;;  '(mode-line-inactive ((t :inverse-video nil)))
;;  '(mode-line     ((t :inverse-video nil)))
;;  '(sml/global    ((t :inherit font-lock-preprocessor-face)))
;;  '(sml/filename  ((t :inherit mode-line-buffer-id)))
;;  '(sml/prefix    ((t :inherit (font-lock-variable-name-face sml/global))))
;;  '(sml/read-only ((t :inherit (font-lock-type-face sml/not-modified))))
;;  '(sml/modes     ((t :foreground nil :inherit sml/filename :weight
;;  normal))))

(use-package indent-guide
  :init
  (setq indent-guide-char ":")
  (setq indent-guide-recursive nil))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1)
  (add-hook 'buffer-list-update-hook #'golden-ratio)
  (eval-after-load 'j0ni-misc
    '(progn
       (add-to-list 'golden-ratio-extra-commands 'window-number-switch)
       (add-to-list 'golden-ratio-extra-commands 'window-number-select)))

  :init
  ;; somehow this breaks with a single vertical split when set
  ;; see https://github.com/roman/golden-ratio.el/issues/55
  (setq golden-ratio-auto-scale nil))

(require 'color)

(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat nil)
  (setq nyan-wavy-trail nil)

  ;; :config
  ;; (nyan-mode 1)
  )

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

;; (packages-require j0ni-installed-themes)
;; (require 'moe-theme)
;; (setq moe-theme-highlight-buffer-id t)

;; (moe-theme-set-color 'white)
;; (moe-dark)
;; (require 'lawrence-theme)

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
    font-name))

(defun set-mode-line-box ()
  "Makes a nice popout box around the mode line."
  (interactive)
  (set-face-attribute 'mode-line nil :box '(:style released-button))
  (set-face-attribute 'mode-line-inactive nil :box '(:style released-button)))

(defun j0ni-inc-font-size ()
  (interactive "*")
  (setq j0ni-font-size (+ j0ni-font-size 1))
  (set-font-dwim))
(defun j0ni-dec-font-size ()
  (interactive "*")
  (setq j0ni-font-size (- j0ni-font-size 1))
  (set-font-dwim))

(define-key global-map (kbd "C-+") 'j0ni-inc-font-size)
(define-key global-map (kbd "C--") 'j0ni-dec-font-size)

(defvar base-face-list nil)

(defun base-weight (face)
  (let ((original-weight (alist-get face base-face-list 'not-found)))
    (if (not (equal 'not-found original-weight))
        original-weight
      ;; (message "Setting %s weight to %s" face (face-attribute face :weight))
      (setq base-face-list
            (cons `(,face . ,(face-attribute face :weight)) base-face-list))
      (alist-get face base-face-list))))

(defun apply-font-settings (&optional default-font antialias)
  (interactive)
  (let ((font-name (or default-font (set-font-dwim))))
    (mapc
     (lambda (face)
       (cond ;; do nothing if the weight is neither of these - preserving
        ;; inherited properties
        ((equal 'bold (base-weight face))
         (set-face-attribute
          face nil
          :weight j0ni-bold-font-weight
          :width 'semi-condensed
          :font (font-spec :weight 'regular :name font-name
                           :antialias (or antialias j0ni-antialias))))

        ((equal 'normal (base-weight face))
         (set-face-attribute
          face nil
          :weight j0ni-font-weight
          :width 'semi-condensed
          :font (font-spec :weight 'light :name font-name
                           :antialias (or antialias j0ni-antialias))))))
     (face-list))))

;; run the setup
;; (set-font-dwim)
;; (normalize-fonts)

(defun initialize-and-load-theme (theme &optional indent-guide-color sml-theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))
    (read-color "Indent guide color: ")))
  (let ((sml-theme (or sml-theme 'respectful)))
    (load-theme theme t)
    (eval-after-load "j0ni-gui"
      `(progn
         (setq sml/theme ,(symbol-name sml-theme))
         (sml/setup)
         (set-font-dwim)
         (when ,indent-guide-color
           (set-indent-guide-face ,indent-guide-color)))))
  ;; (eval-after-load 'highlight-symbol
  ;;   '(set-face-attribute 'highlight-symbol-face nil :box t))
  )

(use-package zerodark-theme :no-require t)
(use-package one-themes :no-require t)
(use-package synthwave-theme
  :straight (:host github
             :repo "TroyFletcher/emacs-synthwave-theme"
             :fork (:host github
                    :repo "j0ni/emacs-synthwave-theme"
                    :branch "j0ni/autoloads")))

(use-package doom-themes
  :init
  (setq doom-outrun-electric-brighter-modeline nil)
  (setq doom-outrun-electric-brighter-comments nil)
  (setq doom-outrun-electric-comment-bg nil)
  )

(use-package color-theme-sanityinc-tomorrow)

(use-package minions)

(use-package paper-theme)

(when (boundp 'j0ni-theme)
  ;; Solarized specific tweaks

  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)

  ;; make the modeline high contrast
  ;; (setq solarized-high-contrast-mode-line t)

  ;; Use less bolding
  (setq solarized-use-less-bold t)

  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  ;; (setq solarized-emphasize-indicators nil)

  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)

  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1)
  (setq solarized-height-plus-1 1)
  (setq solarized-height-plus-2 1)
  (setq solarized-height-plus-3 1)
  (setq solarized-height-plus-4 1)

  ;; Spacemacs specific tweaks
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil)

  ;; Noctilux

  ;; "For test purposes only; when in GUI mode, forces Noctilux to use the 256
  ;; degraded color mode to test the approximate color values for accuracy."
  (setq noctilux-degrade nil)

  ;; "Sets the level of highlighting to use in diff-like modes. '(high normal low)"
  (setq noctilux-diff-mode 'normal)

  ;; "Stops Noctilux from displaying bold when nil."
  (setq noctilux-bold t)

  ;; "Stops Noctilux from displaying underlines when nil."
  (setq noctilux-underline t)

  ;; "Stops Noctilux from displaying italics when nil."
  (setq noctilux-italic nil)

  ;; "Stick with normal! It's been carefully tested. Setting this option to high
  ;; or low does use the same Noctilux palette but simply shifts some values up
  ;; or down in order to expand or compress the tonal range displayed." '(high normal low)
  (setq noctilux-contrast 'normal)

  ;; (custom-set-faces
  ;;  '(show-paren-match ((t (:background "#5f5f5f")))))

  ;; some customizations
  ;; (set-face-attribute 'eval-sexp-fu-flash ((t (:background "#101010" :foreground "white"))))
  ;; (set-face-attribute 'nrepl-eval-sexp-fu-flash ((t (:background "#101010" :foreground "white"))))
  ;; (custom-set-faces
  ;;  '(rainbow-delimiters-depth-9-face ((t (:foreground "#7fff7f"))))
  ;;  '(rainbow-delimiters-depth-8-face ((t (:foreground "#5fdf5f"))))
  ;;  '(rainbow-delimiters-depth-7-face ((t (:foreground "#3fbf3f"))))
  ;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "#1f9f1f"))))
  ;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "#7fff7f"))))
  ;;  '(rainbow-delimiters-depth-4-face ((t (:foreground "#5fdf5f"))))
  ;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "#3fbf3f"))))
  ;;  '(rainbow-delimiters-depth-2-face ((t (:foreground "#1f9f1f"))))
  ;;  '(rainbow-delimiters-depth-1-face ((t (:foreground "#7fff7f"))))
  ;;  '(eval-sexp-fu-flash ((t (:foreground "green"))))
  ;;  '(nrepl-eval-sexp-fu-flash ((t (:foreground "green"))))
  ;;  '(hl-sexp-face ((t (:background "black"))))
  ;;  '(git-gutter:separator ((t (:background "black"))))
  ;;  '(git-gutter:modified ((t (:background "black"))))
  ;;  '(git-gutter:added ((t (:background "black"))))
  ;;  '(git-gutter:deleted ((t (:background "black"))))
  ;;  '(git-gutter:unchanged ((t (:background "black"))))
  ;;  )

  ;; company-mode hack
  ;; (let ((bg (face-attribute 'default :background)))
  ;;   (custom-set-faces
  ;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
  ;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
  ;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
  ;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
  ;;    `(company-tooltip-common ((t (:inherit font-lock-function-name-face))))
  ;;    `(company-tooltip-common-selection ((t (:inherit font-lock-function-name-face))))))

  ;; clues maybe needs this
  ;;
  ;; (custom-set-faces `(mode-line ((t (:foreground "#777777" :background
  ;;  "#111111" :box nil :height 140 :font ,j0ni-font)))) ;;
  ;;  `(highlight-symbol-face ((t (:underline t :background "orange")))) )
  ;;  (custom-set-faces `(mode-line ((t (:height 140 :font ,j0ni-font)))))
  ;;  (custom-set-faces `(mode-line ((t (:foreground black :background
  ;;  "#333333" :box nil :height 140 :font ,j0ni-font)))))

  ;; (custom-set-faces
  ;;  '(racket-keyword-argument-face ((t (:inherit default))))
  ;;  '(racket-paren-face ((t (:inherit default))))
  ;;  '(racket-selfeval-face ((t (:inherit default)))))

  ;; (add-to-list 'default-frame-alist '(height . 40))
  ;; (add-to-list 'default-frame-alist '(width . 120))

  ;; this is my ivy hack for anti-zenburn
  ;; (set-face-background 'ivy-current-match "grey90")
  ;; (set-face-background 'ivy-minibuffer-match-face-1 "grey30")

  ;; (load-theme j0ni-theme)
  ;; (apply-font-settings)

  ;; (normalize-fonts)

  ;; noctilux & fogus hack
  ;; (let ((fg (face-attribute 'font-lock-comment-face :foreground)))
  ;;   (custom-set-faces
  ;;    `(font-lock-doc-face ((t (:foreground ,(color-lighten-name fg 10)))))
  ;;    `(font-lock-comment-face ((t (:foreground ,(color-lighten-name fg 5)))))
  ;;    `(font-lock-comment-delimiter-face ((t (:foreground ,(color-lighten-name fg 5)))))))

  ;; avoid javascript default funcion param name setting
  ;; (let ((fg (face-attribute 'font-lock-variable-name-face :foreground)))
  ;;   (custom-set-faces
  ;;    `(js2-function-param ((t (:foreground ,fg))))))
  )

;; (global-hl-line-mode 1)
;; (global-display-fill-column-indicator-mode)

(use-package hemisu-theme :no-require t)
(use-package dracula-theme :no-require t)
(use-package chocolate-theme :no-require t)
(use-package zenburn-theme :no-require t)
(use-package cyberpunk-theme :no-require t)
(use-package reverse-theme :no-require t)
(use-package leuven-theme :no-require t)
(use-package nord-theme
  :no-require t
  :init
  (setq nord-comment-brightness 15))
(use-package nova-theme :no-require t)
(use-package spacemacs-theme :no-require t)
(use-package gruvbox-theme :no-require t)
(use-package tangotango-theme :no-require t)
(use-package phoenix-dark-pink-theme :no-require t)
;; alternatively....
;; (load "~/Scratch/emacs/phoenix-dark-mono/phoenix-dark-mono-theme.el")
;; (initialize-and-load-theme 'phoenix-dark-pink)
(use-package plan9-theme :no-require t)
(use-package sunburn-theme :no-require t)
(use-package grayscale-theme :no-require t)
(use-package redo-plus
  :no-require t
  :init
  (require 'redo+))

(defvar j0ni-theme)
(defvar j0ni-light-theme)
(defvar j0ni-dark-theme)
;; (defvar j0ni-theme-tint)

;; (after-circadian-load-theme j0ni-dark-theme)
(use-package powerline)
(use-package airline-themes)
(use-package smart-mode-line-atom-one-dark-theme)
(use-package smart-mode-line-powerline-theme)
(use-package doom-modeline
  :init
  (setq doom-modeline-icon nil)
  (setq doom-modeline-height 1.2)
  (setq doom-modeline-vcs-max-length 30)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  ;; or `find-in-project' if it's installed
  (setq doom-modeline-project-detection 'ffip))

;; (use-package maple-minibuffer
;;   :straight (:host github :repo "honmaple/emacs-maple-minibuffer")
;;   :hook (after-init . maple-minibuffer-mode)
;;   :config
;;   (setq maple-minibuffer:position-type 'window-bottom-left
;;         maple-minibuffer:height nil
;;         maple-minibuffer:border-color "gray50"
;;         maple-minibuffer:width 100)

;;   ;; more custom parameters for frame
;;   (defun maple-minibuffer:parameters ()
;;     "Maple minibuffer parameters."
;;     `((height . ,(or maple-minibuffer:height 10))
;;       (width . ,(or maple-minibuffer:width (window-pixel-width)))
;;       (left-fringe . 5)
;;       (right-fringe . 5))))

(defun sml-hook (theme)
  (lambda ()
    (setq sml/theme theme)
    (sml/setup)))

(defun powerline-hook (theme)
  (lambda ()
    (powerline-default-theme)
    (load-theme theme)))

(use-package rainbow-mode
  :init
  (setq rainbow-html-colors nil)
  (setq rainbow-x-colors nil)
  :hook
  (html-mode nxhtml-mode nxhtml-mumamo-mode))

(use-package dim)
(dim-minor-names
 '((company-mode "" company)
   (auto-revert-mode "" nil)
   (highlight-symbol-mode "" highlight-symbol)
   (whitespace-mode " _" whitespace)
   (ivy-mode "" ivy)
   (elisp-slime-nav-mode "" elisp-slime-nav)
   (ws-butler-mode "" ws-butler)
   (golden-ratio-mode "" golden-ratio)
   (projectile-mode "" projectile)
   (indent-guide-mode "" indent-guide)
   (which-key-mode "" which-key)
   (undo-tree-mode "" undo-tree)
   (hs-minor-mode "" hideshow)
   (paredit-mode " ()" paredit)
   (subword-mode "" subword)
   (cider-mode " cider" cider)
   (auto-revert-mode "" autorevert)
   (drag-stuff-mode "" drag-stuff)
   ;; (tree-sitter-mode "" nil)
   ))

(use-package circadian
  :after (doom-modeline)
  :commands (circadian-setup)
  :init
  (setq j0ni-light-theme `(leuven light SlateGray2 (100 . 100)))
  (setq j0ni-dark-theme `(doom-wilmersdorf respectful grey30 (100 . 100)))

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
                           ("08:00" . ,(cl-first j0ni-dark-theme))
                           (:sunset  . ,(cl-first j0ni-dark-theme))))

  ;; note that this executes in a black hole, so if it fails, there will be no
  ;; messaging, or indication as to where it failed.
  (defun after-circadian-load-theme (&optional theme)
    (interactive)
    (let ((theme (or theme (cl-first j0ni-light-theme)))
          (theme-config (if (eq theme (cl-first j0ni-light-theme))
                            j0ni-light-theme
                          j0ni-dark-theme)))
      (cl-destructuring-bind (theme ml-theme indent-guide-color alpha) theme-config
        ;; (rainbow-delimiters--define-depth-faces)
        (set-frame-parameter (selected-frame) 'alpha alpha)
        (add-to-list 'default-frame-alist `(alpha . ,alpha))
        (set-indent-guide-face (symbol-name indent-guide-color))
        (set-face-foreground 'fill-column-indicator (symbol-name indent-guide-color))
        (doom-modeline-mode)
        (doom-themes-org-config)
        (custom-set-faces
         '(mode-line ((t (:height 1.0))))
         '(mode-line-inactive ((t (:height 1.0)))))
        (set-font-dwim)))

    (message "finished circadian hook"))

  (add-hook 'circadian-after-load-theme-hook #'after-circadian-load-theme)
  (circadian-setup)
  )

(use-package celestial-mode-line
  :after (circadian)
  :init
  (require 'solar)
  :config
  (if (null global-mode-string)
      (setq global-mode-string '("" celestial-mode-line-string))
    (append global-mode-string '(celestial-mode-line-string)))
  (celestial-mode-line-start-timer))

(use-package smart-mode-line
  :init
  (setq sml/position-percentage-format "%p")
  (setq sml/theme nil)
  :config
  ;; for default theme
  ;; (set-indent-guide-face "grey80")
  ;; (sml/setup)
  nil)

(use-package dash)
(use-package tco)
(use-package equake
  :config
  (global-set-key (kbd "C-x C-c") 'equake-check-if-in-equake-frame-before-closing))

(use-package wttrin
  :init
  (setq wttrin-default-cities '("Toronto")))

;; (use-package darkroom)

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

(provide 'j0ni-gui)
