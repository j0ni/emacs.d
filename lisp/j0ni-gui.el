;;; j0ni-gui.el --- stuff for GUI only

;; I guess this kind of counts as GUI
;; (package-require 'eyebrowse)
;; (eyebrowse-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 6))

(use-package smart-mode-line
  :init
  (setq sml/position-percentage-format nil))

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
  (setq indent-guide-char "|")
  (setq indent-guide-recursive nil)
  :diminish nil)

(use-package golden-ratio
  :ensure t
  :diminish nil
  :config
  (golden-ratio-mode 1)
  (add-to-list 'buffer-list-update-hook #'golden-ratio)
  :init
  (setq golden-ratio-auto-scale t))

(require 'color)

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))

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

(defun customize-light-or-dark (&optional tint)
  (interactive)
  (unless tint (setf tint j0ni-theme-tint))
  ;; (message (prin1-to-string tint))
  (cond
   ((eq 'light tint)
    (progn
      (if (boundp 'j0ni-theme)
          (cond ((theme-is-one-of '(moe-light
                                    moe
                                    moe-theme
                                    eziam-light))
                 (setq sml/theme nil))
                ((theme-is-one-of '(spacemacs-light
                                    solarized-light
                                    spacemacs-light
                                    tao-yang))
                 (setq sml/theme 'respectful))
                (t
                 (setq sml/theme 'light)))
        (setq sml/theme nil))))

   ((eq 'mid tint)
    (setq sml/theme 'respectful))

   (t
    (progn ;; default to dark
      (if (theme-is-one-of '(base16-greenscreen-dark
                             solarized-theme
                             moe-dark
                             tango-dark))
          (setq sml/theme nil)
        (setq sml/theme 'respectful)))))

  (unless (theme-is-one-of '(phoenix-dark-pink))
    (set-indent-guide-face (pcase tint
                             ('light "gray80")
                             ('mid "gray70")
                             (_ "gray30"))))
  (sml/setup)
  (apply-font-settings)
  (set-mode-line-box))

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

(packages-require j0ni-installed-themes)
;; (require 'moe-theme)
;; (setq moe-theme-highlight-buffer-id t)

;; (moe-theme-set-color 'white)
;; (moe-dark)
;; (require 'lawrence-theme)

(defun set-font-dwim (&optional size font ln-spc antialias)
  (interactive)
  (when (display-graphic-p)
    (let ((ln-spc (or ln-spc j0ni-line-spacing))
          (font (or font j0ni-font-face))
          (size (or size j0ni-font-size))
          (antialias (or antialias j0ni-antialias)))
      (setq j0ni-default-font (concat font "-" (format "%d" size)))
      (setq-default line-spacing ln-spc)
      (apply-font-settings))))

(defun set-mode-line-box ()
  "Makes a nice popout box around the mode line."
  (interactive)
  (set-face-attribute 'mode-line nil :box '(:style released-button))
  (set-face-attribute 'mode-line-inactive nil :box '(:style released-button)))

(when (display-graphic-p)
  ;; Only define these fns if we have a GUI, but make them reusable
  (defun j0ni-inc-font-size ()
    (interactive "*")
    (setq j0ni-font-size (+ j0ni-font-size 1))
    (set-font-dwim))
  (defun j0ni-dec-font-size ()
    (interactive "*")
    (setq j0ni-font-size (- j0ni-font-size 1))
    (set-font-dwim))

  (global-set-key (kbd "C-+") 'j0ni-inc-font-size)
  (global-set-key (kbd "C--") 'j0ni-dec-font-size)

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
    (mapc
     (lambda (face)
       (cond ;; do nothing if the weight is neither of these - preserving
        ;; inherited properties
        ((equal 'bold (base-weight face))
         (set-face-attribute
          face nil
          :weight j0ni-bold-font-weight
          :font (font-spec :name (or default-font j0ni-default-font)
                           :antialias (or antialias j0ni-antialias))))

        ((equal 'normal (base-weight face))
         (set-face-attribute
          face nil
          :weight j0ni-font-weight
          :font (font-spec :name (or default-font j0ni-default-font)
                           :antialias (or antialias j0ni-antialias))))))
     (face-list)))

  ;; run the setup
  ;; (set-font-dwim)
  ;; (normalize-fonts)
  )

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

  (load-theme j0ni-theme)
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

(when (boundp 'j0ni-theme-tint)
  (customize-light-or-dark j0ni-theme-tint))

;; (global-hl-line-mode 1)

;; take care of stupid eww behaviour
(advice-add
 #'shr-colorize-region
 :around (defun shr-no-colourise-region (&rest ignore)))

(provide 'j0ni-gui)
