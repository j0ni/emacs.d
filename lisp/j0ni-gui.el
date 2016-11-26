;;; j0ni-gui.el --- stuff for GUI only

;; I guess this kind of counts as GUI
(package-require 'eyebrowse)
(eyebrowse-mode t)

(package-require 'smart-mode-line)

;; (custom-theme-set-faces
;;  '(mode-line-inactive ((t :inverse-video nil)))
;;  '(mode-line     ((t :inverse-video nil)))
;;  '(sml/global    ((t :inherit font-lock-preprocessor-face)))
;;  '(sml/filename  ((t :inherit mode-line-buffer-id)))
;;  '(sml/prefix    ((t :inherit (font-lock-variable-name-face sml/global))))
;;  '(sml/read-only ((t :inherit (font-lock-type-face sml/not-modified))))
;;  '(sml/modes     ((t :foreground nil :inherit sml/filename :weight
;;  normal))))

;; indent-guide - don't switch it on though, just have it there fore
;; when I need it
(package-require 'indent-guide)
(eval-after-load 'indent-guide
  '(progn
     (setq indent-guide-char ":")
     (setq indent-guide-recursive nil)
     (diminish 'indent-guide-mode)))

(require 'color)

;; I keep switching between dark and light themes; dark is nice, there are
;; more usable variants, but light is better for my eyes I think. Light looks
;; a bit garish with flux. So let's put the things I always have to change in
;; one place and make it magic.
(defun theme-is-one-of (themes)
  (seq-some (lambda (theme) (eq theme j0ni-theme)) themes))

(defun set-indent-guide-face (color)
  (interactive)
  (if (featurep 'indent-guide)
      (set-face-foreground 'indent-guide-face color)
    (eval-after-load 'indent-guide
      `(set-face-foreground 'indent-guide-face ,color))))

(defun customize-light-or-dark (tint)
  (message "tint is" tint)
  (cond
   ((eq 'light tint)
    (progn
      (if (boundp 'j0ni-theme)
          (cond ((theme-is-one-of '(moe-light
                                    moe
                                    moe-theme))
                 (setq sml/theme nil))
                ((theme-is-one-of '(spacemacs-light))
                 (setq sml/theme 'respectful))
                (t
                 (setq sml/theme 'light)))
        (setq sml/theme nil))
      (set-indent-guide-face "gray80")))

   ((eq 'mid tint)
    (progn
      (setq sml/theme 'respectful)
      (set-indent-guide-face "gray70")))

   (t
    (progn ;; default to dark
      (if (theme-is-one-of '(base16-greenscreen-dark
                             solarized-theme
                             moe-dark
                             tango-dark))
          (setq sml/theme nil)
        (setq sml/theme 'respectful))
      (set-indent-guide-face "gray30"))))
  (sml/setup)
  (apply-font-settings))

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
    (desktop-read)
    (desktop-save-mode 1)))

(packages-require j0ni-installed-themes)
(require 'moe-theme)
(setq moe-theme-highlight-buffer-id t)

;; (moe-theme-set-color 'white)
;; (moe-dark)
;; (require 'lawrence-theme)
;; (blink-cursor-mode +1)

(when (display-graphic-p)
  ;; Only define these fns if we have a GUI, but make them reusable
  (defun apply-font-settings ()
    "Apply font choices across the board."
    (interactive)
    (set-face-attribute 'default nil :font (font-spec :name j0ni-font
                                                      :antialias t))
    (eval-after-load 'linum
      '(set-face-attribute 'linum nil :font (font-spec :name j0ni-font
                                                       :antialias t))))

  (defun set-mode-line-box ()
    "Makes a nice popout box around the mode line."
    (interactive)
    (set-face-attribute 'mode-line nil :box '(:style released-button))
    (set-face-attribute 'mode-line-inactive nil :box '(:style released-button)))

  (defun normalize-fonts ()
    "Removes underlining and bold decorations."
    (interactive)
    (mapc
     (lambda (face) (set-face-attribute face nil
                                   :weight 'normal
                                   :underline nil))
     (face-list)))

  ;; run the setup
  (apply-font-settings)
  (normalize-fonts))

;; (package-require 'nyan-mode)
;; doesn't do small very well
;; (setq nyan-bar-length 24)
;; (nyan-mode 1)

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

  ;; ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1)
  (setq solarized-height-plus-1 1)
  (setq solarized-height-plus-2 1)
  (setq solarized-height-plus-3 1)
  (setq solarized-height-plus-4 1)

  ;; (set-face-foreground 'show-paren-match-face "red")
  ;; (set-face-background 'show-paren-match-face "grey30")

  ;; some customizations
  ;; (set-face-attribute 'eval-sexp-fu-flash ((t (:background "#101010" :foreground "white"))))
  ;; (set-face-attribute 'nrepl-eval-sexp-fu-flash ((t (:background "#101010" :foreground "white"))))
  (custom-set-faces
   ;; '(rainbow-delimiters-depth-9-face ((t (:foreground "#7fff7f"))))
   ;; '(rainbow-delimiters-depth-8-face ((t (:foreground "#5fdf5f"))))
   ;; '(rainbow-delimiters-depth-7-face ((t (:foreground "#3fbf3f"))))
   ;; '(rainbow-delimiters-depth-6-face ((t (:foreground "#1f9f1f"))))
   ;; '(rainbow-delimiters-depth-5-face ((t (:foreground "#7fff7f"))))
   ;; '(rainbow-delimiters-depth-4-face ((t (:foreground "#5fdf5f"))))
   ;; '(rainbow-delimiters-depth-3-face ((t (:foreground "#3fbf3f"))))
   ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "#1f9f1f"))))
   ;; '(rainbow-delimiters-depth-1-face ((t (:foreground "#7fff7f"))))
   ;; '(eval-sexp-fu-flash ((t (:foreground "green"))))
   ;; '(nrepl-eval-sexp-fu-flash ((t (:foreground "green"))))
   ;; '(hl-sexp-face ((t (:background "black"))))
   ;; '(git-gutter:separator ((t (:background "black"))))
   ;; '(git-gutter:modified ((t (:background "black"))))
   ;; '(git-gutter:added ((t (:background "black"))))
   ;; '(git-gutter:deleted ((t (:background "black"))))
   ;; '(git-gutter:unchanged ((t (:background "black"))))
   )



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

  ;; (set-mode-line-box)
  ;; for native fullscreen icon
  ;; (menu-bar-mode +1)
  ;; (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
  ;; (remove-hook 'text-mode-hook 'esk-turn-on-hl-line-mode)

  ;; this is my ivy hack for anti-zenburn
  ;; (set-face-background 'ivy-current-match "grey90")
  ;; (set-face-background 'ivy-minibuffer-match-face-1 "grey30")

  (load-theme j0ni-theme)

  ;; noctilux & fogus hack
  (let ((fg (face-attribute 'font-lock-comment-face :foreground)))
    (custom-set-faces
     `(font-lock-doc-face ((t (:foreground ,(color-lighten-name fg 10)))))
     `(font-lock-comment-face ((t (:foreground ,(color-lighten-name fg 5)))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,(color-lighten-name fg 5)))))))

  ;; avoid javascript default funcion param name setting
  ;; (let ((fg (face-attribute 'font-lock-variable-name-face :foreground)))
  ;;   (custom-set-faces
  ;;    `(js2-function-param ((t (:foreground ,fg))))))
  )

(when (boundp 'j0ni-theme-tint)
  (customize-light-or-dark j0ni-theme-tint))

(setq linum-format "%d")
;; (global-hl-line-mode 1)

(provide 'j0ni-gui)
