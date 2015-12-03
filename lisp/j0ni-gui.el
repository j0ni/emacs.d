;;; j0ni-gui.el --- stuff for GUI only

(when (display-graphic-p)
  (packages-require j0ni-installed-themes)

  ;; (require 'lawrence-theme)
  ;; (blink-cursor-mode +1)

  ;; first things first
  (defun apply-font-settings ()
    "Apply font choices across the board."
    (interactive)
    (set-face-attribute 'default nil :font j0ni-font)
    (eval-after-load 'linum
      '(set-face-attribute 'linum nil :font j0ni-linum-font)))

  (defun set-mode-line-box ()
    "Makes a nice popout box around the mode line."
    (interactive)
    (set-face-attribute 'mode-line nil :box '(:style released-button))
    (set-face-attribute 'mode-line-inactive nil :box '(:style released-button)))

  (defun normalize-fonts ()
    "Removes underlining and bold decorations."
    (interactive)
    (mapc
     (lambda (face) (set-face-attribute face nil :weight 'normal :underline nil))
     (face-list)))

  (require 'color)

  (when (boundp 'j0ni-theme)
    ;;; Solarized specific tweaks

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

    (load-theme j0ni-theme)

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

    ;; noctilux & fogus hack
    ;; (let ((fg (face-attribute 'font-lock-comment-face :foreground)))
    ;;   (custom-set-faces
    ;;    `(font-lock-doc-face ((t (:foreground ,(color-lighten-name fg 10)))))
    ;;    `(font-lock-comment-face ((t (:foreground ,(color-lighten-name fg 5)))))
    ;;    `(font-lock-comment-delimiter-face ((t (:foreground ,(color-lighten-name fg 5)))))))

    ;; fogus stuff for javascript
    ;; (let ((fg (face-attribute 'font-lock-variable-name-face :foreground)))
    ;;   (custom-set-faces
    ;;    `(js2-function-param ((t (:foreground ,fg))))))
    )

  ;; company-mode hack
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common-selection ((t (:inherit font-lock-function-name-face))))))

  ;; clues maybe needs this
  ;;
  ;; (custom-set-faces
  ;;  `(mode-line ((t (:foreground "#777777" :background "#111111" :box nil :height 140 :font ,j0ni-font))))
  ;;  ;; `(highlight-symbol-face ((t (:underline t :background "orange"))))
  ;;  )
  ;; (custom-set-faces
  ;;  `(mode-line ((t (:height 140 :font ,j0ni-font)))))
  ;; (custom-set-faces
  ;;  `(mode-line ((t (:foreground black  :background "#333333" :box
  ;;nil :height 140 :font ,j0ni-font)))))

  (custom-set-faces
   '(racket-keyword-argument-face ((t (:inherit default))))
   '(racket-paren-face ((t (:inherit default))))
   '(racket-selfeval-face ((t (:inherit default)))))

  (add-to-list 'default-frame-alist '(height . 40))
  (add-to-list 'default-frame-alist '(width . 120))

  (apply-font-settings)
  (normalize-fonts)
  ;; (set-mode-line-box)
  ;; for native fullscreen icon
  (menu-bar-mode +1)
  ;; (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
  ;; (remove-hook 'text-mode-hook 'esk-turn-on-hl-line-mode)

  (setq linum-format "%d"))

(provide 'j0ni-gui)
