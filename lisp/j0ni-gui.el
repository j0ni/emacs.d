;;; j0ni-gui.el --- stuff for GUI only

(when (display-graphic-p)
  (packages-require j0ni-installed-themes)

  ;; first things first
  (defun apply-font-settings ()
    "Apply font choices across the board."
    (interactive)
    (set-face-attribute 'default nil :font j0ni-font)
    (eval-after-load 'linum
      '(set-face-attribute 'linum nil :font j0ni-linum-font)))

  (apply-font-settings)

  (defun set-mode-line-box ()
    "Makes a nice popout box around the mode line."
    (interactive)
    (set-face-attribute 'mode-line nil :box '(:style released-button))
    (set-face-attribute 'mode-line-inactive nil :box '(:style released-button)))

  (defun normalize-fonts ()
    "Removes underlining and bold decorations."
    (interactive)
    (mapc
     (lambda (face)
       (set-face-attribute face nil :weight 'normal :underline nil))
     (face-list)))

  (load-theme j0ni-theme)
  (set-face-foreground 'show-paren-match-face "#ffcfff")
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 120))
  (set-mode-line-box)
  ;; for native fullscreen icon
  (menu-bar-mode +1))

(provide 'j0ni-gui)
