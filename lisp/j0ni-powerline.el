;;; j0ni-powerline.el

(when (display-graphic-p)
  (package-require 'powerline)
  (require 'powerline)

  (defun clean-powerline ()
    (interactive)
    (powerline-default-theme)
    (set-face-attribute 'mode-line nil
                        :box nil)
    (set-face-attribute 'mode-line-inactive nil
                        :box nil))

  ;; (clean-powerline)

  (powerline-default-theme))

(provide 'j0ni-powerline)
