;;; j0ni-powerline.el

(package-require 'powerline)
(require 'powerline)

(when (display-graphic-p)
  (defun clean-powerline ()
    (interactive)
    (powerline-default-theme)
    (set-face-attribute 'mode-line nil
                        :box nil)
    (set-face-attribute 'mode-line-inactive nil
                        :box nil))

  (clean-powerline))

(provide 'j0ni-powerline)
