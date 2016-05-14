;;; j0ni-ibuffer --- ibuffer configuration

(package-require 'ibuffer-vc)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-expert t)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(provide 'j0ni-ibuffer)
