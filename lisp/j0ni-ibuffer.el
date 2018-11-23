;;; j0ni-ibuffer --- ibuffer configuration

(use-package ibuffer-vc
  :bind (("C-x C-b" . ibuffer))
  :init (setq ibuffer-expert t)
  :config
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(provide 'j0ni-ibuffer)
