;;; j0ni-sml.el --- smart-mode-line config

;; sml
(package-require 'smart-mode-line)

;;
;; (custom-theme-set-faces
;;  '(mode-line-inactive ((t :inverse-video nil)))
;;  '(mode-line     ((t :inverse-video nil)))
;;  '(sml/global    ((t :inherit font-lock-preprocessor-face)))
;;  '(sml/filename  ((t :inherit mode-line-buffer-id)))
;;  '(sml/prefix    ((t :inherit (font-lock-variable-name-face sml/global))))
;;  '(sml/read-only ((t :inherit (font-lock-type-face sml/not-modified))))
;;  '(sml/modes     ((t :foreground nil :inherit sml/filename :weight normal))))

(sml/setup)

(provide 'j0ni-sml)
