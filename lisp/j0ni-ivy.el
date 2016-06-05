;;; j0ni-ivy.el

(packages-require '(swiper ivy flx counsel avy))

(ivy-mode 1)
(diminish 'ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq ivy-height 10)
(setq ivy-length 100)
;; (setq ivy-count-format "")

(avy-setup-default)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Hmmm... I doubt I'll ever use these
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; (global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c s") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; I have no use for dired
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-done)

(setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))

;; w00t
(setq ivy-extra-directories nil)

;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-plus)))

;; set elsewhere
;; (setq magit-completing-read-function 'ivy-completing-read)
;; (setq projectile-completion-system 'ivy)

;; https://github.com/abo-abo/swiper/issues/419
(advice-add 'swiper--action :after (lambda (throwaway-arg)
                                     (recenter)))

(provide 'j0ni-ivy)
