;;; j0ni-ivy.el

(use-package smex)

(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1))

(use-package company-prescient
  :config
  (company-prescient-mode 1))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  ;; (setq ivy-height 20)
  (setq ivy-length 200)
  ;; (setq ivy-count-format "")
  (setq confirm-nonexistent-file-or-buffer t)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  ;; w00t
  (setq ivy-extra-directories nil)

  :config
  (ivy-mode t)

  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)))

(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-top-center))))

(use-package swiper
  :bind (("C-s" . swiper))
  :config
  ;; https://github.com/abo-abo/swiper/issues/419
  (advice-add 'swiper--action :after (lambda (throwaway-arg)
                                       (recenter))))

(use-package counsel
  :diminish nil
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)

         ;; Hmmm... I doubt I'll ever use these
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)

         ("M-y" . counsel-yank-pop)

         ;; ("C-c j" . counsel-git)
         ;; ("C-c g" . counsel-git-grep)
         ;; ("C-c a g" . counsel-ag)
         ("C-x l" . counsel-locate)
         ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

         ("C-\\" . counsel-company)
         ("C-," . counsel-imenu)))

;; I have no use for dired
;; (define-key ivy-minibuffer-map (kbd "RET") 'ivy-done)

;; externals
(setq magit-completing-read-function 'ivy-completing-read)
(setq projectile-completion-system 'ivy)

(use-package ivy-rich
  :init
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 40))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 30 :face warning))
            (ivy-rich-switch-buffer-project (:width 20 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.4))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-find-file
          (:columns
           ((ivy-read-file-transformer)
            (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 45))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 40))
            (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:face font-lock-doc-face))))))

  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode t))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(use-package imenu-anywhere
  :bind (("C-." . imenu-anywhere)))

(provide 'j0ni-ivy)
