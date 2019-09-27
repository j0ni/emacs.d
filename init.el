;;; init.el --- user init file -*- no-byte-compile: t -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org-plus-contrib)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Good chance this is what I want :P
(progn
  ;; new
  (defvar j0ni-font-face)
  (defvar j0ni-font-size)
  (defvar j0ni-line-spacing)
  (defvar j0ni-antialias)
  (defvar j0ni-font-weight)
  (defvar j0ni-bold-font-weight)
  ;; old
  (defvar j0ni-default-font)
  ;; (setq j0ni-font-face "Consolas")
  ;; (setq j0ni-font-face "iosevka term")
  ;; (setq j0ni-font-face "Droid Sans Mono Dotted for Powerline")
  ;; (setq j0ni-font-face "Hack")
  ;; (setq j0ni-font-face "Liberation Mono")
  ;; (setq j0ni-font-face "Linux Libertine Mono")
  ;; (setq j0ni-font-face "Fira Mono Medium")
  ;; (setq j0ni-font-face "Fira Mono")
  ;; (setq j0ni-font-face "Fira Code")
  ;; (setq j0ni-font-face "Fira Code Light")
  (setq j0ni-font-face "Envy Code R")
  ;; (setq j0ni-font-face "Agave")
  ;; (setq j0ni-font-face "Source Code Variable")
  ;; (setq j0ni-font-face "Courier Prime Code")
  ;; (setq j0ni-font-face "Operator Mono Book")
  (setq j0ni-font-face "Lucida Console Patched")
  ;; (setq j0ni-font-face "Terminus (TTF)")
  ;; (setq j0ni-font-face "DejaVu Sans Mono")
  ;; (setq j0ni-font-face "Noto Sans Mono")
  ;; (setq j0ni-font-face "Linux Biolinum G")
  ;; (setq j0ni-font-face "Lucida Grande Mono Nrw")
  ;; (setq j0ni-font-face "Operator Mono")
  ;; (setq j0ni-font-face "Lucida Grande Mono")
  ;; (setq j0ni-font-face "Monoid")
  ;; (setq j0ni-font-face "PragmataPro Mono")
  ;; (setq j0ni-font-face "Inconsolata")
  ;; (setq j0ni-font-face "Mensch")
  ;; (setq j0ni-font-face "M+ 1M")
  ;; (setq j0ni-font-face "Go Mono")
  ;; (setq j0ni-font-face "Iosevka")
  ;; (setq j0ni-font-face "IBM Plex Mono")
  (setq j0ni-font-weight 'regular)
  ;; (setq j0ni-bold-font-weight 'regular)
  (setq j0ni-font-size 11)
  ;; (setq j0ni-line-spacing 8)
  (setq j0ni-line-spacing 0)
  (setq j0ni-antialias t)

  (setq j0ni-default-font "Lucida Grande Mono Nrw-11")

  (when (fboundp 'set-font-dwim)
    (set-font-dwim)))

;; (set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;; (set-frame-parameter (selected-frame) 'alpha <both>)
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 50)))

(save-place-mode 1)

;; (setq screen-gamma 1.5)
;; (setq alpha 50)
;; (setq background-mode 'light)

(eval-when-compile
  (require 'cl-lib))

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(eval-after-load "j0ni-gui"
  '(add-hook 'prog-mode-hook #'set-font-dwim))

(add-hook 'window-setup-hook #'(lambda ()
                                 (when (fboundp 'set-font-dwim)
                                   (set-font-dwim))))

(defun insert-shrug ()
  (interactive)
  (insert "¯\\_(ツ)_/¯"))
(global-set-key (kbd "C-c M-C-s") 'insert-shrug)

;; Get here more easily
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
(global-set-key (kbd "C-c C-i") 'open-init-file)

(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    '(add-to-list 'byte-compile-not-obsolete-funcs
                  'preceding-sexp)))

;; Apparently mu4e doesn't do well without this
(set-language-environment "UTF-8")

;; load secrets
(load-file "~/.emacs-secrets.el")

;; Suggested by Le Wang, to reduce GC thrash
(setq gc-cons-threshold 20000000)

;; Add .emacs.d/lisp to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(dolist (sdir (directory-files (concat dotfiles-dir "site-lisp") t "\\w+"))
  (when (file-directory-p sdir)
    (add-to-list 'load-path sdir)))

;; Themes we want to install
;; (require 'color-theme-tomorrow)
;; (require 'lawrence-theme)
;; (color-theme-tomorrow-night)

;; (defvar j0ni-installed-themes
;;   '(nord-theme
;;     monotropic-theme
;;     challenger-deep-theme
;;     kaolin-themes
;;     doom-themes
;;     nova-theme
;;     darktooth-theme
;;     sunburn-theme
;;     zenburn-theme
;;     anti-zenburn-theme
;;     noctilux-theme
;;     cyberpunk-theme
;;     ir-black-theme
;;     darkburn-theme
;;     gotham-theme
;;     solarized-theme
;;     ;; phoenix-dark-pink-theme
;;     phoenix-dark-mono-theme
;;     clues-theme
;;     tao-theme
;;     flatui-theme
;;     moe-theme
;;     minimal-theme
;;     sexy-monochrome-theme
;;     plan9-theme
;;     spacemacs-theme
;;     material-theme
;;     color-theme-sanityinc-tomorrow
;;     ;; base16-theme
;;     goose-theme
;;     sourcerer-theme
;;     spacegray-theme
;;     monochrome-theme
;;     reykjavik-theme
;;     arjen-grey-theme
;;     rebecca-theme
;;     dracula-theme
;;     apropospriate-theme
;;     ))


(defvar j0ni-theme)
(defvar j0ni-light-theme)
(defvar j0ni-dark-theme)
;; (defvar j0ni-theme-tint)

;; (setq j0ni-dark-theme 'spacemacs-dark)
;; (setq j0ni-light-theme 'spacemacs-light)
;; (setq j0ni-dark-theme 'gruvbox)
(setq j0ni-light-theme '(leuven light SlateGray2 (100 . 70)))
;; (setq j0ni-light-theme 'darktooth)
;; (setq j0ni-dark-theme '(zenburn respectful grey40))
;; (setq j0ni-dark-theme '(dracula respectful grey30 (90 . 50)))
(setq j0ni-dark-theme '(zerodark respectful grey30 (95 . 50)))
;; (setq j0ni-dark-theme '(leuven light SlateGray2 (100 . 70)))

;; 'dark 'mid 'light
;; (setq j0ni-theme-tint 'dark)

(defun concat-home (path)
  (concat (getenv "HOME") "/" path))

;; (add-to-list 'custom-theme-load-path (concat-home "Scratch/emacs/phoenix-dark-pink"))
;; (require 'phoenix-dark-pink-theme)

;; Go bits
(defvar j0ni-go-path)
;; Path elements
(defvar j0ni-path)
;; Where are the system Git contrubutions?
(defvar j0ni-git-contrib-dir)
;; Org mode locations
(defvar j0ni-org-dir)
(defvar j0ni-notebook)
(defvar j0ni-org-dropbox)
(defvar j0ni-agenda-files)


(setq j0ni-go-path (list
                    (concat-home "Scratch/go")))

(setq j0ni-path (list (concat-home ".cabal/bin")
                      (concat-home ".cargo/bin")
                      (concat-home "Scratch/clojure/leiningen")
                      (concat-home "Scratch/clojure/boot")
                      (concat-home "Scratch/go/bin")
                      ;; (concat-home ".miniconda3/bin")
                      "/usr/local/bin"))

(setq j0ni-org-dir (concat-home "Dropbox/OrgMode/"))
(setq j0ni-notebook (concat j0ni-org-dir "notebook.org"))
(setq j0ni-org-dropbox (concat-home "Dropbox/Apps/MobileOrg"))
(setq j0ni-agenda-files (list j0ni-notebook))

;; Switch some stuff off...
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Setup exec-path and PATH environment variable, in case shell has failed to do so
(let ((paths j0ni-path))
  (setenv "PATH" (apply 'concat
                        (append (mapcar (lambda (i) (concat i ":")) paths)
                                (list (getenv "PATH")))))
  (dolist (path paths)
    (when (file-directory-p path)
      (add-to-list 'exec-path path))))

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Get hostname
(setq hostname (replace-regexp-in-string
                "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)"
                ""
                (with-output-to-string (call-process "hostname" nil standard-output))))

;; Set paths to custom.el and loaddefs.el
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Detect online status, from ESK
(require 'cl-lib)
(defun esk-online? ()
  (interactive)
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (cl-some (lambda (iface) (unless (equal "lo" (car iface))
                                 (member 'up (cl-first (last (network-interface-info
                                                              (car iface)))))))
               (network-interface-list))
    t))

;; set up TLS before doing anything with package
(setq gnutls-trustfiles (split-string (shell-command-to-string "python -m certifi")))
(setq gnutls-verify-error t)
;; (setq gnutls-log-level 2)
;; because builtin tls is bollocks (until Emacs 25?)
;; (setq tls-checktrust 'always)
;; (setq tls-program
;;       (list
;;        (format "gnutls-cli --x509cafile %s -p %%p %%h"
;;                (cl-first gnutls-trustfiles))))

;; This breaks in Emacs 25.0.92.2
;; (when (and (< emacs-major-version 25)
;;            (fboundp 'gnutls-available-p))
;;   (fmakunbound 'gnutls-available-p))

;; ELPA etc
;; (require 'package)
;; (setq package-user-dir (concat dotfiles-dir "elpa"))
;; (setq package-archives '(("org" . "https://orgmode.org/elpa/")
;;                          ("melpa" . "https://melpa.org/packages/")
;;                          ;; ("melpa-backup" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
;;                          ("melpa-stable" . "https://stable.melpa.org/packages/")
;;                          ("gnu" . "https://elpa.gnu.org/packages/")))
;; seems to work now?
;; (setq package-check-signature nil)

;;; Sometimes CIDER breaks and I need to retreat to stable
;;
;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)

;; (when (< emacs-major-version 27)
;;   (package-initialize))

;; (eval-when-compile
;;   (require 'package)
;;   (when (esk-online?)
;;     (when (not (package-installed-p 'use-package))
;;       (package-refresh-contents)
;;       (package-install 'use-package)
;;       (require 'use-package))
;;     (when (not (package-installed-p 'diminish))
;;       (package-install 'diminish)
;;       (require 'diminish))
;;     (when (not (package-installed-p 'bind-key))
;;       (package-install 'bind-key)
;;       (require 'bind-key))))

;; (setq use-package-always-ensure t)
;; (setq use-package-verbose t)

(use-package auto-compile
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package speed-type)

;; (use-package feebleline
;;   ;; :config
;;   ;; (feebleline-mode 1)
;;   :init
;;   (setq feebleline-msg-functions
;;         '((feebleline-line-number         :post "" :fmt "%5s")
;;           (feebleline-column-number       :pre ":" :fmt "%-2s")
;;           (feebleline-file-directory      :face feebleline-dir-face :post "")
;;           (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
;;           (feebleline-file-modified-star  :face font-lock-warning-face :post "")
;;           (feebleline-git-branch          :face feebleline-git-face :pre " - ")
;;           (feebleline-project-name        :align right))))

(defun initialize-and-load-theme (theme &optional indent-guide-color sml-theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))
    (read-color "Indent guide color: ")))
  (let ((sml-theme (or sml-theme 'respectful)))
    (load-theme theme t)
    (eval-after-load "j0ni-gui"
      `(progn
         (setq sml/theme ,(symbol-name sml-theme))
         (sml/setup)
         (set-font-dwim)
         (when ,indent-guide-color
           (set-indent-guide-face ,indent-guide-color)))))
  ;; (eval-after-load 'highlight-symbol
  ;;   '(set-face-attribute 'highlight-symbol-face nil :box t))
  )

(use-package dracula-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'dracula "grey30")
  )

(use-package chocolate-theme
  :no-require t
  ;; :config (initialize-and-load-theme 'chocolate "grey30")
  )

(use-package zenburn-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'zenburn "grey30")
  )

(use-package cyberpunk-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'cyberpunk "grey30")
  )

(use-package reverse-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'reverse "grey30")
  )

(use-package leuven-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'leuven "SlateGray2" 'light)
  )

(use-package nord-theme
  :no-require t
  :init
  (setq nord-comment-brightness 15)
  ;; :config
  ;; (initialize-and-load-theme 'nord "grey30")
  )

(use-package nova-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'nova "grey40")
  )

(use-package spacemacs-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'spacemacs-dark "grey30")
  ;; (initialize-and-load-theme 'spacemacs-light "grey80")
  )

(use-package gruvbox-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'gruvbox-light-medium "AntiqueWhite3")
  )

(use-package tangotango-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'tangotango "grey30")
  )

(use-package zerodark-theme :no-require t)

(use-package doom-themes
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'doom-nord-light "grey100")
  ;; (initialize-and-load-theme 'doom-solarized-light "grey70")
  ;; (initialize-and-load-theme 'doom-one-light "grey80")
  )

;; (use-package phoenix-dark-pink-theme
;;   :no-require t
;;   :config
;;   (initialize-and-load-theme 'phoenix-dark-pink))
;; (load "~/Scratch/emacs/phoenix-dark-mono/phoenix-dark-mono-theme.el")
;; (initialize-and-load-theme 'phoenix-dark-pink)

(use-package plan9-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'plan9 "gold")
  )

(use-package sunburn-theme
  :no-require t
  ;; :config
  ;; (initialize-and-load-theme 'sunburn "grey30")
)

;; (use-package olivetti)

;; (use-package grayscale-theme
;;   :no-require t
;;   :init
;;   (setq sml/theme 'respectful)
;;   :config
;;   (load-theme 'grayscale)
;;   (eval-after-load "j0ni-gui"
;;     '(progn
;;        (sml/setup)
;;        ;; (set-mode-line-box)
;;        (set-font-dwim))))

;; (use-package focus)

;; (defun package-require (pkg)
;;   "Install a package only if it's not already installed."
;;   (when (not (package-installed-p pkg))
;;     (package-install pkg)))

;; (defun packages-require (pkg-list)
;;   "Install a list of packages using package-require."
;;   (dolist (pkg pkg-list)
;;     (package-require pkg)))

(setq j0ni-pkg-full
      '(;; j0ni-exwm
        ;; j0ni-evil
        ;; j0ni-snippets
        j0ni-complete
        j0ni-lsp
        j0ni-esk
        ;; j0ni-eshell
        j0ni-defuns
        j0ni-misc
        j0ni-gui
        j0ni-ibuffer
        j0ni-codestyle ;; what even is this
        j0ni-lisp
        ;; j0ni-elixir
        j0ni-clojure
        ;; j0ni-erlang
        ;; j0ni-julia
        ;; j0ni-flycheck
        ;; j0ni-ido
        j0ni-ivy
        ;; j0ni-helm
        j0ni-ml
        ;; j0ni-go
        ;; j0ni-js
        j0ni-git
        ;; j0ni-ruby
        j0ni-markup
        j0ni-markdown
        j0ni-haskell
        ;; j0ni-purescript
        j0ni-rust
        j0ni-org
        j0ni-python
        ;; j0ni-irc
        ;; j0ni-jabber
        ;; j0ni-powerline
        j0ni-mail
        ;; j0ni-twitter
        ;; j0ni-audio
        ;; j0ni-scala
        ))

(dolist (file j0ni-pkg-full)
  (require file))

(when (fboundp 'set-font-dwim)
  (set-font-dwim))

(setq-default fill-column 80)

;; Emacs disables a bunch of commands by default so as not to confuse new users.
;; This enables them all.
(setq disabled-command-function nil)

;; Open for business

(require 'server)
(unless (server-running-p)
  (server-start))

;; start desktop saving
;; (boot-desktop)

;; (setq redisplay-dont-pause t)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; don't do that here, see j0ni-ml above
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
