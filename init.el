;;; init.el --- user init file -*- no-byte-compile: t -*-

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-protocol 'ssh)

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

;; install the org hack as early as possible
(straight-use-package 'org-plus-contrib)
(require 'org)

(straight-use-package 'use-package)

(defun update-straight-packages ()
  (interactive)
  (dolist (package '(gnu-elpa-mirror melpa org))
    (let ((p (symbol-name package)))
      (straight-pull-package p)
      (straight-normalize-package p)
      (straight-rebuild-package p)))
  (straight-pull-all)
  (straight-normalize-all)
  (straight-rebuild-all))

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
  (setq j0ni-font-face "Consolas")
  ;; (setq j0ni-font-face "Cuprum")
  ;; (setq j0ni-font-face "iosevka term")
  ;; (setq j0ni-font-face "Droid Sans Mono Dotted for Powerline")
  ;; (setq j0ni-font-face "Hack")
  ;; (setq j0ni-font-face "Linux Libertine Mono")
  ;; (setq j0ni-font-face "Fira Mono Medium")
  ;; (setq j0ni-font-face "Fira Mono")
  ;; (setq j0ni-font-face "Fira Code")
  ;; (setq j0ni-font-face "Envy Code R")
  (setq j0ni-font-face "Agave")
  ;; (setq j0ni-font-face "Source Code Variable")
  ;; (setq j0ni-font-face "Courier Prime Code")
  ;; (setq j0ni-font-face "Operator Mono Book")
  (setq j0ni-font-face "Lucida Console Patched")
  ;; (setq j0ni-font-face "Terminus (TTF)")
  ;; (setq j0ni-font-face "DejaVu Sans Mono")
  ;; (setq j0ni-font-face "Liberation Mono")
  ;; (setq j0ni-font-face "Bitstream Vera Sans Mono")
  ;; (setq j0ni-font-face "Noto Sans Mono")
  ;; (setq j0ni-font-face "Linux Biolinum G")
  ;; (setq j0ni-font-face "Lucida Grande Mono Nrw")
  ;; (setq j0ni-font-face "Operator Mono")
  ;; (setq j0ni-font-face "Lucida Grande Mono")
  (setq j0ni-font-face "Monoid")
  ;; (setq j0ni-font-face "PragmataPro Mono")
  ;; (setq j0ni-font-face "Inconsolata")
  ;; (setq j0ni-font-face "Mensch")
  ;; (setq j0ni-font-face "M+ 1mn")
  ;; (setq j0ni-font-face "Go Mono")
  ;; (setq j0ni-font-face "Iosevka ss10")
  ;; (setq j0ni-font-face "IBM Plex Mono")
  ;; (setq j0ni-font-face "agave")
  (setq j0ni-font-weight 'regular)
  ;; (setq j0ni-bold-font-weight 'regular)
  (setq j0ni-font-size 10)
  ;; (setq j0ni-line-spacing 8)
  (setq j0ni-line-spacing 2)
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

;; 'dark 'mid 'light
;; (setq j0ni-theme-tint 'dark)

(defun concat-home (path)
  (concat (getenv "HOME") "/" path))

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

(use-package auto-compile
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package speed-type)

;; (use-package focus)

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

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; start desktop saving
;; (boot-desktop)

;; (setq redisplay-dont-pause t)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; don't do that here, see j0ni-ml above
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
;; (exwm-enable)
