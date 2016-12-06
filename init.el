;;; init.el --- new init, based on Bodil Stokke's

(defun insert-shrug ()
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

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

;; Add brew contrib stuff to the load-path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

;; Add .emacs.d/lisp to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(dolist (sdir (directory-files (concat dotfiles-dir "site-lisp") t "\\w+"))
  (when (file-directory-p sdir)
    (add-to-list 'load-path sdir)))

(progn
  ;; (defvar j0ni-font "Anonymous Pro-10")
  ;; (defvar j0ni-linum-font "Anonymous Pro-9")
  ;; (defvar j0ni-font "Droid Sans Mono Dotted-12")
  ;; (defvar j0ni-linum-font "Droid Sans Mono Dotted-10")
  ;; (defvar j0ni-font "Droid Sans Mono Dotted-14")
  ;; (defvar j0ni-linum-font "Droid Sans Mono Dotted-12")
  ;; (defvar j0ni-font "PragmataPro Mono-10")
  ;; (defvar j0ni-linum-font "PragmataPro Mono-9")
  ;; (defvar j0ni-font "PragmataPro Mono-14")
  ;; (defvar j0ni-linum-font "PragmataPro Mono-12")
  ;; (defvar j0ni-font "Consolas-12")
  ;; (defvar j0ni-font "Lucida Grande Mono-9")
  ;; (defvar j0ni-linum-font "Lucida Grande Mono-8")
  ;; (defvar j0ni-font "Linux Libertine Mono-10")
  ;; (defvar j0ni-linum-font "Linux Libertine Mono-8")
  ;; (defvar j0ni-font "Fira Mono Medium-9")
  ;; (defvar j0ni-linum-font "Fira Mono-8")
  (defvar j0ni-font "Go Mono-11")
  (defvar j0ni-linum-font "Go Mono-8")
  ;; (defvar j0ni-font "Lucida Grande Mono Nrw-11")
  ;; (defvar j0ni-linum-font "Lucida Grande Mono Nrw-8")
  ;; (defvar j0ni-font "Lucida Grande Mono Nrw-28")
  ;; (defvar j0ni-font "Lucida Grande Mono Nrw-12")
  (when (fboundp 'apply-font-settings)
    (apply-font-settings)))

(setq-default line-spacing 2)

;; Themes we want to install

(require 'color-theme-tomorrow)
;; (require 'lawrence-theme)
;; (color-theme-tomorrow-night)

(defvar j0ni-installed-themes
  '(soothe-theme
    ;; late-night-theme
    noctilux-theme
    cyberpunk-theme
    subatomic-theme
    ir-black-theme
    twilight-theme
    twilight-bright-theme
    twilight-anti-bright-theme
    zenburn-theme
    bubbleberry-theme
    darkburn-theme
    gotham-theme
    solarized-theme
    phoenix-dark-pink-theme
    phoenix-dark-mono-theme
    ;; color-theme-github
    clues-theme
    sublime-themes
    flatland-theme
    flatland-black-theme
    tao-theme
    darcula-theme
    firecode-theme
    ujelly-theme
    tango-2-theme
    tangotango-theme
    tango-plus-theme
    ;; spacegray-theme
    ;; purple-haze-theme
    flatui-theme
    moe-theme
    minimal-theme
    plan9-theme
    stekene-theme
    spacemacs-theme
    material-theme
    color-theme-sanityinc-tomorrow
    base16-theme
    goose-theme
    anti-zenburn-theme
    sourcerer-theme
    spacegray-theme
    oceanic-theme
    hipster-theme))

;; Theme I like at the moment
(defvar j0ni-theme 'phoenix-dark-pink)
;; (defvar j0ni-theme 'phoenix-dark-mono)
;; (defvar j0ni-theme 'late-night)
;; (defvar j0ni-theme 'tango-dark)
;; (defvar j0ni-theme 'tango-2)
;; (Defvar j0ni-theme 'material)
;; (defvar j0ni-theme 'material-light)
;; (defvar j0ni-theme 'stekene-light)
;; (defvar j0ni-theme 'stekene-dark)
;; (defvar j0ni-theme 'solarized-light)
;; (defvar j0ni-theme 'solarized-dark)
;; (defvar j0ni-theme 'tomorrow-night-eighties)
;; (defvar j0ni-theme 'monochrome)
;; (defvar j0ni-theme 'plan9)
;; (defvar j0ni-theme 'anti-zenburn)
;; (defvar j0ni-theme 'spacemacs-dark)
;; (defvar j0ni-theme 'spacemacs-light)
;; (defvar j0ni-theme 'sanityinc-tomorrow-night)
;; (defvar j0ni-theme 'sanityinc-tomorrow-floraverse)
;; (defvar j0ni-theme 'sanityinc-tomorrow-floraverse-boost)
;; (defvar j0ni-theme 'bubbleberry)
;; (defvar j0ni-theme 'zenburn)
;; (defvar j0ni-theme 'lawrence)
;; (defvar j0ni-theme 'darkburn)
;; (defvar j0ni-theme 'base16-apathy-dark)
;; (defvar j0ni-theme 'base16-ashes-dark)
;; (defvar j0ni-theme 'base16-solarized-dark)
;; (defvar j0ni-theme 'base16-green-screen)
;; (defvar j0ni-theme 'base16-grayscale-dark)
;; (defvar j0ni-theme 'sourcerer)
;; (defvar j0ni-theme 'base16-isotope)
;; (defvar j0ni-theme 'base16-ir-black)
;; (defvar j0ni-theme 'hipster)
;; (defvar j0ni-theme 'base16-ocean)
;; (defvar j0ni-theme 'mccarthy)
;; (defvar j0ni-theme 'github)
;; (defvar j0ni-theme 'fogus)
;; (defvar j0ni-theme 'gotham)
;; (defvar j0ni-theme 'moe-dark)
;; (defvar j0ni-theme 'moe-light)
;; (defvar j0ni-theme 'cyberpunk)
;; (defvar j0ni-theme 'noctilux)
;; (defvar j0ni-theme 'ujelly)
;; (defvar j0ni-theme 'clues)
;; (defvar j0ni-theme 'flatui)
;; (defvar j0ni-theme 'subatomic)

;; 'dark 'mid 'light
(defvar j0ni-theme-tint 'dark)

;; experimenting with a new thing
;; (global-font-lock-mode -1)

;; Notes: this is cool, except I'd like a couple of things to be
;; fontlocked. For example, highlight-symbol-mode, and eval-sexp-fu,
;; and maybe some error stuff in the cider repl. I suspect I'm going
;; to need to create a specifically targeted set of face
;; configurations for the standard set of programming faces.

(defun concat-home (path)
  (concat (getenv "HOME") "/" path))

;; Go bits
(defvar j0ni-go-path (list ;; (concat-home "Scratch/CircleCI/go-projects")
                           (concat-home "Scratch/go")
                           ;; (concat-home "Scratch/goeg")
                           ))

;; Path elements
(defvar j0ni-path (list (concat-home ".rbenv/shims")
                        (concat-home ".rbenv/bin")
                        (concat-home ".cabal/bin")
                        (concat-home ".cargo/bin")
                        (concat-home "Scratch/clojure/leiningen")
                        (concat-home "Scratch/go/bin")
                        "/usr/local/bin"))
;; Where are the system Git contrubutions?
(defvar j0ni-git-contrib-dir "/usr/local/share/emacs/site-lisp/git")

;; Org mode locations
(defvar j0ni-org-dir (concat-home "Dropbox/OrgMode/"))
(defvar j0ni-notebook (concat j0ni-org-dir "notebook.org"))
(defvar j0ni-org-dropbox (concat-home "Dropbox/Apps/MobileOrg"))
(defvar j0ni-agenda-files (list j0ni-notebook))

;; Switch some stuff off...
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; (setq mac-command-modifier 'meta)

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
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)"
                                         ""
                                         (with-output-to-string (call-process "hostname" nil standard-output))))

;; Set paths to custom.el and loaddefs.el
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file)

;; Detect online status, from ESK
(require 'cl)
(defun esk-online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                              (member 'up (first (last (network-interface-info
                                                        (car iface)))))))
            (network-interface-list))
    t))

;; set up TLS before doing anything with package
(setq gnutls-trustfiles (split-string (shell-command-to-string "python -m certifi")))
(setq gnutls-verify-error t)
;; (setq gnutls-log-level 2)
;; because builtin tls is bollocks (until Emacs 25?)
(setq tls-checktrust 'always)
(setq tls-program
      (list
       (format "gnutls-cli --x509cafile %s -p %%p %%h"
               (first gnutls-trustfiles))))

;; This breaks in Emacs 25.0.92.2
(when (and (< emacs-major-version 25)
           (fboundp 'gnutls-available-p))
  (fmakunbound 'gnutls-available-p))

;; ELPA etc
(require 'package)
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
;; seems to work now?
;; (setq package-check-signature nil)

;;; Sometimes CIDER breaks and I need to retreat to stable
;;
;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)

(package-initialize)

(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents)))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(setq use-package-always-ensure t)

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(defun packages-require (pkg-list)
  "Install a list of packages using package-require."
  (dolist (pkg pkg-list)
    (package-require pkg)))

(use-package diminish)

(setq j0ni-pkg-full
      '(
        ;; j0ni-evil
        j0ni-esk
        j0ni-eshell
        j0ni-defuns
        j0ni-misc
        j0ni-ibuffer
        j0ni-snippets
        j0ni-codestyle ;; what even is this
        j0ni-lisp
        j0ni-elixir
        j0ni-julia
        ;; j0ni-flycheck
        ;; j0ni-ido
        j0ni-ivy
        ;; j0ni-helm
        j0ni-go
        j0ni-js
        j0ni-git
        j0ni-ruby
        j0ni-markup
        j0ni-markdown
        j0ni-haskell
        j0ni-purescript
        j0ni-rust
        j0ni-org
        j0ni-python
        j0ni-complete
        j0ni-irc
        ;; j0ni-jabber
        ;; j0ni-powerline
        j0ni-mail
        j0ni-twitter
        j0ni-gui
        j0ni-audio
        j0ni-scala
        ))

(dolist (file j0ni-pkg-full)
  (require file))

;; helpful work stuff

(defun my-cider-connect (host port)
  (interactive "sDefault Host: \nnPort: ")
  (cider-connect host port))

(defun circle-connect ()
  (interactive)
  (cider-connect "localhost" 6005))

(setq-default fill-column 80)

;; Emacs disables a bunch of commands by default so as not to confuse new users.
;; This enables them all.
(setq disabled-command-function nil)

;; Open for business

(require 'server)
(unless (server-running-p)
  (server-start))
