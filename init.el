;;; init.el --- new init, based on Bodil Stokke's

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
  ;; (defvar j0ni-font "Anonymous Pro-14")
  ;; (defvar j0ni-linum-font "Anonymous Pro-12")
  ;; (defvar j0ni-font "Droid Sans Mono Dotted-11")
  ;; (defvar j0ni-linum-font "Droid Sans Mono Dotted-10")
  ;; (defvar j0ni-font "Droid Sans Mono Dotted-14")
  ;; (defvar j0ni-linum-font "Droid Sans Mono Dotted-12")
  ;; (defvar j0ni-font "PragmataPro-14")
  ;; (defvar j0ni-linum-font "PragmataPro-12")
  ;; (defvar j0ni-font "PragmataPro-12")
  ;; (defvar j0ni-linum-font "PragmataPro-10")
  ;; (defvar j0ni-font "Consolas-14")
  ;; (defvar j0ni-linum-font "Consolas-12")
  ;; (defvar j0ni-font "Inconsolata-14")
  ;; (defvar j0ni-linum-font "Inconsolata-12")
  ;; (defvar j0ni-font "Menlo-12")
  ;; (defvar j0ni-linum-font "Menlo-10")
  ;; (defvar j0ni-font "Menlo-14")
  ;; (defvar j0ni-linum-font "Menlo-12")
  ;; (defvar j0ni-font "DejaVu Sans Mono-14")
  ;; (defvar j0ni-linum-font "DejaVu Sans Mono-12")
  ;; (defvar j0ni-font "PT Mono-12")
  ;; (defvar j0ni-linum-font "PT Mono-9")
  ;; (defvar j0ni-font "Lucida Grande Mono-12")
  ;; (defvar j0ni-linum-font "Lucida Grande Mono-9")
  ;; (defvar j0ni-font "Fira Code-13")
  ;; (defvar j0ni-linum-font "Fira Code-9")
  ;; (defvar j0ni-font "Lucida Grande Mono Nrw-14")
  ;; (defvar j0ni-linum-font "Lucida Grande Mono Nrw-10")
  (defvar j0ni-font "Lucida Grande Mono-12")
  (defvar j0ni-linum-font "Lucida Grande Mono-10")
  ;; (defvar j0ni-font "Lucida Sans Typewriter-11")
  ;; (defvar j0ni-linum-font "Lucida Sans Typewriter-9")
  ;; (defvar j0ni-font "Lucida Console-11")
  ;; (defvar j0ni-linum-font "Lucida Console-9")
  ;; use this to play with new fonts - not defined yet so don't leave it
  ;; uncommented
  (when (fboundp 'apply-font-settings)
    (apply-font-settings)))

(setq-default line-spacing 0)

;; Themes we want to install

(require 'color-theme-tomorrow)
;; (require 'lawrence-theme)

(defvar j0ni-installed-themes
  '(soothe-theme
    late-night-theme
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
    color-theme-github
    clues-theme
    sublime-themes
    flatland-theme
    flatland-black-theme
    tao-theme
    darcula-theme
    firecode-theme
    ujelly-theme
    ;; spacegray-theme
    ;; purple-haze-theme
    flatui-theme
    minimal-theme
    plan9-theme
    stekene-theme
    spacemacs-theme
    material-theme
    color-theme-sanityinc-tomorrow
    base16-theme))

;; Theme I like at the moment
;; (defvar j0ni-theme 'phoenix-dark-pink)
;; (defvar j0ni-theme 'phoenix-dark-mono)
;; (defvar j0ni-theme 'late-night)
;; (defvar j0ni-theme 'tango-dark)
;; (defvar j0ni-theme 'material)
;; (defvar j0ni-theme 'stekene-dark)
;; (defvar j0ni-theme 'solarized-dark)
;; (defvar j0ni-theme 'tomorrow-night-eighties)
;; (defvar j0ni-theme 'monochrome)
;; (defvar j0ni-theme 'plan9)
;; (defvar j0ni-theme 'spacemacs-dark)
;; (defvar j0ni-theme 'bubbleberry)
;; (defvar j0ni-theme 'zenburn)
;; (defvar j0ni-theme 'lawrence)
;; (defvar j0ni-theme 'darkburn)
(defvar j0ni-theme 'base16-irblack-dark)
;; (defvar j0ni-theme 'base16-tomorrow-dark)
;; (defvar j0ni-theme 'base16-ashes-dark)
;; (defvar j0ni-theme 'base16-solarized-dark)
;; (defvar j0ni-theme 'base16-greenscreen-dark)
;; (defvar j0ni-theme 'stekene-dark)
;; (defvar j0ni-theme 'mccarthy)
;; (defvar j0ni-theme 'github)
;; (defvar j0ni-theme 'fogus)
;; (defvar j0ni-theme 'gotham)
;; (defvar j0ni-theme 'cyberpunk)
;; (defvar j0ni-theme 'noctilux)
;; (defvar j0ni-theme 'ujelly)
;; (defvar j0ni-theme 'clues)
;; (defvar j0ni-theme 'flatui)
;; (defvar j0ni-theme 'minimal-light)
;; (defvar j0ni-theme 'subatomic)

(defun concat-home (path)
  (concat (getenv "HOME") "/" path))

;; Go bits
(defvar j0ni-go-path (list (concat-home "Scratch/CircleCI/go-projects")
                           (concat-home "Scratch/go")
                           (concat-home "Scratch/goeg")))

;; Path elements
(defvar j0ni-path (list (concat-home ".rbenv/shims")
                        (concat-home ".rbenv/bin")
                        (concat-home ".cabal/bin")
                        (concat-home "Scratch/clojure/leiningen")
                        (concat-home "Scratch/go/bin")
                        "/usr/local/bin"
                        "/Applications/GHC.app/Contents/bin"
                        "/Applications/Racket v6.3/bin"))
;; Where are the system Git contrubutions?
(defvar j0ni-git-contrib-dir "/usr/local/share/git-core/contrib/emacs")

;; Org mode locations
(defvar j0ni-org-dir (concat-home "Dropbox/OrgMode"))
(defvar j0ni-notebook (concat j0ni-org-dir "/notebook.org"))
(defvar j0ni-org-dropbox (concat-home "Dropbox/Apps/MobileOrg"))
(defvar j0ni-agenda-files (list j0ni-notebook))
(defvar j0ni-org-journal-dir (concat j0ni-org-dir "/Journal"))

;; Switch some stuff off...
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; ...and some other stuff on
;; (dolist (mode '(blink-cursor-mode line-number-mode column-number-mode))
;;   (when (fboundp mode) (funcall mode +1)))

(setq mac-command-modifier 'meta)

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
  (dolist (path paths) (when (file-directory-p path)
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


;; ELPA
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ;; questionable value - mostly just for CIDER 0.7.0
                  ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ;; This is sooooo slow, do we really need it?
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

;;; Sometimes CIDER breaks and I need to retreat to stable
;;
;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)

(package-initialize)
(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents)))

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(defun packages-require (pkg-list)
  "Install a list of packages using package-require."
  (dolist (pkg pkg-list)
    (package-require pkg)))

(setq j0ni-pkg-full
      '(j0ni-evil
        j0ni-esk
        j0ni-eshell
        j0ni-defuns
        j0ni-misc
        j0ni-snippets
        j0ni-codestyle
        j0ni-lisp
        ;; j0ni-flycheck
        j0ni-ido
        ;; j0ni-helm
        j0ni-go
        j0ni-js
        j0ni-git
        j0ni-ruby
        j0ni-markup
        j0ni-markdown
        j0ni-haskell
        j0ni-org
        j0ni-python
        j0ni-complete
        j0ni-gui
        j0ni-irc
        ;; j0ni-jabber
        ;; j0ni-powerline
        j0ni-mail
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

;; Emacs disables a bunch of commands by default so as not to confuse
;; new users. This enables them all. I may regret this, but I've been
;; using emacs for a long time now.
(setq disabled-command-function nil)

;; Open for business

(require 'server)
(unless (server-running-p)
  (server-start))
