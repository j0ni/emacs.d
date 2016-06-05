;;; j0ni-org.el

(package-require 'org)

;; Set to the location of your Org files on your local system
;; (setq org-directory "~/.org")
(setq org-directory j0ni-org-dir)

;; org-journal
(setq org-journal-dir (concat j0ni-org-dir "/journal/"))
(setq org-journal-find-file 'find-file)

;; ensure the journal files get picked up
(setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat j0ni-org-dir "/flagged.org"))

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory j0ni-org-dropbox)

;; track TODO completion
(setq org-log-done 'time)

;; get a completion note - let's see how easy this is to deal with
(setq org-log-done 'note)

;; indentation for org-mode
(setq org-startup-indented t)

(with-eval-after-load 'org
  '(progn
     ;; Set agenda file(s)
     (add-to-list 'org-agenda-files org-journal-dir)

     (dolist (tag '(circle skalera orchard home))'
       (add-to-list 'org-tag-persistent-alist tag))))

;; prevent org-mode hijacking arrow keys
(setq org-replace-disputed-keys t)

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (python . t)
   (ruby . t)
   (sh . t)
   (clojure . t)
   (js . t)
   (lisp . t)
   (sql . t)
   (haskell . t)
   (scheme . t)))

;; extra indentation
(setq org-adapt-indentation t)

;; Use cider as the clojure execution backend
(setq org-babel-clojure-backend 'cider)

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

;; hook for clojure programming
(add-hook 'clojure-mode-hook (lambda () (require 'ob-clojure)))

;; Key-bindings for some global commands
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; org-fstree
(package-require 'org-fstree)
(require 'org-fstree)

;; deft
(package-require 'deft)
(setq deft-extension "org"
      deft-directory j0ni-org-dir
      deft-text-mode 'org-mode
      deft-use-filename-as-title t
      deft-auto-save-interval 0)

;; journal
(package-require 'org-journal)

;; bullets
;; (package-require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(provide 'j0ni-org)
