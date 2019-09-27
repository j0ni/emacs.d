;;; j0ni-org.el

(use-package org
  :init
  ;; Set to the location of your Org files on your local system
  (setq org-directory j0ni-org-dir)

  ;; ensure the journal files get picked up
  (setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")

  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull (concat j0ni-org-dir "flagged.org"))

  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory j0ni-org-dropbox)

  ;; track TODO completion
  (setq org-log-done 'time)

  ;; get a completion note - let's see how easy this is to deal with
  (setq org-log-done 'note)

  ;; time tracking
  (setq org-clock-persist t)
  (setq org-duration-format 'h:mm)
  (setq org-clock-clocked-in-display 'mode-line)
  (setq org-clock-mode-line-total 'current)

  ;; indentation for org-mode
  (setq org-startup-indented t)

  ;; Set agenda file(s)
  (setq org-agenda-files (concat j0ni-org-dir "Agenda/"))

  ;; prevent org-mode hijacking arrow keys
  (setq org-replace-disputed-keys t)

  ;; switch quickly
  (setq org-use-fast-todo-selection t)

  ;; extra indentation
  (setq org-adapt-indentation t)

  ;; Use cider as the clojure execution backend
  (setq org-babel-clojure-backend 'cider)

  ;; Let's have pretty source code blocks
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil)

  ;; org-capture
  (setq org-default-notes-file (concat j0ni-org-dir "captured.org"))
  (setq org-capture-templates
        '(("j" "Journal entry" entry (file+datetree "")
           "* %<%H:%M> %?\n")
          ("J" "Hinted Journal entry" entry (file+datetree "")
           "* %<%H:%M> %?\n** How do I feel?\n** What have I been doing?\n** What shall I do next?")
          ("t" "Task" entry (file+headline "" "Tasks")
           "* TODO %?\n%u")))

  :config
  (org-clock-persistence-insinuate)

  (dolist (tag '(orchard home motiva sanity tdg))'
    (add-to-list 'org-tag-persistent-alist tag))

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (python . t)
     ;; (ipython . t)
     (ruby . t)
     ;; (sh . t)
     (clojure . t)
     (js . t)
     (lisp . t)
     (sql . t)
     (haskell . t)
     (scheme . t)))

  ;; hook for clojure programming
  (add-hook 'clojure-mode-hook (lambda () (require 'ob-clojure)))

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook
               'before-save-hook 'org-update-all-dblocks nil 'local-only)))


  ;; Key-bindings for some global commands
  ;; (global-set-key "\C-cl" 'org-store-link)
  ;; (global-set-key "\C-cc" 'org-capture)
  ;; (global-set-key "\C-ca" 'org-agenda)
  ;; (global-set-key "\C-cb" 'org-iswitchb)

  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture))

  :commands (org-store-link org-capture)
  )


;; org-journal
;; (package-require 'org-journal)

;; (setq org-journal-dir (concat j0ni-org-dir "Journal/"))
;; (setq org-journal-find-file 'find-file)

;; jupyter
(use-package jupyter)

;; deft
(use-package deft
  :init
  (setq deft-extension "org"
        deft-directory j0ni-org-dir
        deft-recursive t
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-auto-save-interval 0))


(provide 'j0ni-org)
