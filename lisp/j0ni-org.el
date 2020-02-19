;;; j0ni-org.el

(use-package org-plus-contrib
  :init
  ;; Set to the location of your Org files on your local system
  (setq org-directory j0ni-org-dir)

  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull (concat j0ni-org-dir "flagged.org"))

  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory j0ni-org-dropbox)

  ;; track TODO completion
  ;; (setq org-log-done 'time)

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
  (setq org-agenda-files (list (concat j0ni-org-dir "todo.org")
                               (concat j0ni-org-dir "journal.org")
                               (concat j0ni-org-dir "theplan.org")))
  (setq org-agenda-span 14)
  ;; (setq org-agenda-start-on-weekday nil)

  ;; prevent org-mode hijacking arrow keys
  (setq org-replace-disputed-keys t)

  ;; set our own todo keywords
  (setq org-todo-keywords
        '((sequence "TODO" "WAITING" "QUEUED" "PAUSED" "|" "DONE" "ABANDONED")))

  ;; switch quickly
  (setq org-use-fast-todo-selection t)
  (setq org-priority-default ?C)

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
  (require 'org-datetree)

  (setq org-default-notes-file (concat j0ni-org-dir "captured.org"))
  (setq org-capture-templates
        `(("j" "Journal" entry (file+datetree ,(concat j0ni-org-dir "journal.org"))
           "* %T\n  %i\n  %a")
          ("t" "Task" entry (file+headline ,(concat j0ni-org-dir "todo.org") "Tasks")
           "* TODO %?\n  %a\n%i")))

  :config
  (org-clock-persistence-insinuate)

  (dolist (tag '(orchard home motiva sanity tdg self))'
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
     ;; (clojure . t)
     (js . t)
     (lisp . t)
     (sql . t)
     (haskell . t)
     (scheme . t)))

  ;; hook for clojure programming
  ;; (add-hook 'clojure-mode-hook (lambda () (require 'ob-clojure)))

  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-capture-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook
               'before-save-hook 'org-update-all-dblocks nil 'local-only)))

  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))

  :commands (org-store-link org-capture))


;; org-journal
;; (use-package org-journal
;;   :after org-plus-contrib
;;   :commands (org-journal-new-entry)
;;   :custom
;;   (org-journal-dir (concat j0ni-org-dir "Journal/"))
;;   (org-journal-date-format "%A, %d %B %Y")
;;   (org-journal-enable-agenda-integration t)
;;   (org-journal-extend-today-until 4)

;;   ;; read this and fix this up
;;   ;; https://orgmode.org/manual/Matching-tags-and-properties.html
;;   (org-journal-carryover-items "TODO=\"TODO\"")

;;   :config
;;   ;; integrate with org-capture
;;   (defun org-journal-find-location ()
;;     ;; Open today's journal, but specify a non-nil prefix argument in order to
;;     ;; inhibit inserting the heading; org-capture will insert the heading.
;;     (org-journal-new-entry t)
;;     ;; Position point on the journal's top-level heading so that org-capture
;;     ;; will add the new entry as a child entry.
;;     (goto-char (point-min)))

;;   (setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
;;                                  "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))))


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

;; Can't get this to work,
;; (use-package org-sidebar
;;   :after org-plus-contrib
;;   :defer t
;;   :commands (org-sidebar-tree
;;              org-sidebar-tree-toggle
;;              org-sidebar
;;              org-sidebar-toggle))


(provide 'j0ni-org)
