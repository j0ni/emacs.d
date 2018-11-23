;;; pp-debug.el --- Pretty-printing debugger  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Felipe Ochoa

;; Author: Felipe Ochoa
;; Created: 5 Dec 2017
;; License: GPLv3

;;; Commentary:
;;; Pretty-print debugger frames.

;;; Code:

(require 'cl-lib)
(require 'debug)

(defvar pp-debug-frame-num nil "Counting from `(debugger--backtrace-base)'.")

(defun pp-debug-from-debugger ()
  "Open a new pp-debug buffer for the debugger frame at point."
  (interactive)
  (let ((num (debugger-frame-number t)))
    (message "Pretty-printing frame %s" num)
    (pp-debug num)))

(defun pp-debug--count-stack-size ()
  "Count the number of frames in the current backtrace stack."
  (let ((base (debugger--backtrace-base))
        (i 0) frame)
    (while (setq frame (backtrace-frame i base))
      (cl-incf i))
    i))

(defun pp-debug (frame-num)
  "Load FRAME-NUM into the pretty debugger frame."
  (setq pp-debug-frame-num frame-num)
  (set-buffer (pop-to-buffer "*BT: Frame*"))
  (let* ((base (debugger--backtrace-base))
         (frame (backtrace-frame frame-num base))
         (inhibit-read-only t))
    (cl-destructuring-bind (special fn &rest args) frame
      (erase-buffer)
      (pp-debug-mode)
      (if special (insert ";; special form\n"))
      (save-excursion
        (insert "(" (pp-to-string fn))
        (dolist (arg args)
          (insert "\n" (pp-to-string arg)))
        (insert ")"))
      (indent-pp-sexp))))

(defvar pp-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" #'pp-debug-up)
    (define-key map "d" #'pp-debug-down)
    (define-key map "s" #'debugger-step-through)
    (define-key map "c" #'debugger-continue)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "e" #'pp-debug-eval)
    (define-key map "x" #'pp-debug-value-at-pos)
    map))

(define-derived-mode pp-debug-mode special-mode "Pretty Debugger"
  "Debug emacs in a pretty way."
  (lisp-mode-variables nil nil 'elisp))

(defun pp-debug-up ()
  "Re-render pp-debug with the frame above the current one."
  (interactive)
  (unless pp-debug-frame-num (error "No current pp-debug frame"))
  (when (= pp-debug-frame-num (pp-debug--count-stack-size))
      (user-error "Already at top-most frame"))
  (pp-debug (1+ pp-debug-frame-num)))

(defun pp-debug-down ()
  "Re-render pp-debug with the frame below the current one."
  (interactive)
  (unless pp-debug-frame-num (error "No current pp-debug frame"))
  (when (zerop pp-debug-frame-num)
    (user-error "Already at bottom frame"))
  (pp-debug (1- pp-debug-frame-num)))

(defun pp-debug-eval (exp)
  "Eval EXP in the environment of the current frame."
  (interactive (list (read--expression "Eval in stack frame: ")))
  (debugger-eval-expression exp pp-debug-frame-num))

(defun pp-debug-value-at-pos ()
  "Evaluate the symbol at point."
  (interactive)
  (let ((start (car (bounds-of-thing-at-point 'symbol)))
        syntax
        path i)
    (when (or (not start) (nth 3 syntax) (nth 4 syntax))
      (user-error "No symbol found at point"))
    (save-excursion
      (goto-char (1- start))
      (setq syntax (syntax-ppss))
      (while (nth 1 syntax)
        (goto-char (1+ (nth 1 syntax)))
        (forward-sexp)
        (setq i 0)
        (while (< (point) start)
          (forward-sexp)
          (cl-incf i))
        (push i path)
        (setq start (nth 1 syntax))
        (goto-char start)
        (setq syntax (syntax-ppss))))
    (cl-assert path)
    (let* ((base (debugger--backtrace-base))
           (frame (backtrace-frame pp-debug-frame-num base))
           (sym (cdr frame)))
      (dolist (i path) (setq sym (nth i sym)))
      (cl-assert (symbolp sym) t)
      (debugger-eval-expression sym))))

(define-key debugger-mode-map "r" 'pp-debug-from-debugger)

(provide 'pp-debug)

;;; pp-debug.el ends here
