;;; npel.el --- Minor-mode provides some conventions when you write el files  -*- lexical-binding:t -*-

;; Author: Yuto Sato <nyannmaru.project0214@gmail.com>
;; URL: https://github.com/nyannmaru/npel
;; Version: 0.1.00
;; Keywords: utility, emacs lisp, convention, minor-mode
;; Package-Requires: ((emacs "28.2"))

;;; Commentary: No Comment(´・ω・｀)

(require 'simple)
(eval-when-compile (require 'subr-x))
(require 'paren)
;;(require 'lisp);;doesn't work. is there any way of explicit import of `insert-parentheses'?(´・ω・｀)


;;;;;       Some apapters of `show-paren--default' begins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun npel--proc-showparen (&optional notif)
  (let ((points (show-paren--default)))
    (cond ((null points);simply you're not on pars => nil
	   (when notif (message "You're not on parens") nil))
	  ((nth 4 points);par balance goes out of style => nil
	   (when notif (message "Your buffer's pars are somewhere untidy")) nil)
	  (t;you're on a decnet par => (spt ept)
	   (let ((pt1 (car points)) (pt2 (caddr points)))
	     (list (min pt1 pt2) (1+ (max pt1 pt2))))))))
(defun npel-goto-paren-start nil
  "Move point to where `show-paren--default' signifies as the start point.
If you're not on a parenthesis or pars are out of balance message it."
  (interactive)
  (let ((pts (npel--proc-showparen t)))
    (if pts (goto-char (car pts)))))
(defun npel-goto-paren-end nil
  "Move point to where `show-paren--default' signifies as the end point.
If you're not on a parenthesis or pars are out of balance message it."
  (interactive)
  (let ((pts (npel--proc-showparen t)))
    (if pts (goto-char (cadr pts)))))
(defun npel-goto-paren-counterpart nil
  "Move point to where `show-paren--default' signifies as the counterpart of the parens on the current point.
If you're not on a parenthesis or pars are out of balance message it."
  (interactive)
  (when (npel--proc-showparen t)
    (let ((cpt (point)))
      ;;(push-mark nil t);;needs predicate fn, this introduces a plethora of marks(´・ω・｀)
      (npel-goto-paren-start)
      (when (eq (point) cpt)
	(npel-goto-paren-end)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;          Some apapters of `show-paren--default' ends

;;;;;         Some wrappers of `insert-parentheses' begins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom npel-insert-parenth-starters "\"("
  "This variable is used when you calling `npel-insert-parentheses' on an expression.
Back to the nearest point one of starter shows up")
(defun npel-insert-parentheses nil
  (interactive)
  (let ((parens-require-spaces nil)
	(lstr (string-trim (buffer-substring-no-properties (point-at-bol) (point))))
	(bch (char-before))
	(ach (char-after))
	(blankers '(?\s ?\t ?\n)))
    (cond ((and (memq bch blankers) (memq ach blankers));you're on "   |  "
	   (insert-parentheses))
	  ((and (eq bch ?\() (eq ach ?\)));you're on (|)
	   (insert-parentheses))
	  ((npel--proc-showparen);you're on either side of paren
	   (let ((pt-before (point)))
	     (npel-goto-paren-start) (insert-parentheses 1)
	     (insert " ") (backward-char)
	     (indent-region (point) pt-before)));better than nothing huh?(´・ω・｀)
	  ((string-match-p (concat "[" npel-insert-parenth-starters "]") lstr)
	   (let ((seps (string-to-list npel-insert-parenth-starters)))
	     (while (not (memq (char-after) seps)) (backward-char));can't treat escaped char at all!(´・ω・｀)
	     (insert-parentheses 1) (insert " ") (backward-char)))
	  (t (message "Unexpected Pattern \n%s" lstr)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;          Some wrappers of `insert-parentheses' ends


(defvar npel-global-map (let ((kmap (make-sparse-keymap)))
			  (define-key kmap (kbd "M-n") 'npel-goto-paren-counterpart)
			  (define-key kmap (kbd "M-p") 'npel-insert-parentheses)
			  kmap)
  "keymap on the buffer in where `npel-mode' in on")

(define-minor-mode npel-mode
  "npel is the minor-mode for providing some utilties when you writing emacs-lisp aka el files.
Associated keymap (`npel-global-map'):"
  :lighter " nEl"
  :keymap npel-global-map)
(provide 'npel)
