;;; mu4e-unreadqueue.el -- extension to mu4e for treating unread
;;;   maildirs as a queue
;;
;; Copyright (C) 2012 Christopher Allan Webber

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

; To use this, simply load this file and run:
;   M-x mu4e-uqueue
;
; ...this will open up a buffer that lists all the maildirs you have mail in 



(defun mu4e-uqueue~find-result-count (query)
  "Find the number of results that match a QUERY find with mu"
  (string-to-int
   (gnus-strip-whitespace
    (shell-command-to-string
     (format "%s find %s flag:unread 2>/dev/null | wc -l"
             mu4e-mu-binary query)))))


(defun mu4e-uqueue~unread-for-maildir (maildir)
  (mu4e-uqueue~find-result-count (format "\"maildir:%s\"" maildir)))

(defun mu4e-uqueue~unread-maildirs (maildirs)
  (mapcar
   (lambda (maildir)
     (cons maildir (mu4e-uqueue~unread-for-maildir maildir)))
   maildirs))

(defun mu4e-uqueue~unread-all-maildirs (path)
  (mapcar
   (lambda (maildir)
     (cons maildir (mu4e-uqueue~unread-for-maildir maildir)))
   (mu4e-get-maildirs)))


(defun mu4e-uqueue-update-queue ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (mailcount (mu4e-uqueue~unread-all-maildirs mu4e-maildir))
      (let ((maildir (car mailcount))
            (count (cdr mailcount)))
        (if (> count 0)
            (progn
              (insert (propertize maildir 'font-lock-face 'font-lock-variable-name-face))
              (insert (format ": %s" count))
              (add-text-properties
               (line-beginning-position) (line-end-position)
               `(mu4e-uqueue-maildir ,maildir))
              (insert "\n")))))
    (beginning-of-buffer)))


(defun mu4e-uqueue-visit-query ()
  (interactive)
  (let ((maildir (get-text-property (point) 'mu4e-uqueue-maildir)))
    (if maildir
        (mu4e~headers-search-execute (format "\"maildir:%s\" flag:unread" maildir) nil))))


(defun mu4e-uqueue ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*mu4e-uqueue*"))
  (mu4e-uqueue-mode)
  (mu4e-uqueue-update-queue))


(define-derived-mode mu4e-uqueue-mode special-mode "mu4e-uqueue"
  "A queue of mail directories or queries to go through in mu4e"
  )


(defvar mu4e-uqueue-mode-map (make-sparse-keymap))

(define-key mu4e-uqueue-mode-map "n" 'next-line)
(define-key mu4e-uqueue-mode-map "p" 'previous-line)
(define-key mu4e-uqueue-mode-map (kbd "RET") 'mu4e-uqueue-visit-query)
(define-key mu4e-uqueue-mode-map "g" 'mu4e-uqueue-update-queue)

