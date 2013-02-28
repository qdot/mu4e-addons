(setq qdot/mu4e-unread-maildirs
			(list
			 "/nonpolynomial/INBOX"
			 "/mozilla/INBOX"
			 "/nonpolynomial/Mozilla.bugzilla"
			 "/nonpolynomial/MailingLists.Hackers"
			 "/nonpolynomial/MailingLists.OhhShiny"))

(setq qdot/mu4e-unread-maildir-index nil)

(defun qdot/mu4e-choose-unread-dir (&optional idx)
  (interactive "nUnread Directory Index: ")
  (if (> idx (length qdot/mu4e-unread-maildir-index))
      (message "Index not available"))
  (mu4e~headers-jump-to-maildir (nth idx qdot/mu4e-unread-maildir-index)))

(define-key mu4e-main-mode-map "g" '(lambda () (interactive) (mu4e~main-view)))
(define-key mu4e-main-mode-map "u" 'qdot/mu4e-choose-unread-dir)

(defadvice mu4e~main-view (after qdot/mu4e-main-view-unread () activate)
  "Adds the ability to show unread email counts for certain maildirs"
  (setq qdot/mu4e-unread-maildir-index nil)
  (let ((buf (get-buffer-create mu4e~main-buffer-name))
	(inhibit-read-only t))
    (with-current-buffer buf
      (insert
       (propertize "\n\n  Unread ('u' to select)\n\n" 'face 'mu4e-title-face)
       (let ((mbcount -1))
	 (mapconcat
	  (lambda (mailcount)
	    (let ((maildir (car mailcount))
		  (count (cdr mailcount)))
	      (when (> count 0)
		(setq mbcount (+ 1 mbcount))
		(if qdot/mu4e-unread-maildir-index
		    (add-to-list 'qdot/mu4e-unread-maildir-index maildir t)
		  (setq qdot/mu4e-unread-maildir-index (list maildir)))
		(concat (format "\t* [%s] " mbcount) maildir (format ": %s\n" count)))))
	  (mu4e-uqueue~unread-maildirs qdot/mu4e-unread-maildirs) ""))))))

(provide 'mu4e-unread-main)

