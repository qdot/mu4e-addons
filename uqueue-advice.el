;;; uqueue-advice.el -- Hackily make mu4e-uqueue.el better
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


(defvar mu4e-uqueue-return-to-uqueue nil)
(defvar mu4e-uqueue-visiting-headers-via-uqueue nil)

(defcustom mu4e-uqueue-refresh-on-return t
  "Whether or not to refresh uqueue every time we return to it by quitting
the header buffer")

(defadvice mu4e~headers-quit-buffer
  (around mu4e-uqueue-possibly-return-to-uqueue)
  "Return to uqueue if appropriate when quitting"
  (let ((return-to-uqueue
         (buffer-local-value 'mu4e-uqueue-return-to-uqueue (current-buffer))))
    ad-do-it
    (if (and return-to-uqueue (get-buffer "*mu4e-uqueue*"))
        (progn
          (switch-to-buffer (get-buffer "*mu4e-uqueue*"))
          (if mu4e-uqueue-refresh-on-return
              (mu4e-uqueue-update-queue))))))

(defadvice mu4e-uqueue-visit-query
  (around mu4e-uqueue-set-return-to-uqueue)
  ad-do-it
  (make-local-variable 'mu4e-uqueue-return-to-uqueue)
  (setq mu4e-uqueue-return-to-uqueue t))

(ad-activate 'mu4e~headers-quit-buffer)
(ad-activate 'mu4e-uqueue-visit-query)
