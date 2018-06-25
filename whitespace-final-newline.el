;;; whitespace-final-newline.el -- Highlight when a buffer does not end with a NEWLINE

;; Copyright (c) 2018 Michael D. Adams

;; Author: Michael D. Adams <https://michaeldadams.org>
;; Version: 1.0
;; Keywords: whitespace, newline, wp
;; URL: https://bitbucket.org/adamsmd/whitespace-final-newline/

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package contains a minor mode that highlights when a buffer does
;; not end with a NEWLINE by adding an overlay with a highlighted message
;; to the end of the buffer.

;;; Code:

(require 'whitespace)

;;; ---------------------------------------------------------------------
;;; User Options
;;; ---------------------------------------------------------------------

(defgroup whitespace-final-newline nil
  "Minor mode highlighting when a buffer does not end with a NEWLINE."
  :group 'whitespace)

(defcustom whitespace-final-newline-no-message-when-point-at-eob t
  "Do not show the message when the point is at the end of the buffer.

WARNING: Turning this off may cause the message to be shown in the minibuffer."
  :type 'boolean
  :group 'whitespace-final-newline)

(defcustom whitespace-final-newline-message " <-- No final newline"
  "Text to show when a buffer does not end with a NEWLINE."
  :type 'string
  :group 'whitespace-final-newline)
;; TODO: When string changes, update overlays

(defface whitespace-final-newline
  '((t :background "#aa00aa"))
  "Face in which to show the highlighted message for when a
buffer does not end with a NEWLINE."
  :group 'whitespace-final-newline)

;;; ---------------------------------------------------------------------
;;; Functions
;;; ---------------------------------------------------------------------

(defun whitespace-final-newline-p (buffer-position)
  "Return true when the overlay at BUFFER-POSITION should not be shown."
  (or
   ;; Point is at the end of the buffer
   (and whitespace-final-newline-no-message-when-point-at-eob
        (= (point) buffer-position))
   ;; There is a newline at the end of the buffer
   (let ((s (buffer-substring-no-properties (buffer-size) (1+ (buffer-size)))))
     (or (string-equal "\n" s)
         (string-equal "\r" s)))))

(defun whitespace-final-newline-make-overlay ()
  "Add the overlay containing the highlighted message."
  (let* ((s (propertize
             (concat
              (propertize whitespace-final-newline-message
               'display `((when (whitespace-final-newline-p buffer-position) . "")))
              (propertize " "
               'display '((when (whitespace-final-newline-p buffer-position) . "")
                          (space :align-to right))))
             'cursor t
             'face 'whitespace-final-newline))
         (b (current-buffer))
         (i (1+ (buffer-size b)))
         (o (make-overlay i i b t t)))
      (overlay-put o 'after-string s)
      (overlay-put o 'whitespace-final-newline t)))

(defun whitespace-final-newline-delete-overlay ()
  "Remove all overlays added by this minor mode."
  (mapc #'(lambda (o) (if (overlay-get o 'whitespace-final-newline) (delete-overlay o)))
        (overlays-in (1+ (buffer-size)) (1+ (buffer-size)))))

;;; ---------------------------------------------------------------------
;;; Mode Definition
;;; ---------------------------------------------------------------------

;;;###autoload
(define-minor-mode whitespace-final-newline-mode
  "Minor mode that highlights when a buffer has no final NEWLINE
by adding an overlay with a highlighted message to the end of the
buffer."
  :group 'whitespace-final-newline
  ;; Disable/cleanup before doing anything
  (whitespace-final-newline-delete-overlay)
  ;; Enable if necessary
  (when whitespace-final-newline-mode
    (whitespace-final-newline-make-overlay)))

;;;###autoload
(define-globalized-minor-mode global-whitespace-final-newline-mode whitespace-final-newline-mode
  whitespace-final-newline-mode
  :require 'whitespace-final-newline
  :group 'whitespace-final-newline)

(provide 'whitespace-final-newline)
;;; whitespace-final-newline.el ends here
