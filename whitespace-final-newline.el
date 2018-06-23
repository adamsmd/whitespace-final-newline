;;; whitespace-final-newline -- TODO:Summary
;;; Commentary:
;;; Code:

(defun final-newline-p (buffer-position)
  "TODO"
  (or
   ;; Point is at end of file
   (= (point) buffer-position)
   ;; There is a newline at the end of the file
   (= 0 (save-excursion (goto-char buffer-position) (current-column)))))

;; TODO: (define-face whitespace-no-newline-at-end-of-buffer

(defun go ()
  "TODO"
  (interactive)
  (let ((s (propertize
            (concat
             (propertize " <-- No final newline"
              'display `((when (final-newline-p buffer-position) . "")))
             (propertize " "
              'display '((when (final-newline-p buffer-position) . "")
                         (space :align-to right))))
            'cursor t
            'face '(:background "#aa00aa"))))
    (let* ((b (current-buffer))
           (o (make-overlay (1+ (buffer-size b)) (1+ (buffer-size b)) b t t)))
      (overlay-put o 'after-string s))))

;              (propertize "XXX"
;               'display `(((margin left-margin) "ABC")
;))
;    (setf left-margin-width 5)

(provide 'whitespace-final-newline)
;;; whitespace-final-newline ends here
