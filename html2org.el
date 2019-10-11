; Provide utility to extract links from web pages
; And turn those into org-mode projects

(require 'dom)

(setq url "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-4.html#%_toc_start")

(defun h2o-extract-response-code ()
  "Extract HTTP response code from response buffer."
  (goto-char (point-min))
  (cadr (split-string (thing-at-point 'line))))

; TODO Keep track of point to continue advancing
(defun h2o-get-next-a (doc)
  "Get next anchor tag from doc"
  (let* ((start (string-match "<a " doc))
         (end (progn
                (string-match "</a>" doc)
                (match-end 0))))
    (substring doc start end)))

(defun h2o-process-response (status)
 "Extract the html response from the buffer returned by url-http."
 (set-buffer-multibyte t)
 (let ((response-code (h2o-extract-response-code)))
   (goto-char (point-min)) ;; Ensure that we start at beginning of buffer
   ;; @TODO Handle other 200
   ;; @TODO Handle error
   (when (and (equal response-code "200") (search-forward "\n\n" nil t))
     (let* ((doc (buffer-substring (point) (point-max))))
       (debug (h2o-get-next-a doc))))))

(url-retrieve url 'h2o-process-response)
