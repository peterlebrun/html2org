; Provide utility to extract links from web pages
; And turn those into org-mode projects

(setq url "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-4.html#%_toc_start")

(defun h2o-extract-response-code ()
  "Extract HTTP response code from response buffer."
  (goto-char (point-min))
  (cadr (split-string (thing-at-point 'line))))

(defun h2o-process-response (status)
 "Extract the html response from the buffer returned by url-http."
 (set-buffer-multibyte t)
 (let ((response-code (h2o-extract-response-code)))
   (goto-char (point-min)) ;; Ensure that we start at beginning of buffer
   ;; @TODO Handle other 200
   ;; @TODO Handle error
   (when (and (equal response-code "200") (search-forward "\n\n" nil t))
     (let* ((doc-dom (libxml-parse-html-region (point) (point-max)))
            (body (caddr (dom-children doc-dom))))
       (debug (dom-children body))))))

(url-retrieve url 'h2o-process-response)
