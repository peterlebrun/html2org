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
   ;(debug response-code)
   (goto-char (point-min)) ;; Ensure that we start at beginning of buffer
   ;(debug (point))
   ;; @TODO Handle other 200
   ;; @TODO Handle error
   (when (and (equal response-code "200") (search-forward "\n\n" nil t))
     (let ((dom (libxml-parse-html-region (point) (point-max))))
       (debug (dom-tag "html"))))))

     ;(debug "foo"))))
     ;(ewl-parse-json))))

;(defun h2o-debug ()
;  "Debug body of HTTP response"
;  (let ((resp (h2o-process-response)))
;    (debug resp)))

(url-retrieve url 'h2o-process-response)
