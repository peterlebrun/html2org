; Provide utility to extract links from web pages
; And turn those into org-mode projects

(require 'dom)

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
     (let* ((dom (buffer-substring (point) (point-max)))
            (first-a-start (string-match "<a " dom))
            (first-a-end (progn
                           (string-match "</a>" dom)
                           (match-end 0)))
            (first-a (substring dom first-a-start first-a-end))
            )
       ;(debug first-a-start)
       ;(debug first-a-end)
       (debug first-a)))))

     ;(search-forward "<body>" nil t)
     ;(goto-char (- (point) 6))
     ;(search-forward "<p>" nil t)
     ;(goto-char (- (point) 3))
     ;(search-forward "</p>"

       ;(when (search-forward "<body" nil t))
       ;(debug doc)
       ;))))
     ;(let* ((doc-dom (libxml-parse-html-region (point) (point-max)))
     ;       (body (caddr (dom-children doc-dom)))
     ;       (div1 (elt body 5)))
       ; TODO: Figure out how to get the p and then the a and loop through those
       ; and grab the hrefs from those a's
       ;(debug (dom-children div1))))))

(url-retrieve url 'h2o-process-response)
