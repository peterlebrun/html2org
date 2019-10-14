; Provide utility to extract links from web pages
; And turn those into org-mode projects

(setq urlbase "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/")
(setq url (concat urlbase "book-Z-H-4.html#%_toc_start"))

(defun h2o-extract-response-code ()
  "Extract HTTP response code from response buffer."
  (goto-char (point-min))
  (cadr (split-string (thing-at-point 'line))))

(defun h2o-extract-href (a)
  "Extract href attribute from anchor tag A"
  ; TODO define body here
  )

(defun h2o-extract-text (a)
  "Extract text from anchor tag A"
  ; TODO define body here
  )

(defun h2o-process-response (status)
 "Extract the html response from the buffer returned by url-http.  STATUS is discarded."
 (set-buffer-multibyte t)
 (let ((response-code (h2o-extract-response-code)))
   (goto-char (point-min)) ;; Ensure that we start at beginning of buffer
   ;; @TODO Handle other 200
   ;; @TODO Handle error
   (when (and (equal response-code "200") (search-forward "\n\n" nil t))
     (setq doc (buffer-substring (point) (point-max)))
     (setq results ())
     (setq position 0)
     (while (string-match-p "<a " doc position)
       (let* ((start (string-match "<a " doc position))
              (end (progn
                     (string-match "</a>" doc start)
                     (match-end 0))))
         (setq results (append results (list (substring doc start end))))
         (setq position end)))
     ; TODO dolist over results to use extract-href and extract-text
     (debug results))))

(url-retrieve url 'h2o-process-response)
