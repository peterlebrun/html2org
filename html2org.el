; Provide utility to extract links from web pages
; And turn those into org-mode projects

(setq sicp-base-url "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/")
(setq url (concat sicp-base-url "book-Z-H-4.html#%_toc_start"))
(setq project-title (read-minibuffer "Enter Project Title: "))

(defun h2o-extract-response-code ()
  "Extract HTTP response code from response buffer."
  (goto-char (point-min))
  (cadr (split-string (thing-at-point 'line))))

(defun h2o-extract-href (elem)
  "Extract href attribute from ELEM"
  (if (string-match-p "href=\"" elem)
      (let* ((start (progn
                      (string-match "href=\"" elem)
                      (match-end 0)))
             (end (string-match "\"" elem start)))
        (substring elem start end))
    ""))

(defun h2o-extract-text (elem)
  "Extract text from ELEM"
  (if (string-match-p ">" elem)
      (let* ((start (progn
                      (string-match ">" elem)
                      (match-end 0)))
             (end (string-match "<" elem start)))
        (substring elem start end))
    ""))

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
     (setq foobar ())
     (while (string-match-p "<a " doc position)
       (let* ((start (string-match "<a " doc position))
              (end (progn
                     (string-match "</a>" doc start)
                     (match-end 0))))
         (setq results (append results (list (substring doc start end))))
         (setq position end)))
     (while results
       (let* ((elem (car results))
              (href (h2o-extract-href elem))
              (text (h2o-extract-text elem)))
         (setq foobar (append foobar (list (list href text)))))
       (setq results (cdr results)))
     (with-current-buffer (h2o-prepare-buffer)
       (while foobar
         (let* ((current (car foobar))
                (href (car current))
                (text (cadr current))) ; @TODO Replace &nbsp; with spaces
           (insert
            (h2o-prepare-org-task
             (h2o-prepare-link href text sicp-base-url))))
         (setq foobar (cdr foobar)))
       (pop-to-buffer (current-buffer))))))

(defun h2o-prepare-org-task (task)
  "Provide org-mode styled task"
  (concat "** TODO " task))

(defun h2o-prepare-link (href text &optional base-url)
  "Make an org-mode link from HREF (using BASE-URL if provided) displaying TEXT"
  (let ((url (concat (if base-url base-url "") href))
        (clean-text (replace-regexp-in-string "&nbsp;&nbsp;" " " text)))
    (concat "[[" url "][" clean-text "]]" "\n")))

(defun h2o-prepare-buffer ()
  "Create consistent buffer object for displaying temp results"
  (let ((buf (get-buffer-create "h2o-tmp-buf")))
    (with-current-buffer buf
      (erase-buffer)
      (kill-all-local-variables)
      (org-mode))
    buf))

(url-retrieve url 'h2o-process-response)
