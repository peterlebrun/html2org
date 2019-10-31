; Provide utility to extract links from web pages
; And turn those into org-mode projects

(setq sicp-base-url "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/")
(setq emacs-base-url "https://www.gnu.org/software/emacs/manual/html_mono/emacs.html")
(setq wikipedia-url "https://en.wikipedia.org/wiki/Black_Panther_Party")
;(setq url (concat emacs-base-url "index.html"))
;(setq url wikipedia-url)
;(setq h2o-project-title (read-from-minibuffer "Enter Project Title: "))
;(setq url (concat sicp-base-url "book-Z-H-4.html#%_toc_start"))
;(setq project-title (read-from-minibuffer "Enter Project Title: "))

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

(defun h2o-extract-elem (elem dom pos)
  "Extract ELEM from DOM starting at POS."
  (let ((elem-start (concat "<" elem))
        (elem-end (concat "</" elem ">")))
    (if (string-match-p elem-start dom)
        (let* ((start (string-match elem-start dom))
               (end (string-match elem-end dom start)))
          (substring doc start end))
      nil)))

(defun h2o-process-response (status cb)
 "Extract the html response from the buffer returned by url-http.  STATUS is discarded."
 (set-buffer-multibyte t)
 (let ((response-code (h2o-extract-response-code)))
   (goto-char (point-min)) ;; Ensure that we start at beginning of buffer
   ;; @TODO Handle other 200
   ;; @TODO Handle error
   (when (and (equal response-code "200") (search-forward "\n\n" nil t))
     (funcall cb))))

(defun h2o-parse-doc ()
  ""
     (setq doc (buffer-substring (point) (point-max)))
     ; Tables will provide the links
     (setq tables ())
     (setq anchors-headers ())
     (setq position 0)
     (while (string-match-p "<table " doc position)
       (let* ((start (string-match "<table " doc position))
              (end (progn
                     (string-match "</table>" doc start)
                     (match-end 0)))
              (table (list (substring doc start end))))
         (setq tables (append tables table))
         (setq position end)))
     (while (string-match-p "<a name=" doc position)
       (let* ((start (string-match "<a name=" doc position))
              (p-start (string-match "<p>" doc start))
              (h-start (string-match "<h" doc p-start))
              (end (progn
                     (string-match "</h" doc h-start)
                     (match-end 0)))
              (text (concat (substring doc start p-start) (substring doc h-start end))))
         (setq anchors-headers (append anchors-headers (list text)))
         (setq position end)))
     (debug anchors-headers))

  ;   (setq results ())
  ;   (setq position 0)
  ;   (setq foobar ())
  ;   (while (string-match-p "<a " doc position)
  ;     (let* ((start (string-match "<a " doc position))
  ;            (end (progn
  ;                   (string-match "</a>" doc start)
  ;                   (match-end 0))))
  ;       (setq results (append results (list (substring doc start end))))
  ;       (setq position end)))
  ;   (while results
  ;     (let* ((elem (car results))
  ;            (href (h2o-extract-href elem))
  ;            (text (h2o-extract-text elem)))
  ;       (setq foobar (append foobar (list (list href text)))))
  ;     (setq results (cdr results)))
  ;   (with-current-buffer (h2o-prepare-buffer)
  ;     (insert (concat "** NOT STARTED " h2o-project-title "\n"))
  ;     (while foobar
  ;       (let* ((current (car foobar))
  ;              (href (car current))
  ;              (text (cadr current))) ; @TODO Replace &nbsp; with spaces
  ;         (insert
  ;          (h2o-prepare-org-task
  ;           (h2o-prepare-link href text sicp-base-url))))
  ;       (setq foobar (cdr foobar)))
  ;     (pop-to-buffer (current-buffer))))))

(defun h2o-prepare-org-task (task)
  "Provide org-mode styled task"
  (concat "*** TODO " task))

(defun h2o-prepare-link (href text &optional base-url)
  "Make an org-mode link from HREF (using BASE-URL if provided) displaying TEXT"
  (let ((url (concat (if base-url base-url "") href))
        (clean-text (replace-regexp-in-string "&nbsp;&nbsp;" " " text)))
    (concat "[[" url "][" clean-text "]]" text "\n")))

(defun h2o-prepare-buffer ()
  "Create consistent buffer object for displaying temp results"
  (let ((buf (get-buffer-create "h2o-tmp-buf")))
    (with-current-buffer buf
      (erase-buffer)
      (kill-all-local-variables)
      (org-mode))
    buf))

(defun h2o-wiki-parse-response ()
  ""
  (let* ((toc (h2o-wiki-get-toc))
         (tags (if toc (h2o-wiki-get-anchors toc) nil))
         nil)))
        ;(debug tags))))

(defun h2o-wiki-get-anchors (doc)
  "Extract anchor tags and text from DOC"
  (setq pos 0)
  (setq anchors ())
  (while (string-match-p "<a" doc pos)
    (let* ((start (string-match "<a" doc pos))
           (end (progn
                  (string-match "</a>" doc start)
                  (match-end 0))))
      (setq anchors (append anchors (list (substring doc start end))))
      (setq pos end)))
  (debug anchors))

(defun h2o-wiki-get-toc ()
  "Extract Table of Contents from wikipedia document"
  (let* ((doc (buffer-substring (point) (point-max)))
         (start (if
                    (string-match-p "<div id=\"toc\"" doc)
                    (string-match "<div id=\"toc\"" doc)
                  nil))
         (end (if start (string-match "</ul>" doc start) nil))
         (toc (if (and start end) (substring doc start end) nil)))
    toc))

; @TODO: Improve name
; @TODO: parse div id="toc" through "</ul>"
; @TODO: make it interactive so M-x h2o-get-wiki-page www.wikipedia.org/bar works
(defun h2o-wikipedia-project ()
  "Parse wikipedia ToC into org-mode reading project"
  (interactive)
  ;set url globally so we have it available
  (defvar *h2o-base-url* (read-from-minibuffer "Enter URL: "))
  (url-retrieve *h2o-base-url* 'h2o-process-response '(h2o-wiki-parse-response)))
