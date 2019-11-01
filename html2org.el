; Provide utility to extract Table of Contents from wiki pages
; And turn those into org-mode projects

;sicp/emacs for further testing
;(setq sicp-base-url "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/")
;(setq emacs-base-url "https://www.gnu.org/software/emacs/manual/html_mono/emacs.html")
(setq wikipedia-url "https://en.wikipedia.org/wiki/Lisp_(programming_language)")

(defun w2o-prepare-buffer ()
  "Create consistent buffer object for displaying temp results"
  (let ((buf (get-buffer-create "w2o-tmp-buf")))
    (with-current-buffer buf
      (erase-buffer)
      (kill-all-local-variables)
      (org-mode))
    buf))

(defun w2o-extract-toc ()
  "Extract Table of Contents from wikipedia document"
  (let* ((doc (buffer-substring (point) (point-max)))
         (start (string-match "<div id=\"toc\" class=\"toc\">" doc))
         ; There's a div in the middle of the TOC div
         (middle (progn
                   (string-match "</div>" doc start)
                   (match-end 0)))
         (end (string-match "</div>" doc middle))
         (toc (substring doc start end)))
    toc))

(defun w2o-extract-anchors (doc)
  "Extract anchor tags and text from DOC"
  (setq anchors ())
  (setq pos 0)
  (while (string-match-p "<a" doc pos)
    (let* ((start (string-match "<a" doc pos))
           (end (progn
                  (string-match "</a>" doc start)
                  (match-end 0)))
           (anchor (substring doc start end)))
      (setq anchors (append anchors (list anchor)))
      (setq pos end)))
  anchors)

(defun w2o-extract-attr (elem attr-start attr-end)
  "Return substring of ELEM between ATTR-START and ATTR-END."
  (if (string-match-p attr-start elem)
      (let* ((start (progn
                      (string-match attr-start elem)
                      (match-end 0)))
             (end (string-match attr-end elem start)))
        (substring elem start end))
    nil))

(defun w2o-extract-response-code ()
  "Extract HTTP response code from response buffer."
  (goto-char (point-min)) ;Ensure that we start at beginning of buffer
  (cadr (split-string (thing-at-point 'line))))

(defun w2o-parse-response ()
  "Parse HTTP response"
  (setq anchors (w2o-extract-anchors (w2o-extract-toc)))
  (setq output (concat "** NOT STARTED " base-url "\n:PROPERTIES:\n:ORDERED:  t\n:END:"))
  (while (car anchors)
    (let* ((anchor (car anchors))
           (link (w2o-extract-attr anchor "<a href=\"" "\""))
           (num (w2o-extract-attr anchor "<span class=\"tocnumber\">" "</span>"))
           (text (w2o-extract-attr anchor "<span class=\"toctext\">" "</span>"))
           (todo (concat "\n*** TODO [[" base-url link "][" num " " text "]]")))
      (setq output (concat output todo)))
    (setq anchors (cdr anchors)))
  (with-current-buffer (w2o-prepare-buffer)
    (insert output)
    (pop-to-buffer (current-buffer))))

(defun w2o-process-response (status cb)
 "Extract the html response from the buffer returned by url-http.  STATUS is discarded."
 (set-buffer-multibyte t)
 (when (and (equal (w2o-extract-response-code) "200") (search-forward "\n\n" nil t))
   (funcall cb)))

(defun w2o-get-wiki-toc ()
  "Parse wikipedia ToC into org-mode reading project"
  (interactive)
  ;set url globally so we have it available
  (setq base-url (read-from-minibuffer "Enter URL: "))
  (url-retrieve base-url 'w2o-process-response '(w2o-parse-response)))
