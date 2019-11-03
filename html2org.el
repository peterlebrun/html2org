; Provide utility to extract Table of Contents from wiki pages
; And turn those into org-mode projects

; @TODO: If current buffer is w2o-org-file, it should not be killed

(defgroup w2o nil
  "A simple utility to turk wikipedia pages into org-mode projects"
  :group 'tools)

(defcustom w2o-org-file ""
  "Org file to write new projects into"
  :group 'w2o
  :type 'string)

(defcustom w2o-add-ordered-property t
  "When non-nil, add ordered property to org project"
  :group 'w2o
  :type 'boolean)

(defcustom w2o-write-full-url-for-project-title nil
  "When non-nil, write the full URL for the project title"
  :group 'w2o
  :type 'boolean)

(defcustom w2o-inspect-project-before-writing-to-file t
  "When non-nil display output in buffer before writing to file"
  :group 'w2o
  :type 'boolean)

(defcustom w2o-indent-toc-levels nil
  "When non-nil, increase heading level of TODOs based on wiki tocnumber"
  :group 'w2o
  :type 'boolean)

(defcustom w2o-project-level-start 2
  "Level of top level project header"
  :group 'w2o
  :type 'integer)

(defcustom w2o-project-todo-string "NOT STARTED"
  "TODO text for top level project header"
  :group 'w2o
  :type 'string)

(defcustom w2o-item-todo-string "TODO"
  "TODO text for individual headers under top-level project"
  :group 'w2o
  :type 'string)

(defcustom w2o-project-tags '("wiki")
  "Tags to add to the project"
  :group 'w2o
  :type 'list)

(defun w2o-extract-toc ()
  "Extract Table of Contents from wikipedia document"
  (let* ((doc (buffer-substring (point) (point-max)))
         (start (string-match "<div id=\"toc\" class=\"toc\">" doc))
         ; There's a div in the middle of the TOC div
         (middle (progn
                   (string-match "</div>" doc start)
                   (match-end 0)))
         (end (string-match "</div>" doc middle))
         (toc (if (and start end) (substring doc start end) nil)))
    (if toc
        toc
      (error "Could not find table of contents in document"))))

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
  (if anchors
      anchors
    (error "No anchor tags found in doc")))

(defun w2o-extract-attr (elem attr-start attr-end)
  "Return substring of ELEM between ATTR-START and ATTR-END."
  (if (string-match-p attr-start elem)
      (let* ((start (progn
                      (string-match attr-start elem)
                      (match-end 0)))
             (end (string-match attr-end elem start)))
        (substring elem start end))
    (error (concat "\"" attr-start "\" not found in " elem))))

(defun w2o-extract-response-code ()
  "Extract HTTP response code from response buffer."
  (goto-char (point-min)) ;Ensure that we start at beginning of buffer
  (cadr (split-string (thing-at-point 'line))))

(defun w2o-get-top-level-project-header ()
  "Produce top-level header for project"
  (concat
   (string-join
    (list
     (make-string w2o-project-level-start ?*)
     w2o-project-todo-string
     (if w2o-write-full-url-for-project-title
         w2o-base-url
       (car (last (split-string w2o-base-url "/"))))
     (concat ":" (string-join w2o-project-tags ":") ":")
     (if w2o-add-ordered-property "\n:PROPERTIES:\n:ORDERED:  t\n:END:"))
    " ")
   (w2o-make-todo-string "" "Intro")))

(defun w2o-make-todo-string (link text)
  "Produce TODO string from LINK TEXT and a couple globals"
  (concat
   "\n"
   (make-string (+ w2o-project-level-start 1) ?*)
   " "
   w2o-item-todo-string
   " "
   "[[" w2o-base-url link "][" text "]]"))

(defun w2o-parse-response ()
  "Parse HTTP response"
  (setq anchors (w2o-extract-anchors (w2o-extract-toc)))
  (setq output (w2o-get-top-level-project-header))
  (while anchors
    (let* ((anchor (pop anchors))
           (link (w2o-extract-attr anchor "<a href=\"" "\""))
           (num (w2o-extract-attr anchor "<span class=\"tocnumber\">" "</span>"))
           (text (w2o-extract-attr anchor "<span class=\"toctext\">" "</span>")))
      (setq output (concat output (w2o-make-todo-string link (concat num " " text))))))
  (if w2o-inspect-project-before-writing-to-file
      (with-current-buffer (find-file-noselect w2o-org-file)
        (goto-char (point-max))
        (insert output)
        (save-buffer)
        (pop-to-buffer (current-buffer)))
    (w2o-write-project-to-file output)))

(defun w2o-write-project-to-file (project-text)
  "Write PROJECT-TEXT to desired project file"
  (if (not w2o-org-file)
      (error "Could not write to file; please set w2o-org-file"))
  (with-current-buffer (find-file-noselect w2o-org-file)
    (goto-char (point-max))
    (insert project-text)
    (save-buffer)))

(defun w2o-process-response (status cb)
 "Extract the html response from the buffer returned by url-http.  STATUS is discarded."
 (set-buffer-multibyte t)
 (when (and (equal (w2o-extract-response-code) "200") (search-forward "\n\n" nil t))
   (funcall cb)))

(defun w2o-get-url ()
  "Get URL from user"
  (let ((url (read-from-minibuffer "Enter URL: ")))
    (if (string-match-p "wikipedia.org" url)
        url
      (error (concat "Please enter a wikipedia URL")))))

(defun w2o-save-wikipedia-to-project ()
  "Parse wikipedia ToC into org-mode reading project"
  (interactive)
  (setq w2o-base-url (w2o-get-url))
  (url-retrieve w2o-base-url 'w2o-process-response '(w2o-parse-response)))
