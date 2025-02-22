; Provide utility to extract Table of Contents from wiki pages
; And turn those into org-mode projects

; @TODO: If current buffer is w2o-org-file, it should not be killed
; @TODO: provide option to not include errata
; Examples:
;*** TODO [[https://en.wikipedia.org/wiki/Fractal#See_also][10 See also]]
;*** TODO [[https://en.wikipedia.org/wiki/Fractal#Notes][11 Notes]]
;*** TODO [[https://en.wikipedia.org/wiki/Fractal#References][12 References]]
;*** TODO [[https://en.wikipedia.org/wiki/Fractal#Further_reading][13 Further reading]]
;*** TODO [[https://en.wikipedia.org/wiki/Fractal#External_links][14 External links]]
; @TODO: How to handle no ToC? (ex: https://en.wikipedia.org/wiki/Coimage)
; @TODO: How to handle #section in URL? (eg: https://en.wikipedia.org/wiki/Kernel_(linear_algebra)#Left_null_space)
; @TODO: Provide ability to remove underscores from URL for project name
; @TODO: How many words in the article/est total reading time?
; @TODO: Align tags via org commands
; @TODO: Add tags via org commands

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

(defcustom w2o-include-statistics-cookie t
  "Whether to include the statistics cookie [%] for project header"
  :group 'w2o
  :type 'boolean)

(defcustom w2o-statistics-cookie "[0%]"
  "Define custom statistics cookie"
  :group 'w2o
  :type 'string)

(defcustom w2o-remove-underscores-from-project-name t
  "Whether to remove underscores from Wikipedia title for the project name"
  :group 'w2o
  :type 'boolean)

(defun w2o-extract-toc (open-tag close-tag &optional middle-tag)
  "Extract Table of Contents from wikipedia document"
  (let* ((doc (buffer-substring (point) (point-max)))
         (start (string-match open-tag doc))
         (middle (if middle-tag (progn
                                  (string-match middle-tag doc start)
                                  (match-end 0))))
         (end (string-match close-tag doc (if middle middle start)))
         (toc (if (and start end) (substring doc start end) nil)))
    (if toc
        toc
      ;@TODO: Handle case of no TOC more gracefully
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

(defun w2o-get-top-level-project-header (&optional title)
  "Produce top-level header for project"
  (concat
   (string-join
    (list
     (make-string w2o-project-level-start ?*)
     w2o-project-todo-string
     (if w2o-write-full-url-for-project-title
         w2o-base-url
       (if title
           title
         (if w2o-remove-underscores-from-project-name
             (replace-regexp-in-string "_" " " (car (last (split-string w2o-base-url "/"))))
           (car (last (split-string w2o-base-url "/"))))))
     (if w2o-include-statistics-cookie w2o-statistics-cookie)
     (concat ":" (string-join w2o-project-tags ":") ":")
     (if w2o-add-ordered-property "\n:PROPERTIES:\n:ORDERED:  t\n:END:"))
    " ")
   (w2o-make-todo-string "" "Intro")))

(defun w2o-make-todo-string (link text &optional urlbase)
  "Produce TODO string from LINK TEXT and a couple globals"
  (concat
   "\n"
   (make-string (+ w2o-project-level-start 1) ?*)
   " "
   w2o-item-todo-string
   " "
   "[[" (if urlbase urlbase w2o-base-url) link "][" text "]]"))

(defun w2o-parse-response ()
  "Parse HTTP response"
  (setq anchors (w2o-extract-anchors
                 (w2o-extract-toc "<div id=\"toc\" class=\"toc\">" "</div" "</div>")))
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

; Below are basically experimental
;  "Half ass is okay if it's the right half of the ass"
(defun w2o-save-quant-econ-toc-to-project ()
  "Parse Quant Econ TOC into learning project"
  (interactive)
  (setq w2o-base-url "https://python.quantecon.org/index_toc.html")
  (url-retrieve w2o-base-url 'w2o-process-response '(w2o-parse-quant-econ-response)))

(defun w2o-extract-quant-econ-toc ()
  "Extract Table of Contents from Quant Econ ToC"
  (let* ((doc (buffer-substring (point) (point-max)))
         (skip (string-match "<div class=\"inner_cell\">" doc))
         (start (string-match "<ul>" doc skip))
         (end (string-match "</div>" doc start))
         (toc (if (and start end) (substring doc start end) nil)))
    (if toc
        toc
      (error "Could not find table of contents in document"))))

(defun w2o-parse-quant-econ-response ()
  "Parse HTTP response"
  (setq anchors (w2o-extract-anchors (w2o-extract-quant-econ-toc)))
  (setq output (w2o-get-top-level-project-header "Quant Econ"))
  (setq urlbase "https://python.quantecon.org/")
  (while anchors
    (let* ((anchor (pop anchors))
           (link (w2o-extract-attr anchor "<a href=\"" "\""))
           (text (w2o-extract-attr anchor ">" "</a>")))
      (setq output (concat output (w2o-make-todo-string link text urlbase)))))
      (with-current-buffer (find-file-noselect w2o-org-file)
        (goto-char (point-max))
        (insert output)
        (save-buffer)
        (pop-to-buffer (current-buffer))))

(defun w2o-parse-project (source-file project-header urlbase)
  (let* ((src (with-temp-buffer
                (insert-file-contents source-file)
                (buffer-string)))
              (anchors (w2o-extract-anchors src))
              (output (w2o-get-top-level-project-header project-header)))
    (while anchors
      (let* ((anchor (pop anchors))
             (link (w2o-extract-attr anchor "<a href=\"" "\""))
             (text (w2o-extract-attr anchor ">" "</a>")))
        (setq output (concat output (w2o-make-todo-string link text urlbase)))))
    (with-current-buffer (find-file-noselect w2o-org-file)
      (goto-char (point-max))
      (insert output)
      (save-buffer)
      (pop-to-buffer (current-buffer)))))

; Pro-Git
;(w2o-parse-project "./pro-git-src" "Pro-Git" "https://git-scm.com"))

; Numpy
;(w2o-parse-project "./numpy-src" "NumPy" "https://docs.scipy.org/doc/numpy/")

; Stanford NLP
;(w2o-parse-project "./stanford-nlp-src" "Stanford NLP" "https://nlp.stanford.edu/IR-book/html/htmledition/")

; SciPy
;(w2o-parse-project "./scipy-src" "SciPy" "https://docs.scipy.org/doc/scipy/reference/")

; Paul Graham Essays
;(w2o-parse-project "./paul-graham-src" "Paul Graham Essays" "http://www.paulgraham.com/")

; GA Tech Linear Algebra
;(w2o-parse-project "./lin-alg-src" "Linear Algebra" "https://textbooks.math.gatech.edu/ila/")

; Pandas User Guide
;(w2o-parse-project "./pandas-getting-started-src" "Pandas - Getting Started" "https://pandas.pydata.org/pandas-docs/stable/getting_started/")
;(w2o-parse-project "./pandas-user-guide-src" "Pandas - User Guide" "https://pandas.pydata.org/pandas-docs/stable/user_guide/")

;
;(w2o-parse-project "./sympy-src" "SymPy" "https://docs.sympy.org/latest/")

;(w2o-parse-project "./numba-src" "Numba" "https://numba.pydata.org/numba-doc/latest/")
;(w2o-parse-project "./numba-1-src" "Numba 1" "https://numba.pydata.org/numba-doc/latest/user/")
;(w2o-parse-project "./numba-2-src" "Numba 2" "https://numba.pydata.org/numba-doc/latest/reference/")

; Emacs Manual - going to require a special remap
;(w2o-parse-project "./emacs-manual-src" "Emacs" "https://www.gnu.org/software/emacs/manual/html_mono/emacs.html")

; Intro to Emacs Lisp - going to require another special remap
;(w2o-parse-project "./intro-emacs-lisp-src" "Intro to Emacs Lisp" "https://www.gnu.org/software/emacs/manual/html_mono/eintr.html")

; Emacs Lisp (full hog) - samesies on special remap
;(w2o-parse-project "./emacs-lisp-src" "Emacs Lisp" "https://www.gnu.org/software/emacs/manual/html_mono/elisp.html")

(defun w2o-load-file (filename)
  ""
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq emacs-manual
      '("https://www.gnu.org/software/emacs/manual/html_mono/emacs.html"
        "./emacs-manual-src-html"
        "Emacs Manual"))
(setq emacs-lisp-intro
      '("https://www.gnu.org/software/emacs/manual/html_mono/eintr.html"
        "./intro-emacs-lisp-html-src"
        "Intro to Emacs Lisp"))
(setq emacs-lisp
      '("https://www.gnu.org/software/emacs/manual/html_mono/elisp.html"
        "./emacs-lisp-html-src"
        "Emacs Lisp Manual"))

(defun get-last-attr (attr start end doc)
  "Return last instance of ATTR between START and END in string DOC"
  (let ((substr (substring doc start end))
	(ptr 0))
    (while (string-match-p attr substr ptr)
      (let ((name-ptr (progn
			    (string-match attr substr ptr)
			    (match-end 0))))
	(setq ptr name-ptr)))
    (substring substr ptr (string-match "\"" substr ptr))))

(defun extract-named-anchor-data (data)
  (let* ((w2o-base-url (nth 0 data))
         (doc (w2o-load-file (nth 1 data)))
         (output (w2o-get-top-level-project-header (nth 2 data)))
         (token "<a name=\"")
         (pos (string-match token doc))) ;start w/ first instance
    (while (string-match token doc pos)
      (let* ((anchor-start (string-match token doc pos))
	     (header-open-start (string-match "<h" doc anchor-start))
	     (header-open-end (progn
				(string-match ">" doc header-open-start)
				(match-end 0)))
             (header-close-start (string-match "</h" doc header-open-end))
             (name (get-last-attr token anchor-start header-open-start doc))
             (text (substring doc header-open-end header-close-start)))
        (setq pos header-close-start)
        (setq output (concat output (w2o-make-todo-string (concat "#" name) text)))))
    (with-current-buffer (find-file-noselect w2o-org-file)
      (goto-char (point-max))
      (insert output)
      (save-buffer)
      (pop-to-buffer (current-buffer)))))
