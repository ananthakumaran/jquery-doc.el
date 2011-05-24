;; jQuery api documentation interface for emacs

(require 'jquery-doc-data)

;; xml helpers
(defun xml-get-first-children (node name)
  (car (xml-get-children node name)))

(defun xml-node-first-children (node)
  (car (xml-node-children node)))

(defun xml-string (node)
  (with-temp-buffer-as-string
    (xml-print node)))

;; utils
(defmacro with-temp-buffer-as-string (&rest body)
  "Evaluate BODY inside a temp buffer and returns the buffer string."
  (declare (indent 0))
  `(with-temp-buffer
     ,@body
     (buffer-substring-no-properties (point-min) (point-max))))

(defun jquery-doc-lynx-dump (xml)
  "Returns the lynx dump of the xml as String"
  (let ((temp-file (make-temp-file "temp"))
	(dump ""))
    (with-temp-file temp-file
      (insert xml))
    (with-temp-buffer
      (call-process "lynx" temp-file t nil "-dump" "-stdin" "-nolist")
      (setq dump
	    (replace-regexp-in-string "^\s\s\s" "" (buffer-string))))
    (delete-file temp-file)
    dump))

(defun jquery-doc-format-node (node)
  (if (stringp node)
      (cons 'text "") ;; XXX remove this
    (let ((tag (xml-node-name node)))
      (cond ((memq tag '(code pre))
	     (cons 'js (xml-node-first-children node)))
	    ((eq tag 'html)
	     (cons 'html (xml-node-first-children node)))
	    ((eq tag 'css)
	     (cons 'css (xml-node-first-children node)))
	    (t (cons 'text
		     (jquery-doc-lynx-dump (xml-string (list node)))))))))

(defun jquery-doc-method-name (entry)
  (replace-regexp-in-string "^jQuery\." "$." (xml-get-attribute entry 'name)))
(defun jquery-doc-desc (entry)
  (list (jquery-doc-format-node
	 (xml-get-first-children entry 'desc))))


(defun jquery-doc-argument-options-list (argument)
  (let ((options (xml-get-children argument 'option)))
    (if options
	(mapcar (lambda (option)
		  (list (xml-get-attribute option 'name)
			(xml-get-attribute option 'type)
			(xml-get-attribute option 'default)
			(jquery-doc-lynx-dump
			 (xml-string (xml-get-children argument 'desc)))))
		options))))

(defun jquery-doc-singatures (entry)
  (cons (jquery-doc-method-name entry)
	(mapcar
	 (lambda (signature)
	   (mapcar
	    (lambda (argument)
	      (list (xml-get-attribute argument 'name)
		    (jquery-doc-lynx-dump
		     (xml-string (xml-get-children argument 'desc)))
		    (xml-get-attribute-or-nil argument 'optional)
		    (jquery-doc-argument-options-list argument)))
	    (xml-get-children signature 'argument)))
	 (xml-get-children entry 'signature))))

(defun jquery-doc-longdesc (entry)
  (mapcar #'jquery-doc-format-node
	  (xml-node-children (xml-get-first-children entry 'longdesc))))

(defun jquery-doc-examples (entry)
  (mapcar
   (lambda (example)
     (mapcar #'jquery-doc-format-node
	     (xml-node-children example)))
   (xml-get-children entry 'example)))

(defun jquery-doc-entry (entry)
  `(puthash ,(jquery-doc-method-name entry)
	    '(("name" . ,(jquery-doc-method-name entry))
	      ("signatures" . ,(jquery-doc-singatures entry))
	      ("desc" . ,(jquery-doc-desc entry))
	      ("longdesc" . ,(jquery-doc-longdesc entry))
	      ("examples" . ,(jquery-doc-examples entry)))
	    jquery-doc-hash))

(defun jquery-doc-entries (node)
  (remove-if-not
   (lambda (node)
     (string= (xml-get-attribute node 'type) "method"))
   (append (xml-get-children (car (xml-get-children node 'entries))
			     'entry)
	   (xml-get-children (car (xml-get-children node 'plugins))
			     'entry))))

(defun jquery-doc-generate-data (file)
  "Extracts data from jQuery api documentation xml dump and writes it
to `jquery-doc-data.el' in some convenient format

This function takes long time(it makes many calls to lynx) to finish"
  (let* ((api (car (xml-parse-file file)))
	 (entries (jquery-doc-entries api)))
    (with-temp-file "jquery-doc-data.el"
      (insert
       (with-output-to-string
	 (print `(setq jquery-doc-hash
		       (make-hash-table :size 500 :test 'equal)))
	 (print `(setq jquery-methods '()))
	 (dolist (entry entries)
	   (print `(push ,(jquery-doc-method-name entry) jquery-methods))
	   (print (jquery-doc-entry entry)))
	 (print `(provide 'jquery-doc-data)))))))

(defgroup jquery-doc-faces nil
  "Customize the appearance of jQuery docs"
  :prefix "jquery-doc"
  :group 'faces
  :group 'jquery-doc)

(defface jquery-doc-argument
  '((t :weight bold))
  "Face for function arguments."
  :group 'jquery-doc-faces)

(defface jquery-doc-header
  '((t :weight bold))
  "Face for headers."
  :group 'jquery-doc-faces)

;; utils
(defun insert-with-mode (mode text)
  "Applies the mode to the text and inserts it in the current buffer"
  (let ((temp-buffer (generate-new-buffer "*temp*")))
    (with-current-buffer temp-buffer
      (insert "\n")
      (insert text)
      (funcall mode)
      (indent-region (point-min) (point-max))
      (insert "\n\n")
      (font-lock-fontify-region (point-min) (point-max)))
    (insert-buffer-substring temp-buffer)
    (kill-buffer temp-buffer)))

(defun jquery-doc-insert-with-face (text face)
  "Insert text in current buffer and color it with face"
  (let ((start (point)))
    (insert text)
    (set-text-properties start (point) `(face ,face))))

(defun jquery-doc-insert-with-fill-region (text)
  "Insert text and justifies the text"
  (let ((beg (point)))
    (insert (replace-regexp-in-string "\n+" "\n" text))
    (fill-region beg (point))))

;; argument accessors
(defun jquery-doc-argument-name (argument)
  (car argument))
(defun jquery-doc-argument-desc (argument)
  (cadr argument))
(defun jquery-doc-argument-optional-p (argument)
  (caddr argument))
(defun jquery-doc-argument-options (argument)
  (cadddr argument))

(defun jquery-doc-insert-header (text)
  (jquery-doc-insert-with-face text 'jquery-doc-header))

(defun jquery-doc-insert-argument (text)
  (jquery-doc-insert-with-face text 'jquery-doc-argument))

(defun jquery-doc-insert-blocks (blocks)
  (dolist (block blocks)
    (let ((block-type (car block))
	  (block-str (cdr block)))
      (case block-type
	('text (insert block-str))
	('js (insert-with-mode #'javascript-mode block-str))
	('css (insert-with-mode #'css-mode block-str))
	('html (insert-with-mode #'html-mode block-str))))))

(defun jquery-doc-insert (buffer method-name)
  (let ((method (gethash method-name jquery-doc-hash)))
    (with-current-buffer buffer
      (erase-buffer)
      (font-lock-mode -1)

      ;; short desc
      (jquery-doc-insert-blocks (cdr (assoc "desc" method)))

      ;; signatures
      (let ((signatures (cddr (assoc "signatures" method))))
	(dolist (signature signatures)
	  (insert method-name)
	  (insert "(")
	  (jquery-doc-insert-argument
	   (mapconcat
	    (lambda (argument)
	      (if (jquery-doc-argument-optional-p argument)
		  (concat "["
			  (jquery-doc-argument-name argument)
			  "]")
		(jquery-doc-argument-name argument)))
	    signature
	    ","))
	  (insert ")")
	  (newline)
	  (insert "\n")

	  ;; arguments
	  (dolist (argument signature)
	    (jquery-doc-insert-argument
	     (concat (jquery-doc-argument-name argument)
		     " : "))
	    (jquery-doc-insert-with-fill-region
	     (jquery-doc-argument-desc argument))
	    (insert "\n")
	    (let ((options (jquery-doc-argument-options argument)))
	      (when options
	       (insert "options for ")
	       (jquery-doc-insert-argument
		(jquery-doc-argument-name argument))
	       (newline)
	       (dolist (option options)
		 (let ((name (car option))
		       (type (cadr option))
		       (default (caddr option))
		       (desc (cadddr option)))
		   (jquery-doc-insert-argument name)
		   (newline)
		   (unless (equal "" default)
		     (jquery-doc-insert-header "Default : ")
		     (jquery-doc-insert-with-fill-region default)
		     (newline))
		   (jquery-doc-insert-header "Desc : ")
		   (jquery-doc-insert-with-fill-region desc)
		   (insert "\n")))))))

      ;; long descriptions
      (let ((long-desc (cdr (assoc "longdesc" method))))
	(jquery-doc-insert-header "Description : ")
	(newline)
	(jquery-doc-insert-blocks long-desc))

      ;; examples
      (when (assoc "examples" method)
	(let ((examples (cdr (assoc "examples" method))))
	  (jquery-doc-insert-header "Examples : ")
	  (newline)
	  (dolist (example examples)
	    (jquery-doc-insert-blocks example)
	    (newline))))

      (setq buffer-read-only t)
      (goto-char (point-min))))))

(defun jquery-doc (&optional jquery-method)
  "Displays the jquery doc in a buffer"
  (interactive (list nil))
  (let* ((completing-read-func (if (null ido-mode)
				   'completing-read
				 'ido-completing-read))
	 (method-name (or jquery-method
			  (funcall completing-read-func
				   "jQuery doc: "
				   jquery-methods
				   nil
				   t)))
	 (buffer-name (format "*jQuery doc %s" method-name)))
    (if (get-buffer buffer-name)
	(display-buffer buffer-name)
      (let ((buffer (get-buffer-create buffer-name)))
	(jquery-doc-insert buffer method-name)
	(display-buffer buffer)))))

(defun jquery-documentation (method)
  "Returns the documentation for method as String"
  (let ((method (substring-no-properties method)))
    (with-temp-buffer-as-string
      (jquery-doc-insert (current-buffer) method))))


;; autocomplete
(defun jquery-doc-ac-prefix ()
  (if (re-search-backward "\\(\\$\\.\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
      (match-beginning 1)
    (ac-prefix-default)))

(ac-define-prefix 'jquery-doc 'jquery-doc-ac-prefix)

(ac-define-source jquery
  '((candidates . jquery-methods)
    (symbol . "f")
    (document . jquery-documentation)
    (prefix . jquery-doc)
    (cache)))

(defun jquery-doc-setup ()
  (setq ac-sources (append '(ac-source-jquery) ac-sources)))

(provide 'jquery-doc)
