;;; jquery-doc.el --- jQuery api documentation interface for emacs

;; Copyright (C) 2011 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; Version: 1.10
;; Keywords: docs, jquery

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; provides completion source for autocomplete, documentation lookup

;;; Code:

(require 'auto-complete nil t)
(require 'company nil t)
(require 'find-func)
(require 'jquery-doc-data)
(require 'cl)
(require 'xml)

;; utils
(defmacro jquery-doc-with-temp-buffer-as-string (&rest body)
  "Evaluate BODY inside a temp buffer and return the buffer string."
  (declare (indent 0))
  `(with-temp-buffer
     ,@body
     (buffer-substring-no-properties (point-min) (point-max))))

;; xml helpers
(defun jquery-doc-xml-get-first-children (node name)
  (car (xml-get-children node name)))

(defun jquery-doc-xml-node-first-children (node)
  (car (xml-node-children node)))

(defun jquery-doc-xml-string (node)
  (jquery-doc-with-temp-buffer-as-string
    (xml-print node)))

(defun jquery-doc-lynx-dump (xml)
  "Return the lynx dump of the XML as String."
  (let ((temp-file (make-temp-file "temp"))
	(dump ""))
    (with-temp-file temp-file
      (insert xml))
    (with-temp-buffer
      (call-process "lynx" temp-file t nil "-dump" "-stdin" "-nolist" "-assume-charset=utf-8")
      (setq dump
	    (replace-regexp-in-string "^\s\s\s" "" (buffer-string))))
    (delete-file temp-file)
    dump))

(defun jquery-doc-code-type (code)
  (if (string-match-p "^ *<[^>]*>.*<[^>]*> *$" code)
      'html
    'js))

(defun jquery-doc-format-node (node)
  (if (stringp node)
      (cons 'text "") ;; XXX remove this
    (let ((tag (xml-node-name node)))
      (cond ((eq tag 'code)
	     (cons 'js (jquery-doc-xml-node-first-children node)))
            ((eq tag 'pre)
             (let ((code (jquery-doc-xml-node-first-children
                          (find-if
                           (lambda (node)
                             (and (listp node) (equal (xml-node-name node) 'code)))
                           (xml-node-children node)))))
               (cons (jquery-doc-code-type code)
                     code)))
	    ((eq tag 'html)
	     (cons 'html (jquery-doc-xml-node-first-children node)))
	    ((eq tag 'css)
	     (cons 'css (jquery-doc-xml-node-first-children node)))
	    (t (cons 'text
		     (jquery-doc-lynx-dump (jquery-doc-xml-string (list node)))))))))

(defun jquery-doc-method-name (entry)
  (replace-regexp-in-string "^jQuery\." "$." (xml-get-attribute entry 'name)))
(defun jquery-doc-desc (entry)
  (list (jquery-doc-format-node
	 (jquery-doc-xml-get-first-children entry 'desc))))


(defun jquery-doc-argument-options-list (argument)
  (let ((options (xml-get-children argument 'option)))
    (if options
	(mapcar (lambda (option)
		  (list (xml-get-attribute option 'name)
			(xml-get-attribute option 'type)
			(xml-get-attribute option 'default)
			(jquery-doc-lynx-dump
			 (jquery-doc-xml-string (xml-get-children argument 'desc)))))
		options))))

(defun jquery-doc-singatures (entry)
  (cons (jquery-doc-method-name entry)
	(mapcar
	 (lambda (signature)
	   (mapcar
	    (lambda (argument)
	      (list (xml-get-attribute argument 'name)
		    (jquery-doc-lynx-dump
		     (jquery-doc-xml-string (xml-get-children argument 'desc)))
		    (xml-get-attribute-or-nil argument 'optional)
		    (jquery-doc-argument-options-list argument)))
	    (xml-get-children signature 'argument)))
	 (xml-get-children entry 'signature))))

(defun jquery-doc-longdesc (entry)
  (mapcar #'jquery-doc-format-node
	  (xml-node-children (jquery-doc-xml-get-first-children entry 'longdesc))))

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

(defvar jquery-doc-api-xml-url
  "http://api.jquery.com/resources/api.xml"
  "Default source used to download the API xml data.")

(defun jquery-doc-ensure-api-xml (&optional update)
  "Ensure api.xml locally.
Downloads api.xml from `jquery-doc-api-xml-url' if it is not
already in the system, otherwise return the path of the local
copy.  When optional argument UPDATE is non-nil, always downloads
a fresh copy of the xml API."
  (let ((to (expand-file-name "api.xml"
                              (file-name-directory
                               (find-library-name "jquery-doc")))))
    (if (and (not update) (file-exists-p to))
        to
      (if (url-copy-file jquery-doc-api-xml-url to t)
          to
        (error "jquery-doc: Cannot download api data.")))))

(defun jquery-doc-fetch-and-generate-data (&optional update)
  "Download and generate doc data from API xml.
Uses `jquery-doc-ensure-api-xml' to ensure the local copy of
api.xml is available before trying to generate data.  When
optional argument UPDATE is non-nil, always downloads a fresh
copy of the xml API."
  (interactive "P")
  (jquery-doc-generate-data (jquery-doc-ensure-api-xml update)))

(defun jquery-doc-generate-data (file)
  "Extract data from FILE and write it to `jquery-doc-data.el'.

This function takes long time(it makes many calls to lynx) to finish"
  (let* ((api (car (xml-parse-file file)))
	 (entries (jquery-doc-entries api)))
    (with-temp-file (expand-file-name "jquery-doc-data.el"
                                      (file-name-directory
                                       (find-library-name "jquery-doc")))
      (insert
       (with-output-to-string
	 (print `(defvar jquery-doc-hash))
	 (print `(defvar jquery-doc-methods))
	 (print `(setq jquery-doc-hash
		       (make-hash-table :size 500 :test 'equal)))
	 (print `(setq jquery-doc-methods '()))
	 (dolist (entry entries)
	   (print `(push ,(jquery-doc-method-name entry) jquery-doc-methods))
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
(defun jquery-doc-insert-with-mode (mode text)
  "Apply the MODE to the TEXT and insert it in the current buffer."
  (let ((temp-buffer (generate-new-buffer "*temp*")))
    (with-current-buffer temp-buffer
      (insert "\n")
      (insert text)
      (ignore-errors
	(funcall mode))
      (indent-region (point-min) (point-max))
      (insert "\n\n")
      (font-lock-fontify-region (point-min) (point-max)))
    (insert-buffer-substring temp-buffer)
    (kill-buffer temp-buffer)))

(defun jquery-doc-insert-with-face (text face)
  "Insert TEXT in current buffer and color it with FACE."
  (let ((start (point)))
    (insert text)
    (set-text-properties start (point) `(face ,face))))

(defun jquery-doc-insert-with-fill-region (text)
  "Insert TEXT and justifies the text."
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
	('js (jquery-doc-insert-with-mode #'javascript-mode block-str))
	('css (jquery-doc-insert-with-mode #'css-mode block-str))
	('html (jquery-doc-insert-with-mode #'html-mode block-str))))))

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

      (help-mode)
      (goto-char (point-min))))))

(defun jquery-doc (&optional jquery-method)
  "Displays the jquery doc in a buffer.
Optional argument JQUERY-METHOD method-name."
  (interactive (list nil))
  (let* ((def (and (member (current-word) jquery-doc-methods)
		   (current-word)))
         (completing-read-func (if (null ido-mode)
				   'completing-read
				 'ido-completing-read))
	 (method-name (or jquery-method
			  (funcall completing-read-func
				   (if def
                                       (format "jQuery doc (default %s): " def)
                                     "jQuery doc: ")
				   (copy-list jquery-doc-methods)
				   nil
				   t nil nil def)))
	 (buffer-name (format "*jQuery doc %s" method-name)))
    (if (get-buffer buffer-name)
	(display-buffer buffer-name)
      (let ((buffer (get-buffer-create buffer-name)))
	(jquery-doc-insert buffer method-name)
	(display-buffer buffer)))))

(defconst jquery-doc-prefix-re
  "\\(\\$\\.\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\="
  "Regexp to match jQuery function call.")

;; auto-complete

(defun jquery-doc-documentation (method)
  "Return the documentation for METHOD as String."
  (let ((method (substring-no-properties method)))
    (jquery-doc-with-temp-buffer-as-string
      (jquery-doc-insert (current-buffer) method))))

;;;###autoload
(defun jquery-doc-ac-prefix ()
  (if (re-search-backward jquery-doc-prefix-re nil t)
      (match-beginning 1)
    (ac-prefix-default)))

;;;###autoload
(defvar ac-source-jquery
  '((candidates . jquery-doc-methods)
    (symbol . "f")
    (document . jquery-doc-documentation)
    (prefix . jquery-doc-ac-prefix)
    (cache)))

;; company-mode

(defvar company-jquery-modes '(js-mode js2-mode))

;;;###autoload
(defun company-jquery (command &optional arg &rest ignore)
  "`company-mode' completion back-end using `jquery-doc'."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-jquery))
    (prefix (when (memq major-mode company-jquery-modes)
              (or (company-grab-line jquery-doc-prefix-re 1)
                  (company-grab-symbol))))
    (candidates (all-completions arg jquery-doc-methods))
    (duplicates t)
    (doc-buffer (let* ((inhibit-read-only t)
                       (buf (company-doc-buffer)))
                  (jquery-doc-insert buf arg)
                  buf))))

;; common
;;;###autoload
(defun jquery-doc-setup ()
  (when (boundp 'ac-sources)
    (pushnew 'ac-source-jquery ac-sources))
  (when (boundp 'company-backends)
    (pushnew 'company-jquery company-backends)))

(provide 'jquery-doc)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; jquery-doc.el ends here
