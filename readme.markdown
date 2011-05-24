Features
========

* provides a completion source for
  [autocomplete](http://www.emacswiki.org/emacs/AutoComplete)
* provides a function(jquery-doc) to lookup the documentation for a
  method in jquery

Installation
============

* add jquery-doc.el and jquery-doc-data.el to load-path
* enable autocomplete

```cl
(require 'jquery-doc)

;; adds ac-source-jquery to the ac-sources list
(add-hook 'js2-mode-hook 'jquery-doc-setup)
```
