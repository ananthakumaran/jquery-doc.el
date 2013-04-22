Features
========

* provides completion source for
  [auto-complete](http://auto-complete.org) and [company-mode](http://company-mode.github.io/)
* provides a command `jquery-doc` to lookup the documentation for a
  method in jquery

Installation
============

* setup auto-complete or company-mode
* add jquery-doc.el and jquery-doc-data.el to load-path

```cl
(require 'jquery-doc)
(add-hook 'js2-mode-hook 'jquery-doc-setup)
```
