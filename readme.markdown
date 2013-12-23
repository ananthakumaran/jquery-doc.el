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

Updating API data
=================

If you'd like to update the API data to the latest version available
you can use the `jquery-doc-fetch-and-generate-data` command.

Warning: it may take a few seconds to parse all the data.
