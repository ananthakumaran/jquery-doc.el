(require 'jquery-doc)

(ert-deftest test-method ()
  (dolist (method jquery-doc-methods)
    (message method)
    (should-not (string-equal (jquery-doc-documentation method) ""))))


