BATCH=emacs --batch -q -no-site-file \
	--eval="(add-to-list 'load-path \"./../auto-complete\")" \
	--eval="(add-to-list 'load-path \".\")"

all: compile

compile:
	 $(BATCH) -f batch-byte-compile jquery-doc-data.el
	 $(BATCH) -f batch-byte-compile jquery-doc.el

update-doc-data: compile
	$(BATCH) --eval="(require 'jquery-doc)" \
	--eval='(jquery-doc-generate-data "jquery.api.xml")'

update-api:
	curl http://api.jquery.com/resources/api.xml > jquery.api.xml
	dos2unix jquery.api.xml

test: clean compile
	$(BATCH) -l ert -l jquery-doc-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
