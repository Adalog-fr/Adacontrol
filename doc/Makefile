# doc/Makefile
TEXI := $(wildcard *.texi)
HTML := $(subst .texi,.html,$(TEXI))
INFO := $(subst .texi,.info,$(TEXI))
PDF  := $(subst .texi,.pdf, $(TEXI))
TXT  := $(subst .texi,.txt, $(TEXI))
docs: $(HTML) $(INFO) $(PDF) $(TXT)
$(HTML): %.html: %.texi
	texi2any --output=$@ --html --no-split --css-include=adalog_doc.css $<
$(INFO): %.info: %.texi
	texi2any --output=$@ --info --no-split $<
$(PDF): %.pdf: %.texi
	texi2any --output=$@ --pdf --no-iftex --Xopt --quiet --Xopt --build=clean $<
$(TXT): %.txt: %.texi
	texi2any --output=$@ --plaintext $<
