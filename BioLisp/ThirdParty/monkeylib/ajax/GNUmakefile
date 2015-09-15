fml := $(wildcard *.fml)
ls  := $(wildcard *.ls)

generated_html := $(wildcard $(fml:.fml=.html))
generated_js := $(wildcard $(ls:.ls=.js))

all:
	@echo $(generated_html) $(generated_js)

clean: clean_html clean_js

clean_html:
	rm -f $(generated_html)

clean_js:
	rm -f $(generated_js)

tidy: clean
	rm -f *~
	rm -f *.fasl
