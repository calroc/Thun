docs      = $(wildcard *.ipynb)
docs_html = $(patsubst %.ipynb,%.html,$(docs))

.PHONY: clean sdist test docs


all: $(docs_html)


clean:
	$(RM) -v $(docs_html)


$(docs_html): %.html : %.ipynb
	python -m nbconvert --to html $<



mov: $(move_us)
	cp -v $? ./sphinx_docs/notebooks/

