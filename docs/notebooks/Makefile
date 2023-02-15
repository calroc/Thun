# Docs notebook makefile.

docs      = $(wildcard *.ipynb)
docs_html = $(patsubst %.ipynb,%.html,$(docs))

.PHONY: clean

all: mov

clean:
	$(RM) -v $(docs_html) index.html

index.html: build_index.py $(docs_html)
	python build_index.py > index.html

$(docs_html): %.html : %.ipynb
	jupyter nbconvert --to html --template basic $<

mov: $(docs_html) index.html
	mv -v $? ../html/notebooks/


# markdown Functor-Reference.md | tidy5 -utf8 --add-meta-charset yes > ../../html/FR.html
# --HTMLExporter.embed_images=True