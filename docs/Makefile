docs      = $(wildcard *.ipynb)
docs_html = $(patsubst %.ipynb,%.html,$(docs))
docs_md   = $(patsubst %.ipynb,%.md,$(docs))
docs_rst  = $(patsubst %.ipynb,%.rst,$(docs))

.PHONY: clean sdist test docs


all: $(docs_html) $(docs_md) $(docs_rst)


clean:
	$(RM) -v $(docs_html) $(docs_md) $(docs_rst)


$(docs_html): %.html : %.ipynb
	python -m nbconvert --to html $<

$(docs_md): %.md : %.ipynb
	python -m nbconvert --to markdown $<

$(docs_rst): %.rst : %.ipynb
	python -m nbconvert --to rst $<

