#docs      := $(wildcard *.ipynb)
#docs_html := $(patsubst %.ipynb,%.html,$(docs))

.PHONY: poop


poop: *.ipynb
	make "$(patsubst %.ipynb,%.html,$<)"

%.html : %.ipynb
	python -m nbconvert --to html $<

%.md : %.ipynb
	python -m nbconvert --to markdown $<

%.rst : %.ipynb
	python -m nbconvert --to rst $<

