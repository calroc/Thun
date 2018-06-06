#docs      := $(wildcard *.ipynb)
#docs_html := $(patsubst %.ipynb,%.html,$(docs))

.PHONY: poop nospaces yesspaces

# https://stackoverflow.com/a/45531875
# Make cannot handle spaces in filenames, so temporarily rename them
nospaces:
	rename -v 's/ /%20/g' *\ *

# After Make is done, rename files back to having spaces
yesspaces:
	rename -v 's/%20/ /g' *%20*


poop: *.ipynb
	make "$(patsubst %.ipynb,%.html,$<)"

%.html : %.ipynb
	python -m nbconvert --to html $<

%.md : %.ipynb
	python -m nbconvert --to markdown $<

%.rst : %.ipynb
	python -m nbconvert --to rst $<

