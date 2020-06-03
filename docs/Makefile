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


move_us = Advent_of_Code_2017_December_1st.rst Derivatives_of_Regular_Expressions.rst Generator_Programs.rst Newton-Raphson.rst Ordered_Binary_Trees.rst Quadratic.rst Recursion_Combinators.rst Replacing.rst Square_Spiral.rst The_Four_Operations.rst Treestep.rst TypeChecking.rst Types.rst Zipper.rst

mov: $(move_us)
	cp -v $? ./sphinx_docs/notebooks/

