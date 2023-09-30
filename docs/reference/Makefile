TITLE = --metadata title="Thun Function Reference"
TOC = --toc --toc-depth=2
CSS = --css=/css/fonts.css --css=/css/func_ref.css

all: mov

FuncRef.html: Function-Reference.md
	pandoc -s $(TOC) $(TITLE) $(CSS) --ascii Function-Reference.md -o FuncRef.html

mov: FuncRef.html
	cp -v $? ../html/

# pandoc --toc --toc-depth=2 --metadata title="Thun Function Reference" --ascii Function-Reference.md
