FUNCDOCS != ls [a-z]*.md


all: Functor-Reference.md
	pandoc -s --toc --toc-depth=2 --ascii Functor-Reference.md -o FuncRef.html


Functor-Reference.md: HEADER.md $(FUNCDOCS)
	cat HEADER.md $(FUNCDOCS) > Functor-Reference.md
