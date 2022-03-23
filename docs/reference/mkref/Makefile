FUNCDOCS = $(wildcard ../*.md)
TITLE = --metadata title="Functor Reference"
TOC = --toc --toc-depth=2


all: Functor-Reference.md
	pandoc -s $(TOC) $(TITLE) --ascii Functor-Reference.md -o FuncRef.html


Functor-Reference.md: HEADER.md $(FUNCDOCS)
	awk 'FNR==1 && NR > 1 {print ""}1' HEADER.md $(FUNCDOCS) > Functor-Reference.md

# https://stackoverflow.com/questions/8183191/concatenating-files-and-insert-new-line-in-between-files
# WTF would we do w/o the Internet?  Know our tools well?
