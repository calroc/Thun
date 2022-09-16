FUNCDOCS = $(wildcard ../*.md)
TITLE = --metadata title="Function Reference"
TOC = --toc --toc-depth=2

all: mov

FuncRef.html: Function-Reference.md
	pandoc -s $(TOC) $(TITLE) --ascii Function-Reference.md -o FuncRef.html

Function-Reference.md: HEADER.md $(FUNCDOCS)
	awk 'FNR==1 && NR > 1 {print ""}1' HEADER.md $(FUNCDOCS) > Function-Reference.md

# https://stackoverflow.com/questions/8183191/concatenating-files-and-insert-new-line-in-between-files
# WTF would we do w/o the Internet?  Know our tools well?

mov: FuncRef.html
	cp -v $? ../../html/
