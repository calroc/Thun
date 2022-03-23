rsts = $(wildcard *.rst)
mds  = $(patsubst %.rst,%.md,$(rsts))

# mv -vf *.md ../reference/


all: $(mds)

clean:
	$(RM) -v $(mds)

$(mds): %.md : %.rst
	pandoc $< -o $@
