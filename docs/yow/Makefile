rsts = $(wildcard *.rst)
mds  = $(patsubst %.rst,%.md,$(rsts))


all: $(mds)

clean:
	$(RM) -v $(mds)

$(mds): %.md : %.rst
	#pandoc $< -o $@
	sed -e 's/^#/##/' $@ > ../reference/$@
 
# 	cp -vf $@ ../reference/
