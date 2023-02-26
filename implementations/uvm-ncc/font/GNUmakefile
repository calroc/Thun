
FONT=Inconsolata
POINTSIZE=14
FILL=white

TXTS=00.txt 01.txt 02.txt 03.txt 04.txt 05.txt 06.txt 07.txt 08.txt 09.txt 10.txt 11.txt 12.txt 13.txt 14.txt 15.txt 16.txt 17.txt 18.txt 19.txt 20.txt 21.txt 22.txt 23.txt 24.txt 25.txt 26.txt 27.txt 28.txt 29.txt 30.txt 31.txt 32.txt 33.txt 34.txt 35.txt 36.txt 37.txt 38.txt 39.txt 40.txt 41.txt 42.txt 43.txt 44.txt 45.txt 46.txt 47.txt 48.txt 49.txt 50.txt 51.txt 52.txt 53.txt 54.txt 55.txt 56.txt 57.txt 58.txt 59.txt 60.txt 61.txt 62.txt 63.txt 64.txt 65.txt 66.txt 67.txt 68.txt 69.txt 70.txt 71.txt 72.txt 73.txt 74.txt 75.txt 76.txt 77.txt 78.txt 79.txt 80.txt 81.txt 82.txt 83.txt 84.txt 85.txt 86.txt 87.txt 88.txt 89.txt 90.txt 91.txt 92.txt 93.txt
BMPS=00.bmp 01.bmp 02.bmp 03.bmp 04.bmp 05.bmp 06.bmp 07.bmp 08.bmp 09.bmp 10.bmp 11.bmp 12.bmp 13.bmp 14.bmp 15.bmp 16.bmp 17.bmp 18.bmp 19.bmp 20.bmp 21.bmp 22.bmp 23.bmp 24.bmp 25.bmp 26.bmp 27.bmp 28.bmp 29.bmp 30.bmp 31.bmp 32.bmp 33.bmp 34.bmp 35.bmp 36.bmp 37.bmp 38.bmp 39.bmp 40.bmp 41.bmp 42.bmp 43.bmp 44.bmp 45.bmp 46.bmp 47.bmp 48.bmp 49.bmp 50.bmp 51.bmp 52.bmp 53.bmp 54.bmp 55.bmp 56.bmp 57.bmp 58.bmp 59.bmp 60.bmp 61.bmp 62.bmp 63.bmp 64.bmp 65.bmp 66.bmp 67.bmp 68.bmp 69.bmp 70.bmp 71.bmp 72.bmp 73.bmp 74.bmp 75.bmp 76.bmp 77.bmp 78.bmp 79.bmp 80.bmp 81.bmp 82.bmp 83.bmp 84.bmp 85.bmp 86.bmp 87.bmp 88.bmp 89.bmp 90.bmp 91.bmp 92.bmp 93.bmp

all: font.h

%.bmp: %.txt
	convert \
	-background transparent \
	-depth 8 \
	-fill $(FILL) \
	-font $(FONT) \
	-pointsize $(POINTSIZE) \
	label:@$< \
	$@


font.h: $(BMPS) convert.py GNUmakefile
	python ./convert.py $(FONT) $(POINTSIZE) > font.h

$(TXTS): make_txt_files.py GNUmakefile
	python ./make_txt_files.py

clean:
	$(RM) $(TXTS)
	$(RM) $(BMPS)

