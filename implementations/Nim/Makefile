SOURCES = defs.nim joy.nim joylib.nim printer.nim reader.nim types.nim utils.nim

joy: $(SOURCES) defs.txt
	nim c joy.nim

defs.txt: ../defs.txt
	cp -fv ../defs.txt defs.txt

