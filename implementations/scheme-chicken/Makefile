
joy: joy.scm defs.scm
	csc -prologue defs.scm joy.scm

generate_defs: generate_defs.scm
	csc generate_defs.scm

defs.scm: ../defs.txt generate_defs
	./generate_defs

clean:
	rm generate_defs joy defs.scm

