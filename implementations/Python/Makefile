
joy: joy.py ../defs.txt
	split -p \'\'\'\.splitlines\(\) joy.py joy.py.
	cat joy.py.aa ../defs.txt joy.py.ab > joy
	chmod u+x joy

clean:
	$(RM) -fv joy joy.py.*
