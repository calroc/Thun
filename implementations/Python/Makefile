SHELL != which python
.ONESHELL:

joy: joy.py ../defs.txt
	from pathlib import Path
	SPLIT_PATTERN = "'''.splitlines()\n"
	joy_source = Path('joy.py').read_text()
	defs_source = Path('../defs.txt').read_text()
	head, sep, tail = joy_source.partition(SPLIT_PATTERN)
	joy = Path('joy')
	joy.write_text(''.join((head, defs_source, sep, tail)))
	joy.chmod(0o775)

