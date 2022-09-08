# My make-fu style is old and tired.  I just want to have a few helper commands.

WEBSERVER = sforman@shell.osdn.net

.PHONY: clean docs upload-docs


clean:
	$(RM) -r Thun.egg-info/ dist/ build/ __pycache__/
	find . -name '*.pyc' | xargs $(RM)

docs:
	cd ./docs && make && make mov && cd ./sphinx_docs && make html

upload-docs: docs
	ssh $(WEBSERVER) /home/users/s/sf/sforman/backup-and-remove-htdocs
	rsync -rv --progress ./docs/sphinx_docs/_build/html/ $(WEBSERVER):/home/groups/j/jo/joypy/htdocs/
