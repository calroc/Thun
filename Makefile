# My make-fu style is old and tired.  I just want to have a few helper commands.

TESTDIR = ./test00
VERSION = 0.4.1
WEBSERVER = sforman@shell.osdn.net

.PHONY: clean sdist test docs upload-docs


clean:
	$(RM) -r Thun.egg-info/ dist/ build/ __pycache__/ $(TESTDIR)
	find . -name '*.pyc' | xargs $(RM)

sdist:
	python ./setup.py sdist bdist_wheel


# In order to support testing the code as installed
# create a virtualenv and install the source dist zip there.
test: sdist
	$(RM) -r $(TESTDIR)
	virtualenv --system-site-packages --never-download $(TESTDIR)
	. $(TESTDIR)/bin/activate && \
		pip install --no-cache-dir --no-index ./dist/Thun-$(VERSION).tar.gz
	echo "Type: source $(TESTDIR)/bin/activate"


docs:
	cd ./docs && make && make mov && cd ./sphinx_docs && make html

upload-docs: docs
	ssh $(WEBSERVER) /home/users/s/sf/sforman/backup-and-remove-htdocs
	rsync -rv --progress ./docs/sphinx_docs/_build/html/ $(WEBSERVER):/home/groups/j/jo/joypy/htdocs/
