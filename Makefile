# My make-fu style is old and tired.  I just want to have a few helper commands.

TESTDIR = ./test00
VERSION = 0.1.0

.PHONY: clean sdist test docs


clean:
	$(RM) -r Thun.egg-info/ dist/ build/ __pycache__/ $(TESTDIR)
	find . -name '*.pyc' | xargs $(RM)

sdist:
	python ./setup.py sdist

# In order to support testing the code as installed
# create a virtualenv and install the source dist zip there.
test: sdist
	$(RM) -r $(TESTDIR)
	virtualenv --system-site-packages --never-download $(TESTDIR)
	. $(TESTDIR)/bin/activate && \
		pip install --no-cache-dir --no-index ./dist/Thun-$(VERSION).tar.gz
	echo "Type: source $(TESTDIR)/bin/activate"


docs:
	cd ./docs && python -m nbconvert --to html *.ipynb
	cd ./docs && python -m nbconvert --to markdown *.ipynb
