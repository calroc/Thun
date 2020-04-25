# My make-fu style is old and tired.  I just want to have a few helper commands.

TESTDIR = ./test00
VERSION = 0.2.0

.PHONY: clean sdist test docs


clean:
	$(RM) -r Thun.egg-info/ dist/ build/ __pycache__/ $(TESTDIR)
	find . -name '*.pyc' | xargs $(RM)

sdist:
	python ./setup.py sdist

joy/utils/generated_library.py: joy/utils/types.py
	python -c 'import joy.utils.types ; joy.utils.types.generate_library_code()' > $@


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

