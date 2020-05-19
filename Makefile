# My make-fu style is old and tired.  I just want to have a few helper commands.

TESTDIR = ./test00
VERSION = 0.1.0
WEBSERVER = sforman@shell.osdn.net

.PHONY: clean sdist test


clean:
	$(RM) -r Xerblin.egg-info/ dist/ build/ __pycache__/ $(TESTDIR)
	find . -name '*.pyc' | xargs $(RM)

sdist:
	python ./setup.py sdist bdist_wheel


# In order to support testing the code as installed
# create a virtualenv and install the source dist zip there.
test: sdist
	$(RM) -r $(TESTDIR)
	virtualenv --system-site-packages --never-download $(TESTDIR)
	. $(TESTDIR)/bin/activate && \
		pip install setuptools && \
		pip install --no-cache-dir --no-index ./dist/Xerblin-$(VERSION).tar.gz
	echo "Type: source $(TESTDIR)/bin/activate"
