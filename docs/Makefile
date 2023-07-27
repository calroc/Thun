# Docs toplevel makefile.

INDEX = ./source/index.md
BUILD_SCRIPT = build_index.py
GENERATOR = python $(BUILD_SCRIPT)
HTML_OUTPUT_DIR = ./html
NOTEBOOK_OUTPUT_DIR = $(HTML_OUTPUT_DIR)/notebooks
INDEX_OUT = $(HTML_OUTPUT_DIR)/index.html

all: $(INDEX_OUT)

$(INDEX_OUT): $(INDEX) $(BUILD_SCRIPT)
	$(GENERATOR) $(INDEX) > $(INDEX_OUT)


#	python -m markdown -f index.html -o html $(INDEX)
