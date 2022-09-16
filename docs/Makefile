README=../README.md
BUILD_SCRIPT=build_index.py
GENERATOR=python $(BUILD_SCRIPT)
HTML_OUTPUT_DIR=./html
NOTEBOOK_OUTPUT_DIR=$(HTML_OUTPUT_DIR)/notebooks


all: $(HTML_OUTPUT_DIR)/index.html

$(HTML_OUTPUT_DIR)/index.html: $(README) $(BUILD_SCRIPT)
	$(GENERATOR) $(README) > $(HTML_OUTPUT_DIR)/index.html


#	python -m markdown -f index.html -o html $(README)

