.PHONY: all build format edit demo clean

graph?=super1.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/ftest.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	@mkdir -p outGraph
	./ftest.exe graphs/${graph} outGraph
#	@echo "\n   🥁  RESULT  🥁\n"
#	@cat outfile
	for f in outGraph/*.txt ; do dot -Tsvg $$f > $${f%.txt}.svg ; done

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean
