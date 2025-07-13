# Trey Rubino
# CPSC 425 Compiler Design 1 
# Dr. Schwesinger 
# Compile instructions

OCAMLC = ocamlc

CHECKER_ML = main.ml
CHECKER_CMO = main.cmo

EXECUTABLE = checker
SRC = test.cl-ast

all: $(EXECUTABLE)

$(EXECUTABLE): $(CHECKER_CMO)
	$(OCAMLC) -o $(EXECUTABLE) $(CHECKER_CMO)

$(CHECKER_CMO): $(CHECKER_ML)
	$(OCAMLC) -c $(CHECKER_ML)

run: 
	make clean && make && ./$(EXECUTABLE) $(SRC)

clean:
	rm -f $(EXECUTABLE) *.cmi *.cmo *.o *.cl-type a.out

.PHONY: all run clean
