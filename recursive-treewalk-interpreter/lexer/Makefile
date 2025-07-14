# Trey Rubino
# CPSC 425 Compiler Design 1
# Dr. Schwesinger
# Compile instructions

OCAMLLEX = ocamllex
OCAMLC = ocamlc

MAIN_MLL = main.mll
MAIN_ML = main.ml

EXECUTABLE = lexer
SRC = two_class_test.cl

all: $(EXECUTABLE)

$(MAIN_ML): $(MAIN_MLL)
	$(OCAMLLEX) $(MAIN_MLL)

main.cmo: $(MAIN_ML)
	$(OCAMLC) -c $(MAIN_ML)

$(EXECUTABLE): main.cmo
	$(OCAMLC) -o $(EXECUTABLE) main.cmo

run:
	make clean && make && ./$(EXECUTABLE) $(SRC)

clean:
	rm -f $(EXECUTABLE) main.ml *.cmi *.cmo *.o *.cl-lex

.PHONY: all run clean
