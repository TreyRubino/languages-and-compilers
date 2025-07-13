# Trey Rubino
# CPSC 425 Compiler Design 1
# Dr. Schwesinger
# Compile instructions

OCAMLYACC = ocamlyacc
OCAMLC = ocamlc

PARSER_MLY = main.mly
PARSER_ML = main.ml

EXECUTABLE = parser
SRC = two_class_test.cl-lex

all: $(EXECUTABLE)

$(PARSER_ML): $(PARSER_MLY)
	$(OCAMLYACC) -v $(PARSER_MLY)
	rm -f main.mli

main.cmo: $(PARSER_ML)
	$(OCAMLC) -c $(PARSER_ML)

$(EXECUTABLE): main.cmo
	$(OCAMLC) -o $(EXECUTABLE) main.cmo

run:
	make clean && make && ./$(EXECUTABLE) $(SRC)

clean:
	rm -f $(EXECUTABLE) main.ml main.mli *.cmi *.cmo *.o *.cl-ast *.output

.PHONY: all run clean
