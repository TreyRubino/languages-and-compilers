# Trey Rubino
# CPSC 425 Compiler Design 1
# Dr. Schwesinger
# Compile instructions

OCAMLYACC = ocamlyacc
OCAMLC    = ocamlc
BUILD     = ../../build

PARSER_MLY = parser.mly
YACC_BASE  = $(BUILD)/parser
GEN_ML     = $(YACC_BASE).ml
GEN_MLI    = $(YACC_BASE).mli

EXECUTABLE = $(BUILD)/parser

all: $(BUILD) $(EXECUTABLE)

$(BUILD):
	mkdir -p $(BUILD)

$(GEN_ML): $(PARSER_MLY) | $(BUILD)
	$(OCAMLYACC) -b $(YACC_BASE) $(PARSER_MLY)
	@rm -f $(GEN_MLI) 

$(BUILD)/parser.cmo: $(GEN_ML) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $(BUILD)/parser.cmo $(GEN_ML)
	@([ -f parser.cmi ] && mv -f parser.cmi $(BUILD)/) || true

$(EXECUTABLE): $(BUILD)/parser.cmo | $(BUILD)
	$(OCAMLC) -I $(BUILD) -o $(EXECUTABLE) $(BUILD)/parser.cmo

clean:
	rm -rf $(BUILD)

.PHONY: all clean
