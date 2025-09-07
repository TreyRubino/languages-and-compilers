# Trey Rubino
# CPSC 425 Compiler Design 1
# Dr. Schwesinger
# Compile instructions

OCAMLYACC = ocamlyacc
OCAMLC    = ocamlc
BUILD     = build

PARSER_MLY = main.mly
# ocamlyacc -b lets us set the basename; we point it inside build/
YACC_BASE  = $(BUILD)/main
GEN_ML     = $(YACC_BASE).ml
GEN_MLI    = $(YACC_BASE).mli
GEN_OUT    = $(YACC_BASE).output

EXECUTABLE = $(BUILD)/parser
SRC        = two_class_test.cl-lex

all: $(BUILD) $(EXECUTABLE)

$(BUILD):
	mkdir -p $(BUILD)

# Generate build/main.ml (+ .mli and .output) from main.mly
$(GEN_ML): $(PARSER_MLY) | $(BUILD)
	$(OCAMLYACC) -v -b $(YACC_BASE) $(PARSER_MLY)
	@rm -f $(GEN_MLI)  # you don't use it

# Compile to build/main.cmo (+ its .cmi into build/)
$(BUILD)/main.cmo: $(GEN_ML) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $(BUILD)/main.cmo $(GEN_ML)
	@([ -f main.cmi ] && mv -f main.cmi $(BUILD)/) || true

# Link final executable into build/
$(EXECUTABLE): $(BUILD)/main.cmo | $(BUILD)
	$(OCAMLC) -I $(BUILD) -o $(EXECUTABLE) $(BUILD)/main.cmo

run:
	$(MAKE) clean && $(MAKE) && ./$(EXECUTABLE) $(SRC)

clean:
	rm -rf $(BUILD)

.PHONY: all run clean
