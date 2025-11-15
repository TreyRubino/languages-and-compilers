# Trey Rubino
# CPSC 372 Independent Study
# Dr. Schwesinger
# Lexer build

OCAMLLEX = ocamllex
OCAMLC   = ocamlc
BUILD = ../../build

MAIN_MLL = lexer.mll
GEN_ML   = $(BUILD)/lexer.ml

EXECUTABLE = $(BUILD)/lexer

all: $(BUILD) $(EXECUTABLE)

$(BUILD):
	mkdir -p $(BUILD)

$(GEN_ML): $(MAIN_MLL) | $(BUILD)
	$(OCAMLLEX) -o $(GEN_ML) $(MAIN_MLL)

$(BUILD)/lexer.cmo: $(GEN_ML) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $(BUILD)/lexer.cmo $(GEN_ML)
	@([ -f lexer.cmi ] && mv -f lexer.cmi $(BUILD)/) || true

$(EXECUTABLE): $(BUILD)/lexer.cmo
	$(OCAMLC) -I $(BUILD) -o $(EXECUTABLE) $(BUILD)/lexer.cmo

clean:
	rm -f $(BUILD)/lexer.ml
	rm -f $(BUILD)/lexer.cmo
	rm -f $(BUILD)/lexer.cmi
	rm -f $(EXECUTABLE)

.PHONY: all clean
