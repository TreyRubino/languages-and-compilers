# Trey Rubino
# CPSC 425 Compiler Design 1
# Dr. Schwesinger
# Compile instructions

OCAMLLEX = ocamllex
OCAMLC   = ocamlc
BUILD    = build

MAIN_MLL = main.mll
GEN_ML   = $(BUILD)/main.ml

EXECUTABLE = $(BUILD)/lexer
SRC = two_class_test.cl

all: $(BUILD) $(EXECUTABLE)

$(BUILD):
	mkdir -p $(BUILD)

# Generate build/main.ml from main.mll
$(GEN_ML): $(MAIN_MLL) | $(BUILD)
	$(OCAMLLEX) -o $(GEN_ML) $(MAIN_MLL)

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
	rm -f *.cl-lex

.PHONY: all run clean
