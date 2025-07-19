# Trey Rubino
# CPSC 425 Compiler Design 1
# Dr. Schwesinger
# Compile instructions

OCAMLYACC = ocamlyacc
OCAMLC = ocamlc

BUILD_DIR = ../build
PARSER_MLY = main.mly
PARSER_ML  = $(BUILD_DIR)/main.ml
EXECUTABLE = $(BUILD_DIR)/parser
SRC = two_class_test.cl-lex

all: $(EXECUTABLE)

# Ensure build directory exists
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Generate main.ml and main.mli from main.mly
$(PARSER_ML): $(PARSER_MLY) | $(BUILD_DIR)
	$(OCAMLYACC) -v -b $(BUILD_DIR)/main $(PARSER_MLY)
	rm -f $(BUILD_DIR)/main.mli

# Compile main.ml to main.cmo
$(BUILD_DIR)/main.cmo: $(PARSER_ML)
	$(OCAMLC) -c -o $(BUILD_DIR)/main.cmo $(BUILD_DIR)/main.ml

# Link to create the executable
$(EXECUTABLE): $(BUILD_DIR)/main.cmo
	$(OCAMLC) -o $(EXECUTABLE) $(BUILD_DIR)/main.cmo

run:
	make clean && make && ./$(EXECUTABLE) $(SRC)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: all run clean
