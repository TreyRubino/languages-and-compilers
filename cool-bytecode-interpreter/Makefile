# Trey Rubino
# CPSC 372 Independent Study
# Dr. Schwesinger
# Lexer build

OCAMLC = ocamlc
BUILD  = build

CORE    = core

LEXER   = frontend/lexer
PARSER  = frontend/parser
CHECKER = frontend/checker

CODEGEN = backend/codegen

CORE_SRC = $(CORE)/error.ml $(CORE)/ast.ml $(CORE)/semantics.ml
CORE_CMO = $(BUILD)/error.cmo $(BUILD)/ast.cmo $(BUILD)/semantics.cmo 

MAIN = main.ml
MAIN_CMO = $(BUILD)/main.cmo

EXEC = cooli

OBJS = \
	$(BUILD)/error.cmo \
  $(BUILD)/ast.cmo \
	$(BUILD)/semantics.cmo \
  $(BUILD)/lexer.cmo \
  $(BUILD)/parser.cmo \
  $(BUILD)/env.cmo \
  $(BUILD)/validate.cmo \
  $(BUILD)/typecheck.cmo \
  $(BUILD)/checker.cmo \
	$(BUILD)/codegen.cmo \
  $(BUILD)/main.cmo

all: $(BUILD) core modules $(EXEC)

$(BUILD):
	mkdir -p $(BUILD)

core: $(CORE_CMO)

$(BUILD)/%.cmo: $(CORE)/%.ml | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $@ $<
	@([ -f $(basename $<).cmi ] && mv -f $(basename $<).cmi $(BUILD)/) || true

$(MAIN_CMO): $(MAIN) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $(MAIN_CMO) $(MAIN)
	@([ -f main.cmi ] && mv -f main.cmi $(BUILD)/) || true

modules:
	make -C $(PARSER)
	make -C $(LEXER)
	make -C $(CHECKER)
	make -C $(CODEGEN)

$(EXEC): $(OBJS)
	$(OCAMLC) -I $(BUILD) -o $(EXEC) $(OBJS)

clean:
	rm -rf $(BUILD)
	rm -f $(EXEC)

.PHONY: all core modules clean
