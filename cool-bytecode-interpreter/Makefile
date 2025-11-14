# Trey Rubino
# Top-level build

OCAMLC = ocamlc
BUILD  = build

CORE    = core

LEXER   = frontend/lexer
PARSER  = frontend/parser
CHECKER = frontend/checker

CODEGEN = backend/codegen
VM = backend/vm

CORE_SRC = $(CORE)/tokens.ml $(CORE)/ast.ml
CORE_CMO = $(BUILD)/tokens.cmo $(BUILD)/ast.cmo

all: $(BUILD) core modules

$(BUILD):
	mkdir -p $(BUILD)

core: $(CORE_CMO)

$(BUILD)/%.cmo: $(CORE)/%.ml | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $@ $<
	@([ -f $(basename $<).cmi ] && mv -f $(basename $<).cmi $(BUILD)/) || true

modules:
	make -C $(LEXER)
	make -C $(PARSER)
	make -C $(CHECKER)

clean:
	rm -rf $(BUILD)

.PHONY: all core modules clean
