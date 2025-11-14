# Trey Rubino
# CPSC 372 Independent study
# Dr. Schwesinger

OCAMLC = ocamlc
BUILD  = ../../build

EXECUTABLE = $(BUILD)/checker

ML  = env.ml typecheck.ml validate.ml checker.ml
CMO = $(addprefix $(BUILD)/,$(ML:.ml=.cmo))

all: $(BUILD) $(EXECUTABLE)

$(BUILD):
	mkdir -p $(BUILD)

$(BUILD)/%.cmo: %.ml | $(BUILD)
	$(OCAMLC) -I $(BUILD) -c -o $@ $<
	@([ -f $*.cmi ] && mv -f $*.cmi $(BUILD)/) || true

$(EXECUTABLE): $(CMO) | $(BUILD)
	$(OCAMLC) -I $(BUILD) -o $@ $(CMO)

clean:
	rm -rf $(BUILD)

.PHONY: all clean
