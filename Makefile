BIN_DIR = ./bin
SCRIPT_DIR = ./scripts
SCRIPTS = $(notdir $(wildcard $(SCRIPT_DIR)/*.lsp))
BINS = $(SCRIPTS:.lsp=)
INSTALL_DIR ?= /usr/local/bin
COMPILED = $(notdir $(wildcard $(BIN_DIR)/*))
INSTALLEDS = $(COMPILED:=)

.SUFFIXES: .lsp
.PHONY: compile install

compile:
	$(MAKE) $(MFLAGS) standalone

standalone: $(addprefix $(BIN_DIR)/, $(BINS))
	chmod 755 $(BIN_DIR)/*

$(BIN_DIR)/%: $(SCRIPT_DIR)/%.lsp
	mkdir -p $(BIN_DIR)
	newlisp -x $< $@

install: $(addprefix $(INSTALL_DIR)/, $(INSTALLEDS))

$(INSTALL_DIR)/%: $(BIN_DIR)/%
	cp $< $@
