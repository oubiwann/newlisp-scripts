SCRIPTS = $(notdir $(wildcard $(SCRIPT_DIR)/*.lsp))
CONCATS = $(SCRIPTS:.lsp=.lsp)

# These won't be defined until `make concat` is called
BUILTS = $(notdir $(wildcard $(BUILD_DIR)/*.lsp))
BINS = $(BUILTS:.lsp=)

# These won't be defined until `make concat` and `make standalone` are called
COMPILED = $(notdir $(wildcard $(BIN_DIR)/*))
INSTALLEDS = $(COMPILED:=)

.SUFFIXES: .lsp
.PHONY: compile concat install

compile: compile-msg
	@$(MAKE) -s $(MFLAGS) concat
	@$(MAKE) -s $(MFLAGS) standalone

concat: concat-msg $(addprefix $(BUILD_DIR)/, $(CONCATS))

$(BUILD_DIR)/%.lsp: $(SCRIPT_DIR)/%.lsp
	@mkdir -p $(BUILD_DIR)
	@head -1 $< > $@
	@cat include/* >> $@
	@cat src/* >> $@
	@tail -n +2 $< | grep -v '(load "' >> $@

standalone: standalone-msg $(addprefix $(BIN_DIR)/, $(BINS))
	chmod 755 $(BIN_DIR)/*

$(BIN_DIR)/%: $(BUILD_DIR)/%.lsp
	mkdir -p $(BIN_DIR)
	newlisp -x $< $@

install: install-msg $(addprefix $(INSTALL_DIR)/, $(INSTALLEDS))

$(INSTALL_DIR)/%: $(BIN_DIR)/%
	@cp $< $@

compile-msg:
	@echo
	@echo "Compileing scripts to executables ..."

concat-msg:
	@echo "\tConcatenating include files ..."

standalone-msg:
	@echo "\tLinking files to newlisp ..."

install-msg:
	@echo
	@echo "Installing compiled executables ..."
