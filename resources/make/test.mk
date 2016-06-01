.PHONY: check

check: compile
	INSTALL_DIR=/tmp $(MAKE) install
	$(MAKE) check-scripts
	$(MAKE) check-bins

check-scripts:
	./scripts/wifi.lsp -h
	./scripts/bat.lsp -h
	./scripts/backlight.lsp -h

check-bins:
	PATH=$PATH:/tmp wifi -h
	PATH=$PATH:/tmp bat -h
	PATH=$PATH:/tmp backlight -h


