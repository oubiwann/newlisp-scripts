SCRIPT_DIR = ./scripts
BUILD_DIR ?= ./_build
BIN_DIR = ./bin
INSTALL_DIR ?= /usr/local/bin

include resources/make/build.mk
include resources/make/test.mk

clean:
	@echo
	@echo "Remoging build directory and compiled files ..."
	@rm -rf _build bin/*
