# Makefile for gitpal CLI tool

# Version detection
# Check if current HEAD is a git tag, otherwise use 'development'
GIT_TAG := $(shell git describe --exact-match --tags HEAD 2>/dev/null)
VERSION := $(if $(GIT_TAG),$(GIT_TAG),development)

# Compiler and flags
FPC = fpc
FPCFLAGS = -O2

# OpenSSL 1.1 support (required for HTTPS)
OPENSSL_PATH = /opt/homebrew/opt/openssl@1.1
OPENSSL_FLAGS = -Fl$(OPENSSL_PATH)/lib -k-rpath -k$(OPENSSL_PATH)/lib -k-lssl -k-lcrypto

# Directories
SRC_DIR = src
BIN_DIR = bin
VENDOR_DIR = vendor

# Unit search paths for vendor dependencies
UNIT_PATHS = -Fu$(VENDOR_DIR)/bobaui/v0.1.12 -Fu$(VENDOR_DIR)/models.pas

# Source files
MAIN_SRC = $(SRC_DIR)/app.pas

# Output binary
TARGET = $(BIN_DIR)/gitpal

# Default target
all: $(TARGET)

# Create bin directory
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Generate version file in /tmp
/tmp/gitpal-version.inc: 
	@echo "const AppVersion = '$(VERSION)';" > /tmp/gitpal-version.inc

# Build the main application
$(TARGET): $(MAIN_SRC) /tmp/gitpal-version.inc | $(BIN_DIR)
	$(FPC) $(FPCFLAGS) -Fi/tmp $(UNIT_PATHS) $(OPENSSL_FLAGS) -o$(TARGET) $(MAIN_SRC)

# Clean build artifacts
clean:
	rm -rf $(BIN_DIR)
	rm -f /tmp/gitpal-version.inc
	find . -name "*.o" -delete
	find . -name "*.ppu" -delete

# Run the application
run: $(TARGET)
	$(TARGET)

# Show version that will be embedded
version:
	@echo "Detected version: $(VERSION)"

# Help target
help:
	@echo "Available targets:"
	@echo "  all     - Build the application (default)"
	@echo "  clean   - Clean build artifacts"
	@echo "  run     - Build and run the application"
	@echo "  version - Show the version that will be embedded"
	@echo "  help    - Show this help"

.PHONY: all clean run version help