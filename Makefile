# Makefile for gitpal CLI tool

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
UNIT_PATHS = -Fu$(VENDOR_DIR)/bobaui -Fu$(VENDOR_DIR)/models.pas

# Source files
MAIN_SRC = $(SRC_DIR)/app.pas

# Output binary
TARGET = $(BIN_DIR)/gitpal

# Default target
all: $(TARGET)

# Create bin directory
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Build the main application
$(TARGET): $(MAIN_SRC) | $(BIN_DIR)
	$(FPC) $(FPCFLAGS) $(UNIT_PATHS) $(OPENSSL_FLAGS) -o$(TARGET) $(MAIN_SRC)

# Clean build artifacts
clean:
	rm -rf $(BIN_DIR)
	find . -name "*.o" -delete
	find . -name "*.ppu" -delete

# Run the application
run: $(TARGET)
	$(TARGET)

# Help target
help:
	@echo "Available targets:"
	@echo "  all     - Build the application (default)"
	@echo "  clean   - Clean build artifacts"
	@echo "  run     - Build and run the application"
	@echo "  help    - Show this help"

.PHONY: all clean run help