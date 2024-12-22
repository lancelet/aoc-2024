# ADVENT OF CODE 2014
# C Makefile
#
# Any file named <something>_main.c will be treated as a main entry point,
# and the executable created will be named <something>.

# Compiler and flags
CC := clang
CFLAGS := -Wall -Wextra -std=c17 -O2
LDFLAGS :=

# Directories
SRC_DIR := src
BUILD_DIR := build
BIN_DIR := bin

# Create directories if they don't exist
$(shell mkdir -p $(BUILD_DIR) $(BIN_DIR))

# Find all .c files in src and determine executables
SOURCES := $(wildcard $(SRC_DIR)/*.c)
OBJECTS := $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SOURCES))
MAINS := $(filter %_main.c,$(SOURCES))
EXECUTABLES := $(patsubst $(SRC_DIR)/%_main.c,$(BIN_DIR)/%,$(MAINS))

# Default target: build all the executables.
all: $(EXECUTABLES)

# Rule to build each executable
$(BIN_DIR)/%: $(BUILD_DIR)/%_main.o $(OBJECTS)
	$(CC) $(CFLAGS) $^ -o $@

# Rule to build object files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR) $(BIN_DIR)

# Debugging builds
debug: CFLAGS += -g -DDEBUG
debug: clean all

# Phony targets
.PHONY: all clean debug
