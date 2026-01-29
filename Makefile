CC      := gcc
CFLAGS  := -Wall -Wextra -std=c11 -pedantic

TARGET  := build/iris
SRC     := examples/main.c
BUILDDIR := build

all: $(TARGET)

$(TARGET): $(SRC) | $(BUILDDIR)
	$(CC) $(CFLAGS) $(SRC) -o $(TARGET)

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

clean:
	rm -f $(TARGET)

run: $(TARGET)
	./$(TARGET)
