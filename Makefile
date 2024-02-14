CC=ghc
TARGET=src/Main
FLAGS=-Wall
all: Main.hs
	$(CC) $(FLAGS) $(PROJ).hs -o $(TARGET)

clean:
	rm -f $(TARGET) $(OBJECTS)