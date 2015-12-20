CC = gcc
CFLAGS = -g -Wall
LDFLAGS = -g -lm
LIBS=-lm

gram_test-NEW: gram_test-NEW.o turtle.o

gram_test-NEW.o: gram_test-NEW.c turtle.h

turtle: turtle.c turtle.h

.PHONY: clean

clean:
	rm -f *.o a.out test *.out *.bmp
