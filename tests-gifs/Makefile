CC = gcc
CFLAGS = -g -Wall
LDFLAGS = -g -lm
LIBS=-lm

test-gram_grow: test-gram_grow.o turtle.o

test-gram_grow.o: test-gram_grow.c turtle.h

turtle: turtle.c turtle.h

.PHONY: clean

clean:
	rm -f *.o a.out test *.out *.bmp *.gif koch
