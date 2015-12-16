CC = gcc
CFLAGS = -g -Wall
LDFLAGS = -g -lm
LIBS=-lm

test: test.o turtle.o

test.o: test.c turtle.h

turtle: turtle.c turtle.h

.PHONY: clean

clean:
	rm -f *.o a.out test *.out *.bmp
