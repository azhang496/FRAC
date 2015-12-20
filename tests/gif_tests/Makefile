CC = gcc
CFLAGS = -g -Wall
LDFLAGS = -g -lm
LIBS=-lm

koch: koch.o turtle.o

koch.o: koch.c turtle.h

turtle: turtle.c turtle.h

.PHONY: clean

clean:
	rm -f *.o a.out test *.out *.bmp *.gif koch
