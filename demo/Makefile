CC = gcc
CFLAGS = -g -Wall
LDFLAGS = -g -lm
LIBS=-lm

demo: demo.o turtle.o

demo.o: demo.c turtle.h

turtle: turtle.c turtle.h

.PHONY: clean

clean:
	rm -f *.o a.out demo demo.c *.out *.bmp *.gif
