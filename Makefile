.PHONY: all

all: dotproduct.so

dotproduct.o: dotproduct.c
	gcc -Wall -c dotproduct.c

dotproduct.so: dotproduct.o
	gcc -shared -o dotproduct.so dotproduct.o
