CFLAGS=-std=c11 -g -static -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

kcc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): kcc.h

test: kcc
	./kcc tests/tests.c > tmp.s
	gcc -static -o tmp tmp.s tests/extern.c
	./tmp

clean:
	rm -rf kcc *.o *~ tmp* tests/*~ tests/*.o

.PHONY: test clean
