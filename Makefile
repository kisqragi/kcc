CFLAGS=-std=c11 -g -static -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

kcc: $(OBJS)
	$(CC) $(CFLAGS) -o kcc $(OBJS) $(LDFLAGS)

$(OBJS): kcc.h

kcc-stage2: kcc $(SRCS) kcc.h self.sh
	./self.sh tmp-stage2 ./kcc kcc-stage2

kcc-stage3: kcc-stage2
	./self.sh tmp-stage3 ./kcc-stage2 kcc-stage3

test: kcc
	./kcc tests/tests.c > tmp.s
	$(CC) -static -o tmp tmp.s tests/extern.c
	./tmp

test-stage2: kcc-stage2
	./kcc-stage2 tests/tests.c > tmp.s
	$(CC) -static -o tmp tmp.s tests/extern.c
	./tmp

test-stage3: kcc-stage3
	diff kcc-stage2 kcc-stage3

test-all: test test-stage2 test-stage3

clean:
	rm -rf kcc kcc-stage* *.o *~ tmp* tests/*~ tests/*.o

.PHONY: test clean
