CFLAGS=-std=c11 -g -static -fno-common

kcc: main.o
	$(CC) -o $@ $? $(LDFLAGS)

test: kcc
	./test.sh

clean:
	rm -f kcc *.o tmp*

.PHONY: test clean
