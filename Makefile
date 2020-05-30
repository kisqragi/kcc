CFLAGS=-std=c11 -g -static -fno-common
TARGET=./bin/kcc
SRCDIR=./src
SRCS=$(wildcard src/*.c)
OBJDIR=./obj
OBJS=$(addprefix $(OBJDIR)/, $(notdir $(SRCS:.c=.o)))
INCLUDE=-Iinclude

$(TARGET): $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJDIR)/%.o : $(SRCDIR)/%.c
	$(CC) $(CFLAGS) $(INCLUDE) -o $@ -c $<

test: $(TARGET)
	./bin/test.sh

clean:
	rm -f ./bin/kcc ./obj/*

.PHONY: test clean
