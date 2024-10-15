CFLAGS=-g

PUBLIC_HEADERS=waechter_message_parser.h waechter_message.h

all: build/waechter_message_parser_test build/waechter_test

clean:
	rm -rf build

build:
	mkdir -p $@

build/lib:
	mkdir -p $@

build/include:
	mkdir -p $@

build/%.o: %.c build
	$(CC) $(CFLAGS) -c $< -o $@

build/waechter_message_parser_test: build/waechter_message.o build/waechter_message_parser.o build/waechter_message_parser_test.o
	$(CC) $(CFLAGS) $^ -o $@

build/waechter_test: build/waechter_message_parser.o build/waechter_message.o build/waechter.o build/waechter_test.o
	$(CC) $(CFLAGS) $^ -o $@

lib: build/lib build/include
	$(shell for header in ${PUBLIC_HEADERS}; do cp $$header build/include; done)
