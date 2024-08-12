rcc:
	cargo build --release

test:
	cargo test

clean:
	rm -f samples/basic
	rm -f samples/unary
	rm -f samples/binary
	rm -f samples/div
	rm -f samples/logical
	rm -f samples/conditional
	rm -f samples/variables
