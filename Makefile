rcc:
	cargo build --release

test:
	cargo test

clean:
	rm -f samples/basic
	rm -f samples/unary
