# rcc - the Rust C Compiler

As the name suggests, a C Compiler written in Rust. What else would you write a C compiler in.
Much nicer an experience than writing a Rust compiler in C, that's for sure. 

The goal is for it to compile C programs, the more the better. No guarantees whatsoever will be provided.
The end goal is for it to be a complete C compiler with hand-crafted codegen for x86 and aarch64, alongside LLVM support.

## Feature list

- [ ] Functions
- [ ] Loops
- [ ] Labeled statements & goto
- [ ] Increment & decrement operators
- [ ] Compound assignment (+=, -=, *=, /=, %=)
- [ ] Bitwise operators (&, |, ^, <<, >>)
- [X] Compound statements 
- [X] If statements 
- [X] Basic statements (return & null) 
- [X] Conditional ternary expression 
- [X] Variables 
- [X] Logical binary operators (&&, ||, ==, !=, <=, >=, <, >) 
- [X] Arithmetic binary operators (+, -, *, /, %) 
- [X] Logical 'not' unary operator
- [X] Unary operators (complement & negation)
