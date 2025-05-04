# book
A compiled language with some nice type inference and a garbage collector. 
---

## Overview
Book is a tiny compiled programming language designed to combine performance and simplicity. With strong type inference and an efficient garbage collector (the Boehm GC), Book aims to be both enjoyable and productive.

## Getting Started

### Installation

To get started, clone this repository and build the project:

```bash
git clone https://github.com/re-masashi/book.git
cd book
cargo build --release
```

### Getting Started

Write your first program in Book:

```python
println("Hello, world!")
```

Compile and run your program:

```bash
bookc hello.book
./hello
```

## Example

```python
def count_to_1B(n: int) -> int
	if n < 1_000_000_000 then
		count_to_1B(n+1)

let items = [
	[1,2,3],
	[3,4,3],
	[5,6,3]
]

println(list[1][1])
# outputs 4
```

## Roadmap

- [x] Parser errors based on spans instead of position.
- [x] Error reporting duing codegen.
- [ ] Enums and maybe ADTs
- [?] Solidify the typechecker
- [ ] Concurrency
- [?] Standard Library
- [x] Dynamic Arrays

## License

This project is licensed under the MIT License.