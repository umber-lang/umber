# Umber

## Disclaimer

This project is very much a work in progress, and is honestly quite a mess at the moment. I'm working on it here and there in my free time, and doing some of the design and implementation in parallel, so it is very ad-hoc. The project will probably not be in a good state for use or contribution for some time.

## Overview

Umber is an ML-style functional programming language. It is modelled after OCaml with some improvements, including:

- Algebraic effects for tracking function side effects at the type level while also providing convenience and flexibility in creating custom control flow
- Better support for ad-hoc polymorphism with traits
- First-class support for laziness
- Proper handling of unicode, both in the source code and in the standard library's `String` type
- Proper handling of nested module namespaces
- Whitespace-sensitive syntax eliminating syntactic noise like `begin` and `end` or extra parentheses
- An import style similar to Python which discourages importing entire modules unqualified

Some other planned features include:

- Better support for concurrent programming (async/await at least can be implemented with effects)
- A macro system

### (Very) Rough Roadmap
- [x] Lexing
- [x] Parsing
- [ ] Name resolution
- [ ] Basic type inference and checking
- [ ] Record types
- [ ] Traits
- [ ] Effects
- [ ] Basic code generation
- [ ] Garbage collection
- [ ] Self-hosting
