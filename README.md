<div align="center">

# Umber

</div>

## Disclaimer

This project is very much a work in progress, and is honestly quite a mess at the moment. I'm working on it here and there in my free time, and doing some of the design and implementation in parallel, so it is very ad-hoc. The project will probably not be in a good state for use or contribution for some time.

## Overview

Umber is an ML-style functional programming language. It is modelled after OCaml with some improvements, including:

- Algebraic effects for tracking function side effects at the type level while also providing convenience and flexibility in creating custom control flow
- Better support for ad-hoc polymorphism with traits
- Proper handling of unicode, both in the source code and in the standard library's `String` type
- Support for nested module namespaces at the file/folder level
- No ml/mli file distinction, eliminating duplication of type/module declarations
- Treating value constructors as regular curried functions, not a special construct
- An import style which discourages glob imports

Some other planned features include:

- Tracking mutability in the type system
- Support for implicit laziness
- Better support for concurrent and parallel programming 
- A macro system

### Code Examples

```
# Naive Fibonacci numbers
let fib = match
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
```

```
# Types are defined like this:
type Option a =
  | Some a
  | None
```

```
# Modules define abstraction boundaries.
# The part after `:` is the "signature", the interface of the module. 
# The part after `=` is the "definition", the implementation of the module.
# Code outside the module can only see the signature.
module PositiveInt : {
  type PositiveInt

  val of_int : Int -> Option PositiveInt
  val to_int : PositiveInt -> Int
} = {
  type PositiveInt = Int

  let to_int x = x

  let of_int x = if x > 0 then Some x else None
}
```

```
# Algebraic effects are a universal control flow abstraction that subsumes exceptions,
# mutable state, iterators, async, etc.
effect Exception = {
   val raise : String -> Never
}

let foo () = if True then raise "error" else 42

let try_with_default f default =
  handle f ()
  | <raise _> -> default

let _ : Int = try_with_default foo 0
```

You can see more code examples used for testing under `test/examples`.

### (Very) Rough Roadmap
- [x] Lexing
- [x] Parsing
- [x] Name resolution
- [x] Basic type inference and checking
- [x] Code generation to LLVM
- [ ] Record types
- [ ] Traits
- [ ] (In progress) Effects
- [ ] Garbage collection
- [ ] Self-hosting
