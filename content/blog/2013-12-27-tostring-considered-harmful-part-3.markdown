---
layout: post
title: "toString considered harmful, part 3"
date: 2013-12-27T22:03:00
comments: true
categories: 
- Scala
- Java
- Haskell
- Standard ML
- OCaml
- C
- C++
- C#
- Ruby
- Python
- Lisp
- Scheme
- Go
- object-oriented
- string interpolation
- type classes
---
This is part 3 of a series of articles, "`toString` considered harmful".

<!--more-->

## Languages without the `toString` problem

There actually are quite a few languages that don't have the `toString` problem, or at least have it to a lesser degree.

### C

There is no friendly generic conversion to a string (uh, really just a pointer to a null-terminated chunk of `char`) in C. The closest thing is using the type-unsafe `printf` family of functions, but you have to do almost all the work if you have some complicated `struct` and want to turn it into a C string.

### C++

C++ introduced iostreams, where f you follow certain conventions and overload `operator<<` for every domain class of interest, you can build up decent looking strings, without using inheritance.

Or you could do the object-oriented thing and set up a hierarchy with a `ToString` abstract base class. But C++ does not come with everything already inheriting from an `Object`.

### Haskell

Haskell does not force a `toString` on everything, but provides a [`Show` type class](http://www.haskell.org/tutorial/stdclasses.html) for convenience in the standard prelude. It is easy (and convenient for debugging) to just tack on `deriving Show` and then call `show` to convert stuff to a string. This means that one can get lazy and fall into the same design traps as mentioned in the very first code example above. Again, the solution is to refuse to abuse `show`, and to use a different name instead for converting something to a string for a particular purpose.

### Go

Go was invented at Google as a modernized C. It does not have classes, but does have [dynamic interfaces](http://research.swtch.com/interfaces). All that is required for a user-defined type to satisfy an interface is to implement the method `String()` returning a `string`. Basically, this makes the type implement the interface [fmt.Stringer](http://golang.org/pkg/fmt/#Stringer). Again, if you don't implement `String() string` for your type, then you will get a compile-time error when trying to treat it as a string.

Superficially, this sounds like Haskell type classes, but it's much more limited, because Go does not have generics and Go is only single dispatch. Go's interfaces really implement a kind of structural subtyping.

### Standard ML

[Standard ML](http://www.standardml.org/) does not have the `toString` problem. It does, by convention, supply a `toString` function in many modules in the [Standard ML Basis Library](http://www.standardml.org/Basis/), such as [Int](http://www.standardml.org/Basis/integer.html) and [Real](http://www.standardml.org/Basis/real.html) and [Bool](http://www.standardml.org/Basis/bool.html), but these are just conventions and do not participate in any kind of unified conversion to string. If you want to convert anything besides a primitive type to a string, you have to write the conversion function yourself.

Furthermore, Standard ML, as a rather opinionated and ["purist" language](http://mitpress.mit.edu/books/definition-standard-ml), designed specifically for static simplicity, semantic minimalism, and [runtime efficiency](http://mlton.org/), does not believe in type classes, so there is no way to write a function that at runtime is generic over what can be turned into a string.

The best you can do is write something that is functorized, but then you have to apply it in a statically known context:

{{< highlight sml >}}
signature TO_STRING =
  sig
    type t
    val toString : t -> string
  end

functor DoStuff(ToString : TO_STRING) =
  struct
    fun doubleString (stuff: ToString.t) =
      let
        val s = ToString.toString stuff
      in
        s ^ s
      end
  end

structure MyStuff : TO_STRING =
  struct
    type t = int * bool
    fun toString (i, b) =
      "(" ^ Int.toString i ^ ", " ^ Bool.toString b ^ ")"
  end

structure DoMyStuff = DoStuff(MyStuff)
{{< /highlight >}}

with

{{< highlight sml >}}
DoMyStuff.doubleString (42, true)
(* result is the string "(42, true)(42, true)" *)
{{< /highlight >}}

Since the Standard ML ecosystem is so minimalist, it's hard to fall into the `toString` trap, because you would have to set it all up yourself.

### OCaml

OCaml, like Standard ML, does not provide a generic `toString` out of the box, but the OCaml ecosystem is much more practically oriented.

There is a [pre-processor for OCaml](http://en.wikipedia.org/wiki/Camlp4) that can be used to generate convenient printers for types, [`deriving`](https://github.com/ocsigen/deriving). There is also an S-expression based generator, [Sexplib](http://realworldocaml.org/v1/en/html/data-serialization-with-s-expressions.html). But these are mechanically generated, rather than part of something generic at runtime.

Of course, one could also use the [object-oriented part of OCaml](http://caml.inria.fr/pub/docs/manual-ocaml-4.01/objectexamples.html) to make a generic "to string" hierarchy starting with a suitable interface:

{{< highlight ocaml >}}
class type convert_to_string =
  object
    method to_string : string
  end
{{< /highlight >}}

But I don't actually know many people who use the object-oriented features of OCaml!

## Conclusion

An annoying bug I temporarily created in my code led me to take stock of the state of `toString` design choices in various programming languages, and also consider how we can better escape fragility in our code, independent of whatever language we are using.
