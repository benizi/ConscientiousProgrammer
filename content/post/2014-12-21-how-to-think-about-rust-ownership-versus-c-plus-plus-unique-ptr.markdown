---
layout: post
title: "How to think about Rust ownership versus C++ unique_ptr"
date: 2014-12-21T14:31:36-05:00
url: "blog/2014/12/21/how-to-think-about-rust-ownership-versus-c-plus-plus-unique-ptr"
comments: true
categories:
- Rust
- C
- C++
- pointers
- memory safety
- types
- ownership
- X Window System
- segmentation fault
---
The [Rust](http://www.rust-lang.org/) programming language, which is nearing its important version 1.0 release, but already seen a lot of use, has many interesting features, the most prominent of which is its [ownership](http://doc.rust-lang.org/guide.html#ownership,-borrowing,-and-lifetimes) static type system that prevents certain kinds of bugs common in many programs in C or C++.

For example, it is common in most applications to create graphs of objects pointing to each other, in which it is not clear how many pointers point to a particular object, who is to free the object's memory when it is no longer needed, and what happens to all outstanding pointers to that block. Languages supporting precise [garbage collection](http://en.wikipedia.org/wiki/Garbage_collection_%28computer_science%29) use various algorithms to determine when memory can be freed, and have become particularly popular in the past twenty years, but there are still situations in which garbage collection is not ideal, hence the continued relevance of languages such as C++.

When I was working as a software engineer in the 1990s developing desktop applications with user interfaces for the [X Window System](http://en.wikipedia.org/wiki/X_Window_System), we used frameworks that included C++ [smart pointers](http://en.wikipedia.org/wiki/Smart_pointer) that used [reference counting](http://en.wikipedia.org/wiki/Reference_counting) to handle graphs of interconnected data. The C++ standard itself lagged behind in standardizing smart pointers; it started with the [terribly flawed and unusable](http://www.informit.com/articles/article.aspx?p=30642&seqNum=9) [`auto_ptr`](http://en.wikipedia.org/wiki/Auto_ptr) that finally got deprecated in C++11, then moved on finally to [`unique_ptr`](http://en.wikipedia.org/wiki/Smart_pointer#unique_ptr) and [`shared_ptr` and `weak_ptr`](http://en.wikipedia.org/wiki/Smart_pointer#shared_ptr_and_weak_ptr).

When talking with C++ programmers about Rust, I have found that often they have been puzzled about how Rust's ownership system does anything better than what C++ `unique_ptr` already does. Here is an explanation (I will not be discussing analogues of `shared_ptr` and `weak_ptr` here).

<!--more-->

## Simplest example: compile-time type-checking vs. run-time segmentation fault

First, go read Steve Klabnik's [article about the simplest possible example illustrating what Rust offers over C++ `unique_ptr`](http://www.steveklabnik.com/uniq_ptr_problem/).

Basically, in C++, `unique_ptr` is supposed to be a way to indicate that a given pointer is the unique "owner" of a piece of data. If you want to transfer ownership from one `unique_ptr` to another, you have to call `move` on it. After that, it is an *run-time error* to try to access the data using the original `unique_ptr`. This is great, except for two things:

- By "it's a run-time error", we mean that the original `unique_ptr`'s embedded pointer gets mutated to `nullptr`, so that you have to perpetually check your `unique_ptr` for `nullptr`, otherwise get a segmentation fault.
- In many situations, we'd prefer to have a *compile-time* guarantee that we will never dereference `nullptr`, so that we don't have hidden memory safety bugs lying around in our code that don't happen until well into a long-running program.

Here is Steve's example of an unintended segmentation fault from C++:

{{< highlight cpp >}}
#include <iostream>
#include <memory>

using namespace std;

int main ()
{
    unique_ptr<int> orig(new int(5));

    cout << *orig << endl;

    auto stolen = move(orig);

    cout << *orig << endl;
}
{{< /highlight >}}

Steve shows an "equivalent" Rust program in which code attempts to move ownership fails to compile because of a type error (I edited to correspond more closely with the C++):

{{< highlight rust >}}
fn main() {
    let orig = box 5i;

    println!("{}", *orig);

    let stolen = orig;

    println!("{}", *orig);
}
{{< /highlight >}}

## Understanding C++ behavior by modeling it in Rust, safely

But it is not quite accurate to call the Rust program "equivalent" to the original C++ program. It's really a "cleaned up" or "restricted" version of that program (and the argument for using a type-safe language like Rust is that we often like these restrictions, as a tradeoff for safety or efficiency).

Here is a Rust program that more accurately models what the C++ `unique_ptr` actually does, which happens *dynamically* at run-time, not at compile time, hence the segmentation fault: the transfer of ownership by `move` on a `unique_ptr` is not type-checked at compile-time.

For illustration's sake, we have modeled C++ directly in *safe* Rust through indirection by treating a C++ pointer to `T` (C++ pointers can always be `nullptr`) as an `Option<Box<T>>`, where `nullptr` is modeled as `None` and a non-null pointer is modeled as `Some(pointer_nonnull)`. This Rust code type-checks, compiles, and simulates a segmentation fault.

You can play with this code [at this Rust playground link](http://is.gd/5kEQA8). (You may find it interesting to examine the assembly code generated.)

{{< highlight rust >}}
// Simulate a segmentation fault.
fn seg_fault() {
    panic!("segmentation fault");
}

fn main() {
    // Rough modeling in Rust of C++ unique_ptr<int>
    // because C++ pointers can always be null.
    let mut orig: Option<Box<int>> = Some(box 5i);

    match orig {
        // In C++, deferencing null seg faults.
        None => seg_fault(),
        Some(orig_nonnull) => {
            println!("{}", *orig_nonnull);

            // The equivalent of C++ unique_ptr move.
            let stolen = Some(orig_nonnull);
            orig = None;

            match orig {
                // We seg fault after the C++ style move.
                None => seg_fault(),
                Some(orig_nonnull) => println!("{}", *orig_nonnull)
            }
        }
    }
}
{{< /highlight >}}

Nobody would ever write this kind of code in Rust, but implicitly, all C++ programs are semantically basically doing this, except that for efficiency, the `nullptr` checking is not done at the language level but just results in an actual segmentation fault at the operating system level.

## An example of smart pointers inside a collection

The single most disturbing failure mode of the C++-based software product I worked on in 1995-1997 in C++ was segmentation faults resulting from mysteriously disappearing pointers. What I mean is, behavior like the following, where a container containing smart pointers to objects was meant to *own* them, but there was no compile-time way to verify this fact, because of the nature of the object graph. If someone didn't play by the uncheckable rules and inadvertently took ownership of (instead of "borrowing" through a raw pointer) something embedded in the collection, then `nullptr` appeared, and a segmentation fault happened.

{{< highlight cpp >}}
#include <vector>
#include <memory>
#include <iostream>

using namespace std;

int main() {
    vector<unique_ptr<int>> v;
    v.push_back(make_unique<int>(5));

    cout << *v[0] << endl;

    // C++ happily allows this.
    auto pointer_to_5 = move(v[0]);
    cout << *pointer_to_5 << endl;

    // Seg fault.
    cout << *v[0] << endl;
}
{{< /highlight >}}

You can probably guess what some developers did to "fix" the segmentation fault. They started adding `nullptr` checks everywhere to prevent crashing, but this only resulted in *corrupt user data and documents*, because in fact, we lost data in those collections and smart pointers to the data should never have ended up `nullptr`!!

The complexity of the application and the deadline pressures made it impossible to fully figure out what was going wrong and where. I actually ended up writing an external "validation" utility program in [Standard ML of New Jersey](http://www.smlnj.org/) that parsed serialized object graphs and tried to fix them up in some fashion. This particularly helped the QA team a lot when dealing with old documents that were already corrupted.

### The Rust version will not compile

The Rust type-checker rejects any attempt to move ownership out of a collection. (Try to compile it in [this playground](http://is.gd/Om7wfr).)

{{< highlight rust >}}
fn main() {
    let v = vec![box 5i];

    println!("{}", *v[0]);

    // Attempted move: type error at compile time.
    let pointer_to_5 = v[0];
    println!("{}", *pointer_to_5);

    println!("{}", *v[0]);
}
{{< /highlight >}}

{{< highlight console >}}
illegal_move_out_of_vector.rs:7:24: 7:28 error: cannot move out of dereference (dereference is implicit, due to indexing)
illegal_move_out_of_vector.rs:7     let pointer_to_5 = v[0];
                                                       ^~~~
illegal_move_out_of_vector.rs:7:9: 7:21 note: attempting to move value to here
illegal_move_out_of_vector.rs:7     let pointer_to_5 = v[0];
                                        ^~~~~~~~~~~~
{{< /highlight >}}

### Often, you do want to share

I have to be honest: at first, Rust's ownership type system seems like quite a restriction, and adhering to it strictly would require the restructuring of certain kinds of programs. Static type safety of any kind in any language is always a matter of tradeoffs.

But note that Rust is flexible: you don't have to use Rust's default pointer type! You can use one of Rust's many other pointer types, such as [`Rc` or `Arc`](http://doc.rust-lang.org/guide.html#rc-and-arc), that allow reference-counted shared ownership (like C++ `shared_ptr`), if that's what you really want. ([Playground here](http://is.gd/HNi7SR).)

{{< highlight rust >}}
// Reference-counted smart pointer.
use std::rc::Rc;

fn main() {
    let v = vec![Rc::new(5i)];

    println!("{}", *v[0]);

    let pointer_to_5 = v[0].clone();
    println!("{}", *pointer_to_5);

    println!("{}", *v[0]);
}
{{< /highlight >}}

## Unsafe

Finally, Rust does allow you to go all out and write [unsafe](http://doc.rust-lang.org/guide.html#unsafe) code, if you truly need to for raw C performance or FFI reasons. Most of the time you do not need to, because the whole point of Rust is to compile down to the same kind of assembly code you would get from C.

{{< highlight rust >}}
extern crate libc;

use libc::{size_t, malloc};
use std::mem;
use std::ptr;

fn main() {
    // How ugly it is to pretend Rust is unsafe C.
    unsafe {
        let mut orig: *mut int = malloc(mem::size_of::<int>() as size_t)
            as *mut int;
        ptr::write(&mut *orig, 5i);

        println!("{}", *orig);

        orig = ptr::null::<int>() as *mut int;

        // null pointer crash!
        println!("{}", *orig);
    }
}
{{< /highlight >}}

## Conclusion

I hope this little article shows a little bit about what Rust's ownership type system can do to make pointer-heavy code memory safe, unlike C++, and also gives you a taste of how Rust's flexibility also allows you to use C++-style reference-counting if desired, and even raw unsafe code. Personally, I am excited about the upcoming 1.0 release of Rust, and although I have not done low-level systems programming for almost two decades, if I ever was to do it again, I would immediately reach out for Rust as a language of choice for the ultimate combination of safety, expressiveness, and performance (in use of time and space).

All code for this article is available in a [GitHub repository](https://github.com/FranklinChen/rust-vs-cpp-unique_ptr).
