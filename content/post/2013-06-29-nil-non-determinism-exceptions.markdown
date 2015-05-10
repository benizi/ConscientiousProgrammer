---
layout: post
title: "nil, non-determinism, exceptions: a journey in debugging the software that generates my blog"
date: 2013-06-29T18:15:00
url: "blog/2013/06/29/nil-non-determinism-exceptions"
comments: true
categories:
- error handling
- error messages
- nil
- null
- Ruby
- exceptions
- timeouts
- testing
- Octopress
- Jekyll
- Pygments
- Python
- dynamic typing
- static typing
- Go
- OCaml
- Scala
- Haskell
---
I mentioned in my [initial post for this blog](/blog/2013/06/24/introducing-this-new-programming-blog/) that I have had some problems with the software I use to generate my personal blog, [Octopress](http://octopress.org/), and was thinking of migrating to a different platform that might in some ways be more robust to such problems. I ended up not doing so, and I still stand by that decision, but I just yet again ran into a problem with Octopress.

Here I report on how I figured out the problem and begin a conversation about the nature of error handling and API design.

<!--more-->

## A disclaimer

I want to emphasize that although I will be critiquing some code, I do not mean to imply that I am a perfect programmer or that this software is of really poor quality. I have written software in the past that has crapped out with `null` or `nil` errors. In addition, the fact that I basically trust and use this software to drive my blogs is a reflection of the fact that it mostly works. Almost all software that has been written, is being written now, and will be written in the future has bugs. Every day we all trust and use software that we know has bugs. And this blogging platform I use is the product of many, many volunteers who contribute to the world through open source through the goodness of their hearts. I happily use this software, warts and all, and am grateful to everyone who works to maintain and improve it!

## Blog generation

The problem arose when I tried to generate [my personal blog](http://franklinchen.com/). I got a [`nil`](http://franklinchen.com/blog/2012/09/06/my-pittsburgh-ruby-talk-nil/) failure.

Here's the error message (I have omitted the 46-line stack trace):

{{< highlight console >}}
      Generating... Liquid Exception: undefined method `sub' for nil:NilClass in atom.xml
{{< /highlight >}}

This is, unfortunately, an example of an error message that is next to useless, and should *never* appear. I have stated that [we as programmers should not generate useless end user error messages](/blog/2013/06/26/we-should-not-create-this-kind-of-terrible-error-message-for-the-end-user/), and of course, in context, I am an end user of the blog generating software I use.

## Debugging and finding the root cause

Thanks to all the Ruby libraries being open source and installed on my machine, by using the stack trace and `debugger`, I figured out the immediate cause of the stray `nil`.

It turns out that Octopress uses Jekyll, which uses [pygments.rb](https://github.com/tmm1/pygments.rb) to do HTML highlighting of code blocks. (I use Octopress precisely because of its built-in support for code block highlighting.)

The bug is that the code in Jekyll that does syntax highlighting using Pygments makes an assumption that the return value of `Pygments.highlight()` is never `nil`:

{{< highlight ruby >}}
              @renderer ||= Class.new(Redcarpet::Render::HTML) do
                def block_code(code, lang)
                  lang = lang && lang.split.first || "text"
                  output = add_code_tags(
                    Pygments.highlight(code, :lexer => lang, :options => { :encoding => 'utf-8' }),
                    lang
                  )
                end

                def add_code_tags(code, lang)
                  code = code.sub(/<pre>/,'<pre><code class="' + lang + '">')
                  code = code.sub(/<\/pre>/,"</code></pre>")
                end
              end
{{< /highlight >}}

Well, it *can* be `nil` and was for me, and therefore `add_code_tags` was calling `code.sub()` with `code` being `nil`.

The bug exists in all the more recent versions of pygments.rb, including 0.4.2 (which is used for my personal blog) and 0.3.7 (which is used for this blog) and the latest released version, 0.5.1. The bug was actually reported by someone two weeks ago, and I added extra commentary, but [it has not yet been fixed](https://github.com/tmm1/pygments.rb/issues/78), so I plan to contribute a fix and submit a pull request.

## Whose responsibility to check for `nil`?

Whenever there is an error, one has to ask whose responsibility it was to

- detect the error
- handle the error

I think it is poor practice to engage in random and spotty "defensive programming" that checks for `nil` here and there, willy-nilly, just to avoid a big program crash. There should be clearly delineated boundaries of error handling.

In this case, it is surprising that a syntax highlighter for some text could return `nil`. One would think that if a syntax highlighter got confused, it could return the text verbatim, for example, without any special formatting. (By the way, don't get me started on the abominable use of HTML string hacking in this code instead of, say, building a proper HTML AST; I'll write about that topic later.)

## Documentation of API

Unfortunately, the documentation of the Pygments API was incomplete in `lib/pygments/popen.rb`, where `highlight` is defined:


{{< highlight ruby >}}
    # Public: Highlight code.
    #
    # Takes a first-position argument of the code to be highlighted, and a
    # second-position hash of various arguments specifiying highlighting properties.
    def highlight(code, opts={})
      # If the caller didn't give us any code, we have nothing to do,
      # so return right away.
      return code if code.nil? || code.empty?

      # Callers pass along options in the hash
      opts[:options] ||= {}

      # Default to utf-8 for the output encoding, if not given.
      opts[:options][:outencoding] ||= 'utf-8'

      # Get back the string from mentos and force encoding if we can
      str = mentos(:highlight, nil, opts, code)
      str.force_encoding(opts[:options][:outencoding]) if str.respond_to?(:force_encoding)
      str
    end
{{< /highlight >}}

## A problem with dynamically typed language culture

The documentation doesn't actually fully specify what the arguments can be, and does not specify what the result should be either! This is a glaring drawback of most code that I have seen in dynamically typed languages such as Ruby. People don't document exactly what can come in or out. A newcomer to a code base (such as me in this situation) cannot just read off the types and know immediately what is going on and what has been promised and what is delivered by a function. As a result, the code reader has to play compiler and read a lot more code to try to figure out what is going on.

*In principle*, someone writing code in a dynamically typed language could provide very helpful comments that amount to informal type annotations, but in practice people do not. I have found in my two decades of programming that human psychology trumps theoretical possibility any day. If something is optional, people won't do it, unless there is a very strong de facto community standard. (It turns out, for example, that what I really admire about the Ruby community is that for some reason, there is a very strong de facto community standard of doing at least some *unit testing* as part of the whole development process. I find this sadly missing in some statically typed language communities.)

In code in a statically typed language such as OCaml (or Haskell or Scala), I would expect to see:

{{< highlight ocaml >}}
let highlight (code : string) (opts : my_map) : string = //...
{{< /highlight >}}

and expect that the return value should be a `String`, or

{{< highlight ocaml >}}
let highlight (code : string) (opts : my_map) : string option = //...
{{< /highlight >}}

to indicate that the return value could be either `Some(formatted_code)` or `None`.

## `nil` all over the place

Reading the Ruby code, I saw that the situation is even more complex than I thought:

{{< highlight ruby >}}
      # If the caller didn't give us any code, we have nothing to do,
      # so return right away.
      return code if code.nil? || code.empty?
{{< /highlight >}}

Wow: `code` can be `nil`, in which case `nil` is returned.

*All code that can return `nil` should document this fact so that the caller knows what to do.* Ideally, the writer of pygments.rb should have documented this fact, and then the writer of Jekyll would in turn have added `nil` checking in its rendering code.

Furthermore, it turns out that `mentos()` can return `nil` on a non-`nil` code string! This was a surprise to me.

## `nil` is not even the real problem here: non-determinism is

`mentos()` is not very well documented. Until I read this code, I didn't realize that pygments.rb actually embeds a call to the Python interpreter to execute the Python Pygments parser to do the real work! In `lib/pygments/popen.rb`:

{{< highlight ruby >}}
    # Our 'rpc'-ish request to mentos. Requires a method name, and then optional
    # args, kwargs, code.
    def mentos(method, args=[], kwargs={}, original_code=nil)
      # Open the pipe if necessary
      start unless alive?

      begin
        # Timeout requests that take too long.
        timeout_time = 8

        Timeout::timeout(timeout_time) do
          # ...[I omitted some code]
        end
      rescue Timeout::Error
        # If we timeout, we need to clear out the pipe and start over.
        @log.error "[#{Time.now.iso8601}] Timeout on a mentos #{method} call"
        stop "Timeout on mentos #{method} call."
      end

    rescue Errno::EPIPE, EOFError
    stop "EPIPE"
    raise MentosError, "EPIPE"
    end
{{< /highlight >}}

It turns out that fundamental problem is not `nil` so much as *non-determinism*: my computer was under heavy load when I was generating my blog, and therefore the timeout kicked in and caused a failure to communicate with the Python process and therefore for `nil` to end up being returned. This non-determinism is worse than `nil`. I think that in a situation like this, an *exception* is called for. `highlight()` should actually raise an exception, which would then be propagated to Jekyll, which could give a useful error message about the timeout. I think this is a better design than return `nil` (or if using a statically typed language, a `None`).

I was also disappointed that logging was used in this code, indicating that it was known that bad things could happen, but the logged information was not used in generating a useful exception.

Amusingly, upon discovering the logging code, I used it, setting the magic environment variable `MENTOS_LOG` (from reading the code) to a file, so that I could see what happens, and verify that the timeout happened:

{{< highlight console >}}
# Logfile created on 2013-06-29 10:26:19 -0400 by logger.rb/36483
I, [2013-06-29 10:26 #13799]  INFO -- : [2013-06-29T10:26:19-04:00] Starting pid 16533 with fd 10.
I, [2013-06-29 10:26 #13799]  INFO -- : [2013-06-29T10:26:19-04:00] Out header: {"method":"highlight","args":null,"kwargs":{"lexer":"console","options":{"encoding":"utf-8","outencoding":"utf-8"},"fd":10,"id":"TVQSJNBV","bytes":147}}
E, [2013-06-29 10:26 #13799] ERROR -- : [2013-06-29T10:26:27-04:00] Timeout on a mentos highlight call
I, [2013-06-29 10:26 #13799]  INFO -- : [2013-06-29T10:26:27-04:00] Killing pid: 16533. Reason: Timeout on mentos highlight call.
I, [2013-06-29 10:26 #13799]  INFO -- : [2013-06-29T10:26:27-04:00] Killing pid: . Reason: Exiting
{{< /highlight >}}

## Testing

It turns out that there *is* a place where the intended behavior is in a sense documented: the unit tests in `test_pygments.rb`:

{{< highlight ruby >}}
  def test_returns_nil_on_timeout
    large_code = REDIS_CODE * 300
    code = P.highlight(large_code) # a 30 mb highlight request will timeout
    assert_equal nil, code
  end
{{< /highlight >}}

The test creates some huge string in hope of triggering a timeout, and then asserts that the result should be `nil`!

OK, fair enough: assuming that the Jekyll authors knew of this intention, then it was Jekyll's responsibility to test for `nil` in the call to `Pygments.highlight()`.

The test is not very satisfactory because it artificially tries to generate a timeout. Instead, one could do some mocking or refactoring of `mentos()`.

## `nil` is still not OK; but are exceptions OK?

So everything is OK, now that we can take `nil` to mean a timeout, right? No, actually, `highlight()` can still return a `nil` just because `code` was `nil`. It is bad to bin random different failure modes into one return value. Personally, I would choose to enforce that the input `code` is never `nil` and that the output is never `nil`, and that an exception is raised if there is a timeout.

An alternative is to not use an exception, but use a union success/failure type instead. This is the route that [StackMob uses for its Scala code](http://blog.stackmob.com/2013/03/why-we-avoid-throwing-exceptions-at-stackmob/). Other Scala developers such as [Jessica Kerr have been arguing against the use of exceptions also](http://blog.jessitron.com/2013/06/whats-dirtier-than-comments-exceptions.html).

Google's systems-level statically typed language [Go](http://golang.org/) deliberately [does not even have exceptions](http://golang.org/doc/faq#exceptions), and therefore mandates a style of error handling that involves [returning a success and failure everywhere](http://golang.org/doc/articles/error_handling.html).

I think there are situations for exceptions and situations for reifying errors as ordinary values in a success/failure object. I will explore this topic further in this blog. Note that even in the Haskell community, there is continued debate over how to handle errors; a famous post compares [eight different ways to handle errors in Haskell](http://blog.ezyang.com/2011/08/8-ways-to-report-errors-in-haskell-revisited/). There is no consensus because there are a lot of tradeoffs involved when it comes to error handling.

## My fix

For now, before I propose to pygments.rb that an exception be raised (this would be least intrusive to Jekyll, whose source code would not need to be changed for the exception to propagate usefully), I simply hacked `8` to `80` to avoid a timeout!

### (Update of 2013-12-18)

The `pygments.rb` library [has finally been updated with a `MENTOS_TIMEOUT` environment variable](https://github.com/tmm1/pygments.rb/commit/e0ed7f73f03aa59680b469f4f26e208d3cf8d999).

This is obviously a short-term hack, and I don't see how a typical Octopress user would even find out about this new environment variable, but until I implement a better solution and submit a pull request to the `pygments.rb` team, I have no right to complain!

## Conclusion

I've used my recent frustration at being unable to publish my personal blog as a vehicle to begin a conversation about error handling and the issues of API design and documentation in the face of errors. I hope you enjoyed the ride.
