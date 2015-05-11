---
layout: post
title: "Pittsburgh Ruby lightning talk night: RVM, business logic with Rails, IRC, rspec-given, Vagrant"
date: 2013-07-11T20:53:00
url: "blog/2013/07/11/pittsburgh-ruby-lightning-talk-night/"
comments: true
categories: 
- Pittsburgh
- Ruby
- RVM
- Rails
- IRC
- RSpec
- Vagrant
- Python
- Steel City Ruby
- Jim Weirich
---
The [Pittsburgh Ruby meetup](http://www.meetup.com/pittsburgh-ruby) held a [lightning talks session](http://www.meetup.com/pittsburgh-ruby/events/120200102).

Originally, I was not going to present anything, since I did not feel that I had anything exciting to quickly share (I have not been doing much Ruby programming lately at all other than [debugging my Octopress-generated blog](/blog/2013/06/29/nil-non-determinism-exceptions)), and don't like talking just to talk.

But at the very last minute, just half an hour before the meeting, I noticed some developments in the world of [RSpec](http://rspec.info/) announced on Twitter by Jim Weirich, and I got excited enough that I decided to talk about his [`rspec-given`](https://github.com/jimweirich/rspec-given), which was just released at version 3.0.0.

<!--more-->

## Colin, on `rvm_recommended_ruby`

Colin briefly talked about how he likes to automate things, and therefore, in the context of using [RVM](https://rvm.io/), created [`rvm_recommended_ruby`](https://github.com/wayneeseguin/rvm/pull/1074).

## Jon, on business logic in pure Ruby

Jon gave a short summary of his [blog post](http://jonathandean.com/2013/07/business-logic-in-pure-ruby/) on not putting all business logic into Rails models, but instead, decoupling from `ActiveRecord` and Rails altogether. This is a theme that has risen a lot in recent years in the Rails community.

## Carol, on how to use IRC to get help

Carol talked about how to use IRC to get help when working on projects.

## Me, on `rspec-given`

I talked about cool features in [`rspec-given`](https://github.com/jimweirich/rspec-given).

`rspec-given` has actually been around for a while, but I had not used in the past. I plan to use it in the future.

It provides `Given`, `When`, `Then`, and `And` for writing specifications.

Also, now there is `Invariant`, which I think is a great addition to `RSpec`, enabling an easy way to check invariants as part of every example.

Finally, I love the new natural assertions, the ability to write stuff like

{{< highlight ruby >}}
Then { stack.top == :second_item }
{{< /highlight >}}

instead of

{{< highlight ruby >}}
Then { expect(stack.top).to eq(:second_item) }
{{< /highlight >}}

while `RSpec` automatically generates useful messages on failure.

## Colin again, on Vagrant

Since all the lightning talks were done in less than half an hour, Carol called for more volunteers to talk about something. Colin ended up impromptu talking about his use of [Vagrant](http://www.vagrantup.com/) to create development environments.

There was particular interest in how to share files with the host file system. Vagrant makes this easy.

Vagrant is great. Use it.

## Ruby meets Python

Carol reminded everyone that coming up is a Ruby/Python joint social.

## Conclusion

A relatively short Pittsburgh Ruby meetup, and with a smaller crowd than some others in the past, but a fun and instructive one.

Don't forget, [Steel City Ruby Conference 2013](http://steelcityruby.org/) is in a month! I'm [excited to attend again](http://franklinchen.com/blog/2012/08/07/the-first-steel-city-ruby-conference-an-amazing-experience/).
