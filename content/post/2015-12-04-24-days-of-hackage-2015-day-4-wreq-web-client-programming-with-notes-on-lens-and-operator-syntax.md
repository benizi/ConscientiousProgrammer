---
categories:
- Haskell
- Hackage
- wreq
- JSON
- lens
- aeson
- syntax
- Pittsburgh Code and Supply
- Standard ML
- OCaml
- Elm
- Elixir
- types
- domain-specific languages
comments: true
date: 2015-12-04T17:05:23-05:00
layout: post
title: "24 days of Hackage, 2015: day 4: wreq: Web client programming;
with notes on lens and operator syntax"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 4

In the late 1990s, I eagerly bought the book
["Web Client Programming with Perl"](http://www.oreilly.com/openbook/webclient/)
and used the [LWP](http://search.cpan.org/dist/libwww-perl/lib/LWP.pm)
library to scrape the Web in automated fashion. I continued doing that
into the 2000s. I am happy that nowadays, I can just use Haskell to do
this kind of programming, in a succinct way also.

Today's topic is [`wreq`](http://www.serpentine.com/wreq/),
[Bryan O'Sullivan](http://www.serpentine.com/blog/)'s high-level
library for doing Web client programming designed specifically for
usability.

`wreq` makes use of the
[`aeson`](https://hackage.haskell.org/package/aeson) ecosystem for JSON
and [`lens`](https://hackage.haskell.org/package/lens) and ecosystem,
including
[`lens-aeson`](https://hackage.haskell.org/package/lens-aeson), so you
may want to check out Ollie's 2012 Days of Hackage posts on
[aeson](https://ocharles.org.uk/blog/posts/2012-12-07-24-days-of-hackage-aeson.html)
and [lens](https://ocharles.org.uk/blog/posts/2012-12-09-24-days-of-hackage-lens.html).

Since `wreq` already has an extensive
[tutorial and reference documentation](http://www.serpentine.com/wreq/),
I'm not going to repeat its explanations. Instead, I'm going to give an
example of use that should be simple enough to be understood from
context, then discuss the issue of using operator syntax in Haskell.

<!--more-->

## The task

I'm a member of many groups on [Meetup](http://www.meetup.com/). It's
often useful for me to get information using the official
[Meetup API](http://www.meetup.com/meetup_api/) rather than go around
clicking on a Web site on or a mobile app. Why do by hand what I can
do much more efficiently and correctly with code?

Here's a very simplified example of something I might want to do with
Meetup. I've been active in the
[Pittsburgh Code and Supply](http://www.codeandsupply.co/) community,
which has a
[Meetup site](http://www.meetup.com/Pittsburgh-Code-Supply/) with a
packed calendar of events (it's on hiatus now in December for the
holidays, but is otherwise very active). Maybe I want to find out what
upcoming events they are, and search for events of interest according
to some criteria. For our toy example here, let's say I want to find
the ten upcoming events and get their names and venue names, and make
sure there's at least one event that has a name and venue name already
set up (sometimes, an event is proposed but no venue has been found
yet).

## A test

Yesterday,
[day 3](/blog/2015/12/03/24-days-of-hackage-2015-day-3-hspec-the-importance-of-testing/)
of this article series, I mentioned liking using HSpec, so let's use
HSpec.

{{< highlight haskell >}}
{-# LANGUAGE OverloadedStrings #-}

import WreqExample (GroupId, eventName, venueName, getMeetupEventInfos)
import Test.Hspec ( Spec, hspec, describe, it
                  , shouldSatisfy, shouldNotSatisfy
                  )
import qualified Data.Text as Text
{{< /highlight >}}

We are using the [`text`](https://hackage.haskell.org/package/text)
packed Unicode string type, because that's what `wreq`
uses. `OverloadedStrings` is a convenient GHC extension that allows
string literals in code to be treated as `Text` values rather than
`String`. Ollie discusses this extension in his [2014 Days of GHC Extensions](https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html).

Also, since I'm operating in test-driven development style, I wrote
this test first, before writing the `WreqExample` module: I only wrote
the imports for what I need for the test.

{{< highlight haskell >}}
spec :: Spec
spec =
  describe "wreq" $ do
    it "there are named, located Pittsburgh Code and Supply events coming up" $ do
      -- Warning! This is a stateful test going out to the Web.
      events <- getMeetupEventInfos pittsburghCodeAndSupplyId
      events `shouldNotSatisfy` null
      events `shouldSatisfy` any
        (\event -> (not . Text.null . eventName) event
                   && (not . Text.null . venueName) event)

pittsburghCodeAndSupplyId :: GroupId
pittsburghCodeAndSupplyId = "13452572"
{{< /highlight >}}

## Module signatures

If Haskell had
[module signatures, like Standard ML and OCaml do](http://jozefg.bitbucket.org/posts/2015-01-08-modules.html),
I would write an explicit module signature for the module I intend to
implement that will conform to that signature, but Haskell doesn't, so
the best we can do is operate in "duck typing" manner at the module
level, relying implicitly on compilation to fail on import of a
conforming module implementation rather than on matching against an
explicit signature without the need for an implementation.

Here are the types we need (in a pseudo-syntax as though Haskell had
module signatures):

{{< highlight haskell >}}
type GroupId    -- abstract

type EventInfo  -- abstract

-- abstract type accessors
eventName :: EventInfo -> Text
venueName :: EventInfo -> Text

getMeetupEventInfos :: GroupId -> IO [EventInfo]
{{< /highlight >}}

## Implementation

### Imports

{{< highlight haskell >}}
import Network.Wreq (Options, defaults, param, getWith, asValue, responseBody)
import Data.Text (Text)
import Data.Aeson (Value)
import Control.Lens (view, set, toListOf)
import Data.Aeson.Lens (key, _Array, _String)
{{< /highlight >}}

### Types

{{< highlight haskell >}}
-- | Information that we care about from a Meetup event.
data EventInfo =
  EventInfo { eventName :: Text
            , venueName :: Text
            }
  deriving (Show)

-- | A valid Meetup group ID.
type GroupId = Text
{{< /highlight >}}

### The Web client part

Since we're only making one request, and are not doing any error
handling, but letting `wreq` throw exceptions instead, the Web client
part is very brief. The Meetup API allows returning information as
JSON.

{{< highlight haskell >}}
meetupEventsUrl :: String
meetupEventsUrl = "https://api.meetup.com/2/events"
{{< /highlight >}}

We perform a `GET` with query parameters. `wreq` uses lens as its
domain-specific language for creating options for `GET`, so let's
create a `wreq` `Options` value, by setting the parameters one after
another using a builder pattern starting with the `wreq` `defaults`:

{{< highlight haskell >}}
eventsOptions :: GroupId
              -> Options
eventsOptions groupId =
  set (param "page") ["10"] (
    set (param "order") ["time"] (
      set (param "status") ["upcoming"] (
        set (param "group_id") [groupId] (
          set (param "format") ["json"] defaults))))
{{< /highlight >}}

We begin by going out to the Web to get back a response, which is a
[lazy `ByteString`](https://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString-Lazy.html):

{{< highlight haskell >}}
getMeetupEventInfos :: GroupId -> IO [EventInfo]
getMeetupEventInfos groupId = do
  response <- getWith (eventsOptions groupId) meetupEventsUrl
{{< /highlight >}}

### The JSON part

Then we parse the lazy `ByteString` response, including the headers
and the body, into an untyped JSON object, an `aeson`
[`Value`](https://hackage.haskell.org/package/aeson-0.10.0.0/docs/Data-Aeson.html#t:Value):
{{< highlight haskell >}}
  jsonResponse <- asValue response
{{< /highlight >}}

More precisely, [`Value` is unityped](https://hackage.haskell.org/package/aeson-0.10.0.0/docs/src/Data-Aeson-Types-Internal.html#Value):

{{< highlight haskell >}}
type Object = HashMap Text Value

type Array = Vector Value

data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null
{{< /highlight >}}

### The lens part

It was annoying figuring out from the official
Meetup API site what fields I needed from the response and what their
types were supposed to be. In practice I just saved off JSON from a
representative query and looked at some events to see what I wanted. I
was told where to find the
[automatically generated documentation of all the API methods](meetup.json)
but it was not ideal. A later Day of Hackage will discuss what I did
about this problem.

We extract the list of events, using a traversal to get the whole
list, which is encoded as a JSON array in the top level JSON object's
`results` field:

{{< highlight haskell >}}
  let events = toListOf (responseBody
                         . key "results"
                         . _Array . traverse
                        ) jsonResponse
{{< /highlight >}}

Here we use `toListOf` from lens with a traversal and a JSON object to
pull out everything from that traversal.

Finally, since we only want, for each event, its name and
its venue's name (the venue's name is actually a field in a venue
object):

{{< highlight haskell >}}
  return (map jsonToEventInfo events)
{{< /highlight >}}

We again use lens, at the level of an individual event object, to
extract what we want from it:

{{< highlight haskell >}}
-- | Extract our typed data model from an untyped JSON object.
jsonToEventInfo :: Value -> EventInfo
jsonToEventInfo json =
  EventInfo { eventName = view (key "name" . _String) json
            , venueName = view (key "venue"
                                . key "name" . _String) json
            }
{{< /highlight >}}

Here we use the `view` function of `lens`, to apply a lens to the JSON
object to pull a field out of it.

And we're done! We've written a script that looks pretty much like
what you'd write in Perl or Python. It will also "fail" in similar
ways, because we're basically not using any types at all; even the
final result just has strings, which may or may not be empty, whatever
that's supposed to mean. For example, if you try to find a field by a
string key that doesn't exist, the particular code here will just
silently give back an empty string. Can we do better? Yes, there are
various ways to do better. Stay tuned for a later Day of Hackage.

## Lens operator syntax

If you've already used `wreq` or `lens`, you may have noticed
something strange above: I didn't use any `lens` operator syntax. This
was deliberate. Although the `wreq` tutorial gives a
[little bit of background on `lens`](http://www.serpentine.com/wreq/tutorial.html#a-quick-lens-backgrounder),
the reality is that when some friends who were not experienced lensers
or Haskellers asked me how I do Web client programming in Haskell, and
I pointed to `wreq` as being pretty cool, they got immediately stuck
on the lens stuff. Looking back at the tutorial, I do see that it
jumps straight into operator soup. This is unfortunate. You can
immediately use libraries like `wreq` without having the lens
operators memorized already. You have to understand some facts (such
as the use of the function composition operator to compose lenses) and
have an idea of how the types work out, but one thing you don't need
is the funny operators. I think it's best to understand how to do
things without operators before starting to use them as a convenient
shortcut.

For example, an idiomatic way to set the options object, as presented
in the "whirlwind tour" section of the `wreq` tutorial, is:

{{< highlight haskell >}}
import Control.Lens ((&), (.~))

eventsOptions :: GroupId
              -> Options
eventsOptions groupId = defaults
  & param "format" .~ ["json"]
  & param "group_id" .~ [groupId]
  & param "status" .~ ["upcoming"]
  & param "order" .~ ["time"]
  & param "page" .~ ["10"]
{{< /highlight >}}

I don't like the idea of newcomers to this library just copying and
pasting stuff without understanding what it does, or getting the
impression that these operators are somehow built into the Haskell
language or required for using the library. People really do get these
impressions.

I happen to like the reverse function operator `&` a lot, although
it's not as suggestive as the exact same reverse function operator in
many other languages (such as F#, OCaml, Elm, Elixir) in the form of a pipe
instead
[`|>`](http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Basics#|%3E),
so I feel OK about using it.

But the `.~` is I think not very suggestive to newcomers to
`lens`. Is `set lens newValue object` so much worse to write or read than
`object & lens .~ newValue`?

## (Update of 2014-12-12) Thinking compositionally

One thing that is unfortunately lost if you use pipeline application
operators such as `&` is the *compositionality* that underlies the
power of lenses. So here is a refactoring of `eventsOptions` that
shows how to best think of what we are doing, which is creating a
"builder" and applying it:

{{< highlight haskell >}}
eventsOptionsRefactored :: GroupId -> Options
eventsOptionsRefactored groupId = builder defaults
  where builder = eventsOptionsBuilder groupId

-- | Recall: type is sugar for GroupId -> (Options -> Options)
eventsOptionsBuilder :: GroupId -> Options -> Options
eventsOptionsBuilder groupId =
  set (param "page") ["10"]
  . set (param "order") ["time"]
  . set (param "status") ["upcoming"]
  . set (param "group_id") [groupId]
  . set (param "format") ["json"]
{{< /highlight >}}

Note the separation of concerns here: instead of thinking of building
an `Options` object as

- starting with a default
- successively applying an extra setting to it

we think of

- creating an options builder through composition
- applying the builder to the default

Partial application in functional programming is used here to
implement the builder pattern: `eventsOptionsBuilder` takes one
argument, and returns an `Options` transformer of type `Options ->
Options`.

## Code golf?

To illustrate both the up sides and down sides of using operators (but
in this case mostly down sides, I think), here is a code golf version
of the entire code:

{{< highlight haskell >}}
import Network.Wreq (Options, defaults, param, getWith, asValue, responseBody)
import Data.Text (Text)
import Control.Lens ((&), (.~), (^.), (^..))
import Data.Aeson.Lens (key, _Array, _String)
import Control.Arrow ((>>>), (&&&))

meetupEventsUrl :: String
meetupEventsUrl = "https://api.meetup.com/2/events"

-- | A valid Meetup group ID.
type GroupId = Text

-- | For searching for events in a Meetup group.
eventsOptions :: GroupId
              -> Options
eventsOptions groupId = defaults
  & param "format" .~ ["json"]
  & param "group_id" .~ [groupId]
  & param "status" .~ ["upcoming"]
  & param "order" .~ ["time"]
  & param "page" .~ ["10"]

-- | Code golf version. Don't do this?
getMeetupNameAndVenues :: GroupId -> IO [(Text, Text)]
getMeetupNameAndVenues groupId =
  getWith (eventsOptions groupId) meetupEventsUrl
  >>= asValue
  >>= ((^.. responseBody
        . key "results"
        . _Array . traverse)
       >>> map ((^. key "name" . _String)
                 &&& (^. key "venue"
                      . key "name" . _String)
                 )
       >>> return
      )
{{< /highlight >}}

In a way, this looks cool because the piping left to right reads well
and naturally, if you know all the operators and are happy with
operator sectioning syntax and point-free combinators. But when I
showed this to friends who are not so fluent in Haskell, they didn't
like this. Also, note that I made concessions in order to arrange this
pipeline. I lost the comments, the intermediate named sub-computations
(very useful for finer-grained testing), and even my custom result
type (resorting to just tupling). I feel something has been lost by
writing in this style even though part of me secretly likes it.

## An interview with Bryan O'Sullivan

Recently (September 2015), [The Haskell Cast](http://www.haskellcast.com/) interviewed Bryan
O'Sullivan. I highly recommend listening to [the whole thing](
http://www.haskellcast.com/episode/010-bryan-osullivan-on-performance-and-efficiency/). He
had stories to tell about how he got into Haskell, how he ended up
writing all these libraries, and how he goes about designing them and
what his goals are when implementing them. Note that `aeson` and
`text`, which everyone uses, are his creations. Thank you, Bryan, for
all you've done for the Haskell community!

## Lens resources

Gabriel Gonzalez wrote a
[lens tutorial](https://hackage.haskell.org/package/lens-tutorial)
that is useful. Thank you, Gabriel, for writing tutorials not only on
your own libraries, but for others as well!

## Conclusion

For day 4, I presented a tiny example of use of `wreq` with `aeson`
and `lens` to perform a simple task of getting information from the
Web, and tried to make `wreq` more accessible by not requiring use of
`lens` operators up front.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
