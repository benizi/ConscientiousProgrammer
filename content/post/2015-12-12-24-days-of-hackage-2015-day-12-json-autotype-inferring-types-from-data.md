---
categories:
- Haskell
- Hackage
- aeson
- json-autotype
- JSON
- inference
- type providers
- F#
comments: true
date: 2015-12-12T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 12: json-autotype: inferring
types from data"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 12

Today we revisit a problem from
[day 4](/blog/2015/12/04/24-days-of-hackage-2015-day-4-wreq-web-client-programming-with-notes-on-lens-and-operator-syntax/),
in which we grabbed JSON off the Web in order to extract information
about it. I mentioned then that we were using an untyped
representation for JSON. Ideally, if it's not too onerous, we want to
use a typed representation for our data, to avoid common errors such
as trying to access a nonexistent field. The Aeson library allows us
to write our own data types and convert them from and to JSON. If we
were in charge of the data and data format, then this would be the way
to go.

But what if, for whatever reason, we do not already have a typed data
model, but are consuming, for example, JSON from a source that has not
given us a spec of the data types, maybe with JSON Schema? Then we
have to reverse-engineer the data types, possibly from an informal text
spec of the format (which is a real pain).

Or we could take a lazy way out, and *infer* plausible types from a
corpus of representative data. This is what the useful
[`json-autotype`](http://hackage.haskell.org/package/json-autotype)
package does ([documentation on the GitHub repo page](https://github.com/mgajda/json-autotype)).

<!--more-->

## Generating a module of types from JSON data

I saved off a sample JSON document,
`pittsburgh-code-and-supply-events.json` from a query of the Meetup
events API.

The easiest way to use `json-autotype` is to use its command line
tool, installing it globally first.

{{< highlight console >}}
$ stack install json-autotype
{{< /highlight >}}

For our quick example, I just manually generated Haskell source code
(ideally we should integrate this process into a fully automated
build):

{{< highlight console >}}
$ json-autotype pittsburgh-code-and-supply-events.json -o
MeetupEventsJSON.hs
$ mkdir generated-src
$ mv MeetupEventsJSon.hs generated-src/
{{< /highlight >}}

Note that although for this example, we only ran the inference on a
single document, running it on more documents may give more precise
types.

### A quick look at the generated code

Here is a sample of what was generated (reformatted and edited for clarity):

{{< highlight haskell >}}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson.AutoType.Alternative ((:|:))

data TopLevel = TopLevel {
    topLevelResults :: [ResultsElt],
    topLevelMeta :: Meta
  } deriving (Show,Eq,Generic)

data ResultsElt = ResultsElt {
    resultsEltStatus :: Text,
    resultsEltGroup :: Group,
    resultsEltTime :: Int,
    resultsEltWaitlistCount :: Int,
    resultsEltVenue :: (Maybe (Venue:|:[(Maybe Value)])),
    resultsEltCreated :: Int,
    resultsEltUtcOffset :: Int,
    resultsEltEventUrl :: Text,
    resultsEltYesRsvpCount :: Int,
    resultsEltHeadcount :: Int,
    resultsEltFee :: (Maybe (Fee:|:[(Maybe Value)])),
    resultsEltVisibility :: Text,
    resultsEltMaybeRsvpCount :: Int,
    resultsEltName :: Text,
    resultsEltId :: Text,
    resultsEltRsvpLimit :: (Maybe (Int:|:[(Maybe Value)])),
    resultsEltUpdated :: Int,
    resultsEltDuration :: (Maybe (Int:|:[(Maybe Value)])),
    resultsEltDescription :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)

instance FromJSON ResultsElt where
  parseJSON (Object v) = ResultsElt
    <$> v .:   "status"
    <*> v .:   "group"
    <*> v .:   "time"
    <*> v .:   "waitlist_count"
    <*> v .:?? "venue"
    <*> v .:   "created"
    <*> v .:   "utc_offset"
    <*> v .:   "event_url"
    <*> v .:   "yes_rsvp_count"
    <*> v .:   "headcount"
    <*> v .:?? "fee"
    <*> v .:   "visibility"
    <*> v .:   "maybe_rsvp_count"
    <*> v .:   "name"
    <*> v .:   "id"
    <*> v .:?? "rsvp_limit"
    <*> v .:   "updated"
    <*> v .:?? "duration"
    <*> v .:?? "description"
  parseJSON _          = mzero

data Venue = Venue {
    venueRepinned :: Bool,
    venueState :: Text,
    venueCountry :: Text,
    venueZip :: (Maybe (Text:|:[(Maybe Value)])),
    venueLat :: Int,
    venueName :: Text,
    venueCity :: Text,
    venueId :: Int,
    venueLon :: Int,
    venueAddress1 :: Text
  } deriving (Show,Eq,Generic)
{{< /highlight >}}

Isn't this much better than inferring and writing the boilerplate
manually? Now we can parse raw JSON directly into our set of types.

The funny `:|:` operator is an `Either`-like type constructor used to
deal with the fact that when a field is missing from some data
examples, we cannot infer whether it could potentially be a complex
object that we just don't know about from our corpus of examples.

## Using the generated types

Imports:

{{< highlight haskell >}}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module JSONAutoTypeExample where

-- Reuse what we did earlier.
import WreqExample (EventInfo(..), GroupId, getMeetupEventsJSONBytes)

-- Module automatically generated using json-autotype.
import qualified MeetupEventsJSON as Meetup
import qualified Data.Aeson as Aeson
import Control.Arrow ((>>>))
import Data.Aeson.AutoType.Alternative ((:|:)(AltLeft, AltRight))
import qualified Data.Text as Text
{{< /highlight >}}

Our replacement for the old `getMeetupEventInfos`:

{{< highlight haskell >}}
getMeetupEventInfos :: GroupId -> IO (Either String [EventInfo])
getMeetupEventInfos groupId =
  getMeetupEventsJSONBytes groupId
  >>= (Aeson.eitherDecode
       >>> fmap extractEventInfos
       >>> return
      )
{{< /highlight >}}

Note already the differences from before. We are now using
`Aeson.eitherDecode` to turn JSON bytes into a full-fledged
`Meetup.TopLevel` typed data object, and therefore can detect up front
whether the JSON we got was valid (in the sense of, conforms to our
prior inferred types).

Extracting events:

{{< highlight haskell >}}
extractEventInfos :: Meetup.TopLevel -> [EventInfo]
extractEventInfos =
  Meetup.topLevelResults
  >>> map extractEventInfo
{{< /highlight >}}

This is also different from before, in that we are not just using
hardcoded string keys to hopefully find what we want in the JSON
result. We have a `Meetup.TopLevel` object and just use ordinary typed
record access to dive into the data.

Extracting information from a single event:

{{< highlight haskell >}}
extractEventInfo :: Meetup.ResultsElt -> EventInfo
extractEventInfo event =
  EventInfo { eventName = Meetup.resultsEltName event
            , venueName = extractVenueName (Meetup.resultsEltVenue event)
            }
{{< /highlight >}}

Again, we just use the typed record fields from `Meetup.ResultsElt`,
instead of string keys into an object.

Finally, bottoming out as we try to get a venue name from the venue
object of an event:

{{< highlight haskell >}}
-- | Trickier because json-autotype apparently found events without a venue.
extractVenueName :: Maybe (Meetup.Venue :|: [Maybe Aeson.Value]) -> Text.Text
extractVenueName Nothing = ""
extractVenueName (Just (AltLeft venue)) = Meetup.venueName venue
extractVenueName (Just (AltRight jsonValues)) =
  Text.pack ("(unexpected JSON venue: " ++ show jsonValues ++ ")")
{{< /highlight >}}

Here we recognize that maybe there is no venue, or the `venue` JSON
field is not actually a `Venue`. In practice, recognizing this fact
might result in changing our `EventInfo` type so that it doesn't
assume a `Text` as its `venueName`, but for here, for now, we just
stay in the stringly-typed world and try to make to with return a
useful string in all cases.

But you see how having typed JSON can drive rethinking of our original
assumptions leading to our definition of `EventInfo` and maybe improve
our design. The cool thing about `json-autotype` is that you can make
use it without having to write the JSON types by hand.

## Type providers

The F# language includes support for
[type providers](https://msdn.microsoft.com/en-us/library/hh156509.aspx),
which are ways to get types from somewhere without having to write
them yourself. This is a really cool feature I'd like to see put into
more typed language ecosystems. For example, there is a library for
[type providers for Idris](https://github.com/david-christiansen/idris-type-providers).

I'm not aware of how much work there has been on a type provider
ecosystem for Haskell, but I imagine that you could use Template
Haskell, for example, to automate some of what we did here with
`json-autotype`, as a crude form of type provider.

## Conclusion

`json-autotype` is a nice library that helps in making sense of
JSON that comes your way but without an associated set of types
already written for you. It infers types and writes out a Haskell
module for you.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
