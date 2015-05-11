---
layout: post
title: "We should not create this kind of terrible error message for the end user"
disqus_identifier: "http://ConscientiousProgrammer.com/blog/2013/06/26/we-should-not-create-this-kind-of-terrible-error-message-for-the-end-user/"
disqus_url: "http://ConscientiousProgrammer.com/blog/2013/06/26/we-should-not-create-this-kind-of-terrible-error-message-for-the-end-user/"
date: 2013-06-26T23:44:00
comments: true
categories: 
- error messages
- error handling
- end user
- Android
- human-centered design
- UX
---
I have a [Samsung Galaxy S II smartphone](http://franklinchen.com/blog/2012/03/22/paradox-i-will-observe-the-national-day-of-unplugging-but-just-bought-my-first-smartphone-this-week/). I recently encountered a problem: auto-updating of apps failed with this terrible error message:

<blockquote>
Application cannot be installed in the default install location.
</blockquote>

Also, a lot of time and bandwidth was each time spent on re-downloading the app and then failing, with no way to gracefully recover.

If you search the Web for this unpleasant error message, you will see hundreds if not thousands of confused end users like me asking on various forums how to deal with this.

I feel pretty bad about this situation, both as a programmer and as an end user.

<!--more-->

## As an end user

As an end user, I know very little about smartphones. I am not currently a mobile-device developer. I just want things to work, and I have never read the huge manual that came with my phone. If there is an error in my operation of the phone, I want some kind of useful guidance (ideally very specific and tailored to a common use case known by the vendor), not a mysterious generic error message.

## As a programmer

It turns out that there is internal device storage and SD card storage, and if you run out of space somewhere, an error can happen, and that there is a default install location for apps. The gory details for developers are [here](https://developer.android.com/guide/topics/data/install-location.html).

Clearing some space and also moving a large app to SD card storage solved my problem.

So why that terrible error message? Why not just tell the end user exactly what is going on? Why not something like:

<blockquote>
The application X could not be installed at (device storage | SD card storage) because you don't have enough space. You can try clearing more space, or go to your application settings to move it to (SD card storage | device storage).
</blockquote>

The application should know exactly what is going on, and not only that, should be able to suggest a course of action based on all the relevant variables, such as how much space there is in different places, how large the application is, etc.

### Why?

I haven't seen the source code that generates the existing error message, but as a programmer, I can guess what might be happening.

I have been as guilty as anyone else of not providing good end user error messages in software I have written. It is very easy to do the following sloppy things instead:

- Throwing exceptions that contain no useful information except some string
- Not handling an exception close to where useful recovery can actually happen

It is superficially "more work" to throw an exception that includes all relevant information, and for handlers to also do something intelligent rather than just pass the buck all the way to some top level catch-all handler.

Also, why should the phone download the whole app and then fail, if there is actually enough information to guess that the whole update might fail? Surely there is information about how big the app is, before downloading it?

And what's with the default concurrent downloads of many updates at the same time, which greatly increases the probability of failure of all of them, when serializing the downloads results in less use of temporary space?

## Conclusion

We programmers clearly are not conscientious enough about error handling and reporting. I believe that today, more than ever, we have a responsibility to *handle failure gracefully* in software. I believe that failure should even be considered to be the *default* expectation, rather than success, in order to create reliable software. As [Dick Wall said at the recent 2013 Pittsburgh TechFest](http://franklinchen.com/blog/2013/06/01/report-on-the-second-pittsburgh-techfest-2013/), it's not enough to design and code and test primarily for the "happy path".

There are ways to do error handling better, but they require serious thinking up front. This is a large topic we will be exploring in depth on this blog.

### (Update of 2013-06-27)

By sheer coincidence, a great [interview on *human-centered design*](http://programming.oreilly.com/2013/06/human-centered-design-may-be-what-makes-your-app-stand-out.html) arrived in my RSS feed. Check it out.

I will be the first to admit that I don't know much about UX or human-centered design. I just have some intuitions as an end user. I think we developers should learn more about this stuff in a more formal, serious way. And it's just a matter of thinking about the ultimate end user. Even if we are not programming for the ultimate end user, we are at least programming for other programmers. If we are writing an API, the principles that guide human behavior must still be valid, even if programmers are an unusual subset of human beings! We write code *not* primarily for computers but for other human beings (including our future selves).
