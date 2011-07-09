Twitching
=========

Twitching is a simple emacs twitter client to get tweets
from a user's followers.

History
-------

Twitching was written with two primary goals in mind:

1. Get tweets from all followed users without losing any.

2. The ability to filter some tweets from the timeline.

Features
--------

1. Periodically download your timeline and show them in a
buffer.

2. Favorite tweets.

3. Navigate easily through your tweets, use Emacs's `C-s`
and `C-r` to quickly find tweets.

4. Filters -- probably the best feature of twitching.  If
you're really irritated that all your tweets have something
annoying like `#RoyalWedding`, set up a filter that will
remove all tweets with that hashtag and you're twitter feed
is clean.  These are persistent so future tweets will also
be filtered away.

5. Remove -- Same as filters but these aren't persistent.
It just removes tweets that match the filter criteria from
the current list of tweets.

6. Group -- This provides a way to show all your tweets
ordered by the tweeters.  Comes in two flavors: group all
tweets, or group just tweets by a single user.

7. Follow/UnFollow users.

8. Open a hashtag in a browser.  If you see a few tweets
with `#interviewswithhari` and want to see more you can open
it up on the browser.  This is currently not supported as
viewing in emacs.

9. Open a user mention in the browser.  Same as 8, but for
user mentions in tweets.

Missing Features
----------------

Twitching does not provide a way to perform any of Twitter's
"social networking" features.

As is right now, there is no way to compose a tweet, reply
to a tweet or retweet.  There's no way to get tweets where
you're mentioned either.

Contributions to do these are of course welcome.  They
aren't on the TODO list though.

Upcoming Features
-----------------

See the TODO file for details on upcoming features.

Usage
-----

To use twitching, put the files `twitching.el`, `oauth.el`,
`json.el` and `hmac-sha1` in a directory and add the
directory to your emacs load-path.

Before you use twitching, you must have a Twitter consumer
token and consumer token secret.  If you don't already have
one, see below on how to get these.

    ;;; Twitter mode in emacs
    (add-to-list 'load-path "c:/code/elisp/twitching")
    (autoload 'twitching-to-get-my-tweets "twitching" "Start Twitching timer" t)
    (autoload 'twitching-show-favorites "twitching" "show favorite tweets" t)
    (autoload 'twitching-home-timeline-get "twitching" "Get twitter home page" t)

To start getting tweets, do `M-x twitching-to-get-my-tweets`.

Twitching will first ask you to enter your consumer key and
consumer secret.  (See below for details on getting these.)
After this, twitching will open up a browser window which
will ask you to enter your username and password.  Enter it
and authorize your app.  After this, you will be shown a 7
digit number on the browser.  Enter this at the emacs
minibuffer.

#### Getting your own consumer-key and consumer-secret.

To get your own `consumer-key` and `consumer-secret`, go to
<https://dev.twitter.com/apps/new> and register a new
application.

Select the Application Type as a **Client** type and

Default Access Type as at least **Read & Write**.  

**Read only** will also work but favoriting tweets will not
be possible.  Filtering, grouping and other features will
work.
