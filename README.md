Twitching
=========

Twitching is a simple Emacs Twitter client to get tweets
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
remove all tweets with that hashtag and your Twitter feed is
clean.  Filters are persistent so future tweets that match
will also be filtered away.

5. Remove -- Same as filters but these aren't persistent.

6. Group -- This provides a way to show all your tweets
ordered by the tweeters.  Comes in two flavors: group all
tweets, or group only the tweets of a single user.

7. Follow/UnFollow users.

8. Open a hashtag in a browser.  If you see a few tweets
with [`#interviewswithhari`][interviewswithhari] and want to
see more you can open it up on the browser.  This is
currently not supported as viewing in Emacs.

9. Open a user mention in the browser.  Same as 8, but for
user mentions in tweets.

10. Copy specific parts of a tweet and put it into the
`kill-ring`.  For example, `c t` copies the tweet text, and
`c s` copies the current tweet's status id.

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
directory to your Emacs load-path.

Before you use twitching, you must have a Twitter consumer
token and consumer token secret.  If you don't already have
one, see below on how to get these.

    ;;; Twitter mode in Emacs
    (add-to-list 'load-path "c:/code/elisp/twitching")
    (autoload 'twitching-to-get-my-tweets "twitching" "Start Twitching timer" t)
    (autoload 'twitching-show-favorites "twitching" "show favorite tweets" t)
    (autoload 'twitching-home-timeline-get "twitching" "Get Twitter home page" t)

To start getting tweets, do `M-x twitching-to-get-my-tweets`.

Twitching will first ask you to enter your consumer key and
consumer secret.  (See below for details on getting these.)
After this, twitching will open up a browser window which
will ask you to enter your username and password.  Enter it
and authorize your app.  After this, you will be shown a 7
digit number on the browser.  Enter this at the Emacs
minibuffer.

### Getting your own consumer-key and consumer-secret.

To get your own `consumer-key` and `consumer-secret`, go to
<https://dev.twitter.com/apps/new> and register a new
application.

Select the _"Application Type"_ as a **Client** type and
_"Default Access Type"_ as at least **Read & Write**.
**Read only** will also work but favoriting tweets will not
be possible.  Filtering, grouping and other features will
work.

Displaying User Profile images
------------------------------

To enable display of user profile pictures along with their
tweets, add the following line to your `.emacs` file after
the customizations mentioned in the section Usage.

    ;;; Show user's profile pictures with their tweets.
    (setq *twitching-profile-use-p* t)

Twitching will lazily download the user profile images and
store them in your filesystem.  So immediately after
enabling this feature, your twitching buffer will still show
your tweets without any profile images.  The images will
show up in time.

The profile images are stored in the directory specified by
the variable `*twitching-profile-directory*`.  By default,
it evaluates to `"~/.emacs.d/twitching/thumbnails"`.

### Lack of image support in Windows

By default, Emacs does not ship with support for JPEG, PNG
or GIF formats.  See the answer to the question "3.3 How do
I get image support?" in the [Emacs FAQ for
Windows][EmacsFAQ].

To enable image support on Windows, I have found
[GnuWin32][GnuWin32] useful.  Install GnuWin32 and add its
installation path to your `PATH` environment variable.
Emacs picks it up and images show up properly.

[interviewswithhari]: http://twitter.com/#!/search/%23interviewswithhari "Tweets on Johann Hari's interviewing style"
[EmacsFAQ]: http://www.gnu.org/software/emacs/windows/Installing-Emacs.html
[GnuWin32]: http://gnuwin32.sourceforge.net/
