# twitching, an Emacs client for Twitter

Twitching is an Emacs Twitter client to get tweets from a user's
followers.

## History

Twitching was written with two primary goals in mind:

1. Get tweets from all followed users without losing any.
2. The ability to filter some tweets from the timeline.

## Features

1. Periodically download your timeline and show them in a buffer.
2. Favorite tweets.
3. Navigate easily through your tweets. Use Emacs's `C-s` and `C-r` to
   quickly find tweets.
4. Filters - probably the best feature of twitching.  If you're really
   irritated that all your tweets have something annoying like
   `#RoyalWedding`, set up a filter on that hashtag and your Twitter
   feed is cleared of all tweets with it.  Filters are persistent so
   future tweets that match will also be filtered away.
5. Remove - Same as filters but without persistence.
6. Group - This provides a way to show all your tweets ordered by the
   tweeters.  Comes in two flavors: group all tweets, or group only the
   tweets of a single user.
7. Follow/UnFollow users.
8. Open a hashtag in a browser.  If you see a few tweets with
   [#interviewswithhari](http://twitter.com/#!/search/%23interviewswithhari)
   and want to see more you can open it up on the browser.  This is
   currently not supported as viewing in Emacs.
9. Open a user mention in the browser.  Same as 8, but for user
   mentions in tweets.
10. Copy specific parts of a tweet and put it into the
    `kill-ring`.  For example, `c t` copies the tweet text, and `c s`
    copies the current tweet's status id.

## Missing Features

Twitching does not provide a way to perform any of Twitter's "social
networking" features.

As is right now, there is no way to compose a tweet, reply to a tweet
or retweet. There's no way to get tweets where you're mentioned
either.

Contributions to do these are of course welcome. They aren't on the
TODO list though.

## Upcoming Features

See the TODO file included with the package for details on upcoming
features.

## Usage

Before you use twitching, you must have a Twitter consumer token and
consumer token secret. If you don't already have one, see the section
below on Getting a consumer-key and consumer-secret for details.

### Installation

To use twitching, put the files `twitching.el`, `oauth.el`, `json.el`
and `hmac-sha1.el` in a directory and add this directory to your Emacs
load-path.

Add the below lines to your `.emacs` file.

    ;;; Twitter mode in Emacs
    (add-to-list 'load-path "/path/to/twitching")
    (autoload 'start-twitching "twitching" "Start Twitching timer" t)
    (autoload 'twitching-show-favorites "twitching" "show favorite tweets" t)
    (autoload 'twitching-home-timeline-get "twitching" "Get Twitter home page" t)


### Downloading tweets

#### Downloading your home timeline once

To just download your tweets, do `M-x twitching-home-timeline-get`.
This will work even if the timer is running.

Your tweets will be shown in the buffer `*Twitching*`.

#### Periodically downloading tweets

To periodically fetch your tweets type `M-x start-twitching`.  This
will start a timer that downloads your tweets periodically and put
them in the buffer `*Twitching*`.

The default value of this timer is set to run every 5 minutes, but it
can be customized by changing the value of
`*twitching-timer-interval*`. Simply add the following line to your
`.emacs` file.

    (setq *twitching-timer-interval* SECONDS)

where `SECONDS` is the new interval period.  If you have already
started the timer, you will need to stop it and start it again for the
new interval to take effect.

##### Stopping the timer

`M-x stop-twitching` stops the timer.

#### Startup

The first time that you start twitching, it will first ask you to
enter your Twitter consumer key and Twitter consumer secret.  (See
below for details on getting these.)  After this, twitching will open
up a browser window which will ask you to enter your username and
password.  Enter it and authorize your app.  After this, you will be
shown a 7 digit number on the browser.  Enter this at the Emacs
minibuffer and your tweets will get downloaded.

This will happen only the first time that twitching starts.

If you want to stop entering the consumer key and consumer secret each
time that you start Emacs, you can add them to your start up script
using the following commands

    (setq *twitching-api-consumer-key* "YOUR CONSUMER KEY"
          *twitching-api-consumer-secret* "YOUR CONSUMER SECRET")

### Navigation

You can go to the next tweet by using any of the following keys:
`C-n`, `n`, down-arrow or `j`.  To nagivate to the previous tweet, you
can use any of the keys: `C-p`, `p`, up-arrow or `k`.  These commands
support prefix arguments, so `C-u 5 j` will move down five tweets.

To scroll down a page, you can use `C-v`, SPC or Page Down.  You can
scroll up a page using `M-v`, Backspace, or Page Up.

(Quick note: Emacs defines two commands `scroll-up` and `scroll-down`
which do the exact inverse of what Page Up and Page Down do. For
Emacs, the command `scroll-up` scrolls _contents_ up but Page Up
scrolls the cursor up. Here the terms "scroll up" and "scroll down",
refer to the common meanings associated with Page Up and Page Down
respectively.)

#### Difference with Twitter Web interface

In Twitter's web interface, newer tweets are shown above older ones.
In twitching older tweets are shown first with new ones appended to
the buffer.

### Favoriting

Tweets can be favorited by typing `s` on a tweet.  If a tweet is
favorited, it will have a star shown next to it.  If `s` is pressed on
an already favorited tweet, twitching will unfavorite the tweet.

### Favorited tweets

To view all your favorited tweets, use the command
`twitching-show-favorites`.  Upon invocation, your favorite tweets are
downloaded and shown in the buffer `*Favorite Tweets*`.  With a prefix
argument, the command switches to the buffer immediately after it has
been rendered.

### Opening links, hashtags and mentions

####  Opening urls

To open a link on a tweet, move point to it and press `o`.  If there
are multiple links in a tweet they can be opened with a prefix
argument.  For instance, to open the 3rd link in a tweet, type `C-u 3
o` or, more simply, `C-3 o`.

To open all links in a tweet, simply type `O`.  The links will open up
in your default browser.

#### Opening hashtags

Opening hashtags work the same way as opening links.  To open the first
hashtag in a tweet, use the key binding `#` and to open the _nth_
hashtag, use a prefix argument as `C-u` _n_ `#`.

#### Opening user mentions

Opening user mentions work the same way as opening links.  To open the
first user mention in a tweet, use the key binding `@` and to open the
_nth_ hashtag, use a prefix argument as `C-u` _n_ `@`.

### Getting a consumer-key and consumer-secret

To get your own `consumer-key` and `consumer-secret`, go to
[https://dev.twitter.com/apps/new](https://dev.twitter.com/apps/new) and
register a new application.

Select **Application Type** as **Client** and **Default Access Type**
as **Read & Write**.  **Read only** will also work but favoriting
tweets will not be possible.  Filtering, grouping and other features
will work.

#### Why such an elaborate route?

If you write a Twitter client, you're expected to encode the
consumer-key and consumer-secret within your application in such a way
that your users do not have access to them.  Obviously, this does not
work well with Free Software programs.

## Displaying User Profile images

To enable display of user profile pictures along with their tweets,
add the following line to your `.emacs` file after the customizations
mentioned in the section Usage.

    (setq *twitching-profile-use-p* t)

Twitching will lazily download the user profile images and store them
in your filesystem.  So immediately after enabling this feature, your
twitching buffer will still show your tweets without any profile
images.  The images will show up in time.

The profile images are stored in the directory specified by the
variable `*twitching-profile-directory*`.  By default, it evaluates to
`~/.emacs.d/twitching/thumbnails`.

### Lack of image support in Windows

By default, Emacs does not ship with support for JPEG, PNG or GIF
formats.  See the answer to the question "3.3 How do I get image
support?" in the
[Emacs FAQ for Windows](http://www.gnu.org/software/emacs/windows/Installing-Emacs.html).

To enable image support on Windows, I have found
[GnuWin32](http://gnuwin32.sourceforge.net/) useful.  I downloaded the
following packages

  * [GifLib](http://gnuwin32.sourceforge.net/packages/giflib.htm) -
    support for GIF files
  * [Jpeg](http://gnuwin32.sourceforge.net/packages/jpeg.htm) -
    support for JPEG files
  * [LibPng](http://gnuwin32.sourceforge.net/packages/libpng.htm) -
    support for PNG files. This has a dependency on ZLib. The page has
    a link to its dependencies.

Extracted the contents of the above packages and place them in your
`EMACS_HOME/bin` folder. The next time you start a new instance of
Emacs, it should have support to display images.

To check if your Emacs supports images, you can run the following from
Emacs. Type `M-:` and paste the following in the minibuffer
`(image-type-available-p 'jpeg)`. If you see a `t` printed, it means
you have support to display JPEG files. The corresponding commands for
GIF and PNG are `(image-type-available-p 'gif)` and
`(image-type-available-p 'png)` respectively.

## License

`twitching` is licensed under the terms of the GPLv3 (or later).  This
means you're free to use it for personal and commercial use, you're
free to copy it and give copies away, and you're free to modify and
redesign the source code, and to create derivative works.  For
details, read the contents of the file LICENSE distributed with
`twitching` or visit
[http://www.gnu.org/license s/gpl-3.0.html](http://www.gnu.org/licenses/gpl-3.0.html).
