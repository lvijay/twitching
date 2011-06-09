; -*- mode: emacs-lisp; paredit-mode: t; -*-
;;; twitching.el --- Twitter client library.
;;;
;;; Copyright (C) 2011 Vijay Lakshminarayanan
;;;
;;; Author: Vijay Lakshminarayanan <laksvij AT gmail.com>
;;; Version: 0.3.0
;;; Created: Thu May 19 18:49:23 2011 +0530
;;; Keywords: twitter
;;; Contributors:
;;;
;;; This file is NOT part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to the
;;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.

;;; Code:


;;; dependencies
(require 'cl)
(require 'url)
(require 'url-util)
(require 'hmac-sha1)
(require 'oauth)
(require 'json)
(require 'browse-url)


;;; Twitter constants
(defconst +twitter-oauth-request-url+
  "http://api.twitter.com/oauth/request_token"
  "Twitter's end point for the request token.")

(defconst +twitter-oauth-access-url+
  "https://api.twitter.com/oauth/access_token"
  "Twitter's access token end point.")

(defconst +twitter-oauth-authorize-url+
  "https://api.twitter.com/oauth/authorize"
  "The endpoint for the authorization url.")


;;; Main interface functions
(defvar *twitching-timer* nil
  "The timer object that keeps getting tweets.")

(defvar *twitching-timer-interval* 300
  "Interval, in seconds, between fetching the twitter home
timeline.")

;;;###autoload
(defun twitching-to-get-my-tweets ()
  "Start timer to fetch home timeline."
  (interactive)
  (labels ((mintos (min) (print (* min 60))))
    (if *twitching-timer*
        (message "Timer already running.")
      (setq *twitching-timer*
            (run-with-timer 0
                            *twitching-timer-interval*
                            #'twitching-home-timeline-get)))))

;;;###autoload
(defun twitching-home-timeline-get ()
  "Fetch home timeline."
  (interactive)
  (let ((buffer (get-twitching-buffer))
        (tweets (twitching-get-home-timeline)))
    (twitching-write-tweets tweets buffer))
  (message "retrieved tweets"))

;;;###autoload
(defun twitching-show-favorites ()
  "Show favorited tweets"
  (interactive)
  (let ((buffer (get-twitching-favorites-buffer))
        (tweets (twitching-get-favorites)))
    (twitching-write-tweets tweets buffer))
  (message "retrieved favorites"))

(defun twitching-write-tweets (tweets buffer)
  (let (starting-point
        ending-point)
    (with-twitching-buffer buffer
      (save-excursion
        (setq starting-point (point-max))
        (goto-char starting-point)
        (mapc (lambda (tweet)
                (let ((tweet (format "%S\n" tweet)))
                  (insert tweet)))
              tweets)
        (setq ending-point (point))))
    (with-twitching-buffer buffer
      (save-excursion
        (twitching-render-region starting-point ending-point buffer)))))

;;;###autoload
(defun twitching-stop ()
  "Stop the timer that fetches tweets."
  (interactive)
  (if *twitching-timer*
    (progn
      (cancel-timer *twitching-timer*)
      (setq *twitching-timer* nil)
      (message "Stopped twitching timer."))
    (message "twitching timer not running.")))

(defun get-twitching-buffer ()
  (get-buffer-create "*Twitching*"))

(defun get-twitching-favorites-buffer ()
  (get-buffer-create "*Favorite Tweets*"))


;;; struct definitions
(defmacro twitching-defstruct (struct-name &rest fields)
  "Macro that defines structures representing twitter responses.
STRUCT-NAME is the name of the represented object.

FIELDS is a list of triples of the form (json-object-field-name
struct-field-name key-function).  key-function is optional and is
IDENTITY by default.  If provided, it must be a function that
takes one argument and returns the object representation."
  (declare (indent 1))
  (let ((constructor-name (intern (concat "make-" (symbol-name struct-name))))
        (new-fun-name (intern (concat "new-" (symbol-name struct-name)))))
    `(progn
       (defstruct (,struct-name (:type vector) :named)
         .,(mapcar (lambda (field-specs)
                     (let ((fld-name (cadr field-specs)))
                       fld-name))
                   fields))
       (defun ,new-fun-name (json-object)
         ,(mapconcat (lambda (x) (format "%s" x))
                     `("Create a new instance of"
                       ,struct-name "from JSON-OBJECT.")
                     " ")
         (flet ((preprop (x)
                  (if (stringp x)
                      (replace-regexp-in-string "[\r\n]" " " x)
                    x)))
           (,constructor-name
            .,(mapcan (lambda (field)
                        (let* ((json-field (first field))
                               (symbol-name (symbol-name (second field)))
                               (keyword-field (intern (concat ":" symbol-name)))
                               (struct-field keyword-field)
                               (form `(cdr (assoc ',json-field json-object)))
                               (key-fn (third field)))
                          (if key-fn
                              `(,struct-field (preprop (funcall ,key-fn ,form)))
                              `(,struct-field (preprop ,form)))))
                      fields)))))))

;;; Defines a twitter-user
(twitching-defstruct twitching-user
  (created_at created-at)
  (geo_enabled geo-enabled-p)
  (verified verifiedp)
  (following followingp)
  (contributors_enabled contributors-enabled-p)
  (url url)
  (location location)
  (id_str id)
  (lang lang)
  (listed_count listed-count)
  (description description)
  (screen_name screen-name)
  (utc_offset utc-offset)
  (notifications notificationsp)
  (favourites_count favorites-count)
  (statuses_count statuses-count)
  (is_translator translatorp)
  (name name)
  (profile_image_url profile-image-url)
  (friends_count friends-count)
  (time_zone time-zone)
  (follow_request_sent follow-request-sent-p)
  (protected protectedp)
  (followers_count followers-count))

;;; Defines a twitter-entity
(twitching-defstruct twitching-entity
  (hashtags hashtags)
  (user_mentions mentions)
  (urls urls))

;;; Defines a twitter-status
(twitching-defstruct twitching-status
  (created_at created-at)
  (retweeted retweetedp)
  (contributors contributors)
  (in_reply_to_status_id_str in-reply-to-status-id)
  (source source)
  (in_reply_to_screen_name in-reply-to-screen-name)
  (retweet_count retweet-count)
  (favorited favoritedp)
  (place place)
  (id_str id)
  (entities entities #'new-twitching-entity)
  (text text)
  (truncated truncatedp)
  (user user #'new-twitching-user)
  (geo geo)
  (coordinates coordinates))


;;; Rendering section
(defvar *twitching-screen-name-category*
  (put '*twitching-screen-name-category* 'face '((:weight bold)
                                                 (:background "MidnightBlue")
                                                 (:foreground "gold"))))

(defvar *twitching-user-name-category*
  (put '*twitching-user-name-category* 'face '((:weight bold)
                                               (:background "MidnightBlue")
                                               (:foreground "gold"))))

(defvar *twitching-timestamp-category*
  (put '*twitching-timestamp-category* 'face '((:background "MidnightBlue")
                                               (:foreground "gold")
                                               (:weight bold))))

(defvar *twitching-separator-category*
  (put '*twitching-separator-category* 'face '((:background "MidnightBlue")
                                               (:foreground "white")
                                               (:weight bold))))

(defvar *twitching-separator*
  (propertize " | " 'category '*twitching-separator-category*))

(defvar *twitching-plaintext-category*
  (put '*twitching-plaintext-category* 'face '((:background "white")
                                               (:foreground "black"))))

(defvar *twitching-hashtags-category*
  (put '*twitching-hashtags-category* 'face '((:background "ivory3")
                                             (:foreground "firebrick")
                                             (:slant italic))))

(defvar *twitching-mentions-category*
  (put '*twitching-mentions-category* 'face '((:background "thistle2")
                                              (:foreground "DarkGreen")
                                              (:weight bold))))

(defvar *twitching-urls-category*
  (put '*twitching-urls-category* 'face '((:background "white")
                                          (:foreground "blue1")
                                          (:slant italic)
                                          (:underline t))))

(defvar *twitching-star-category*
  (put '*twitching-star-category* 'face '((:background "yellow"))))

(defvar *twitching-star* (propertize " \x2605\  "
                                     'category '*twitching-star-category*)
  "String used to represent starred tweets.")

(defvar *twitching-retweet*
  (propertize "\x27F3\ " 'face '((:background "GreenYellow")))
  "String used to represent retweeted tweets.")

(defvar *twitching-newline* (propertize "\n"
                                        'face '((:background "white")
                                                (:foreground "white"))))

(defvar *twitching-fill-column* 70 "Set this to manipulate `fill-column'.")

(defun twitching-render-region (start end buffer)
  "Renders the region in START and END in BUFFER.  Reads
`twitching-status'es in the region and renders each of them."
  (when (> start end) (rotatef start end))
  (with-twitching-buffer buffer
    (let ((string (buffer-substring-no-properties start end))
          result
          (read-start 0))
      (while (ignore-errors (setq result (read-from-string string read-start)))
        (let* ((status (car result))
               (read-end (cdr result))
               (line1 (twitching-decorate-title-text status))
               (line2 (twitching-decorate-status-text status))
               (newline *twitching-newline*)
               (display (concat line1 newline line2 newline))
               (buf-start (+ start read-start))
               (buf-end (+ start read-end)))
          (set-text-properties buf-start buf-end nil buffer)
          (put-text-property buf-start buf-end 'display display)
          (put-text-property buf-start buf-end 'tweet status)
          (setq read-start read-end))))))

(defun twitching-decorate-title-text (status)
  (let* ((created-at (twitching-status-created-at status))
         (created-at (format-time-string "%a %b %d %H:%M:%S %z %Y"
                                         (date-to-time created-at)))
         (user (twitching-status-user status))
         (screen-name (twitching-user-screen-name user))
         (user-name (twitching-user-name user))
         (favoritedp (twitching-status-favoritedp status))
         (retweetedp (twitching-status-retweetedp status))
         (sep *twitching-separator*)
         (screen-name (propertize screen-name
                                  'category '*twitching-screen-name-category*))
         (user-name (propertize user-name
                                'category '*twitching-user-name-category*))
         (created-at (propertize created-at
                                 'category '*twitching-timestamp-category*))
         (line (concat screen-name sep user-name sep created-at)))
    (when favoritedp (setq line (concat line *twitching-star*)))
    (when retweetedp (setq line (concat *twitching-retweet* sep line)))
    line))

(defun twitching-decorate-status-text (status)
  "Decorates the status text."
  (let* ((text (twitching-status-text status))
         (entity (twitching-status-entities status))
         (urls (twitching-entity-urls entity))
         (mentions (twitching-entity-mentions entity))
         (hashtags (twitching-entity-hashtags entity))
         (properties '((hashtag . *twitching-hashtags-category*)
                       (mention . *twitching-mentions-category*)
                       (url . *twitching-urls-category*)
                       (text . *twitching-plaintext-category*)))
         (fn (lambda (list type)
               (mapcar (lambda (x) `(,@(cdr (assoc 'indices x)) ,type)) list)))
         (hashtags (funcall fn hashtags 'hashtag))
         (mentions (funcall fn mentions 'mention))
         (urls (funcall fn urls 'url))
         (indices (sort (append hashtags mentions urls)
                        (lambda (idx1 idx2)
                          (< (second idx1) (second idx2)))))
         (max (length text))
         (ci 0)
         result)
    (dolist (idx indices result)
      (when (/= ci (car idx)) (push `(,ci ,(car idx) text) result))
      (push idx result)
      (setq ci (cadr idx)))
    (when (> max ci) (push (list ci max 'text) result))
    (setq result
          (mapcar (lambda (idx)
                    (let* ((txt (substring text (first idx) (second idx)))
                           (type (third idx)))
                      (propertize txt 'category (cdr (assoc type properties)))))
                  (reverse result)))
    (with-temp-buffer
      (insert (apply #'concat result))
      (let ((fill-column *twitching-fill-column*))
        (fill-region (point-min) (point-max)))
      (buffer-string))))

(defun find-twitching-status (status buffer)
  "Returns point at which STATUS is found in BUFFER or nil if it
could not be found.

Assumes that BUFFER is already rendered."
  (let ((p (point-min))
        (id (twitching-status-id status))
        (continuep 't))
    (block nil
      (while continuep
        (setq status (get-text-property p 'tweet buffer))
        (when (equal id (twitching-status-id status))
          (return p))
        (setq p (next-single-property-change p 'tweet buffer)
              continuep p)))))

(defun twitching-rerender-tweet (tweet &optional point buffer)
  "Re-renders TWEET at POINT in BUFFER.  If POINT is nil, it is
searched for in BUFFER.  If POINT is not nil and the
twitching-status under POINT does not equal TWEET, returns nil.
If BUFFER is not provided, `(current-buffer) is assumed. "
  (when (not point) (setq point (find-twitching-status tweet buffer)))
  (let* ((status (get-text-property point 'tweet buffer))
         (id (twitching-status-id tweet))
         (p point))
    ;; since we're using text properties that, unlike overlays, do not
    ;; store their bounds, we need to do extra jugglery to find bounds
    ;; of a tweet.
    ;;
    ;; pictorially:
    ;;
    ;; +------------+ +------------+ +----------+
    ;; | prev tweet | | this tweet | |next tweet|
    ;; +------------+ +------------+ +----------+
    ;; a            b lb    p     ub e          f
    ;;
    ;; We have the current point, p, and need to find the bounds of
    ;; `this tweet', (lb ub).  b is ending point of `prev tweet' and e
    ;; is the starting point of `next tweet'.  There could be
    ;; whitespace between (b c) and (d e).
    ;;
    ;; We calculate c = (n-s-p-c (p-s-p-c p 'tweet) 'tweet) and
    ;; calculate d = (n-s-p-c p 'tweet), while taking boundary
    ;; conditions into consideration.
    (if (and (twitching-status-p status)
             (equal id (twitching-status-id status)))
        (let* ((point-max (point-max))
               (b (previous-single-property-change p 'tweet buffer))
               (prev-tweet (get-text-property b 'tweet buffer))
               ;; Below check needed because if point, `p' was in the
               ;; middle of a tweet when this function is called, then
               ;; b originally evalutes to `lb' and then `lb'
               ;; evaluates into `e'.
               (b (if (eq tweet prev-tweet)
                      (previous-single-property-change b 'tweet buffer)
                    b))
               (lb (if b
                       (next-single-property-change b 'tweet buffer point-max)
                     (point-min)))
               (ub (next-single-property-change p 'tweet buffer
                                                point-max)))
          (with-twitching-buffer buffer
            (delete-region lb ub)
            (goto-char lb)
            (save-excursion
              (let* ((text (format "%S\n" tweet))
                     (length (length text))
                     (ub (+ lb length)))
                (insert text)
                (set-text-properties lb ub 'nil buffer)
                (twitching-render-region b (point-max) buffer)))))
      nil)))


;;; Define `twitching-mode'
(defvar twitching-mode-hook '()
  "Mode hook.")

;; keymap
(defvar twitching-mode-map
  (let ((keymap (make-sparse-keymap))
        (nodigits t))
    ;(suppress-keymap keymap nodigits)
    (define-key keymap (kbd "n")      'twitching-next-tweet)
    (define-key keymap (kbd "C-n")    'twitching-next-tweet)
    (define-key keymap (kbd "j")      'twitching-next-tweet)
    (define-key keymap (kbd "<down>") 'twitching-next-tweet)
    (define-key keymap (kbd "p")    'twitching-prev-tweet)
    (define-key keymap (kbd "C-p")  'twitching-prev-tweet)
    (define-key keymap (kbd "<up>") 'twitching-prev-tweet)
    (define-key keymap (kbd "k")    'twitching-prev-tweet)
    (define-key keymap (kbd "f") 'twitching-create-filter)
    (define-key keymap (kbd "s") 'twitching-favorite-tweet)
    (define-key keymap (kbd "o") 'twitching-open-link)
    (define-key keymap (kbd "O") 'twitching-open-all-links)
    (define-key keymap (kbd "q") 'bury-buffer)
    (define-key keymap (kbd "SPC") 'scroll-up)
    (define-key keymap (kbd "<backspace>") 'scroll-down)
    (define-key keymap (kbd "#") 'twitching-open-hashtag)
    (define-key keymap (kbd "@") 'twitching-open-mention)
    keymap))

(define-derived-mode twitching-mode nil "Twitching"
  "Major mode for viewing tweets. \\{twitching-mode-map}")

(defmacro with-twitching-buffer (buffer &rest body)
  (declare (indent 1))
  `(with-current-buffer ,buffer
     (twitching-mode)
     (toggle-read-only -1)
     (unwind-protect 
         ,@body
       (toggle-read-only +1))))


;;; Mode interactive functions
(defun twitching-next-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (let* ((plusp (plusp n))
         direction
         limit
         (n (abs n))
         (buffer (current-buffer))
         (point (point)))
    (if plusp
        (setq direction #'next-single-property-change
              limit (point-max))
        (setq direction #'previous-single-property-change
              limit (point-min)))
    (dotimes (i n)
      (setq point (funcall direction point 'tweet buffer limit)))
    (goto-char point)))

(defun twitching-prev-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (twitching-next-tweet (- n)))

(defun twitching-favorite-tweet (point)
  "Favorite or unfavorite the tweet at POINT."
  (interactive "d")
  (let* ((tweet (get-text-property point 'tweet))
         (buffer (current-buffer))
         (tweet (twitching-star-tweet tweet)))
    (if tweet
        (twitching-rerender-tweet tweet point buffer)
      (message "No tweet at point."))))

(defun twitching-create-filter ()
  "Create a twitter filter."
  (interactive))

(defun twitching-open-link (n)
  "Open the N th url in tweet.  Ignored if tweet has no urls.
Counting starts at 1."
  (interactive "p")
  (unless (twitching-open-entity (point) #'twitching-entity-urls
                                 'url n nil)
    (message "No URLs in this tweet.")))

(defun twitching-open-all-links ()
  "Open all links in the tweet under point.  Ignored if there are
no URLs in tweet."
  (interactive)
  (let ((count 1))
    (while (twitching-open-entity (point) #'twitching-entity-urls
                                  'url count nil)
      (setq count (1+ count)))))

(defun twitching-open-mention (n)
  "Open the mentioned twitter user in the tweet under point.
With a prefix argument, opens the N th user in the tweet.
Ignored if no users are mentioned in the tweet.  Counting starts
at 1."
  (interactive "p")
  (unless (twitching-open-entity (point) #'twitching-entity-mentions
                                 'screen_name n
                                 "https://twitter.com/#!/")
    (message "No mention in tweet.")))

(defun twitching-open-hashtag (n)
  "Open the hashtag in the tweet under point.
With a prefix argument, opens the N th hashtag in the tweet.
Ignored if no hashtags are mentioned in tweet.  Counting starts
at 1."
  (interactive "p")
  (unless (twitching-open-entity (point) #'twitching-entity-hashtags
                                 'text n
                                 "https://twitter.com/#!/search?q=%23")
    (message "No hashtag in tweet.")))

(defun twitching-open-entity (point entity-elem-fn key n url-prefix)
  (let ((status (get-text-property point 'tweet)))
    (if status
        (let* ((entities (twitching-status-entities status))
               (elems (funcall entity-elem-fn entities))
               (elem (nth (1- n) elems))
               (elem (cdr (assoc key elem)))
               (url (concat url-prefix elem)))
          (when elem
            (funcall browse-url-browser-function url)))
      (message "No tweet at point."))))


;;; Twitter API interactions
(defvar *twitching-user-dir*
  (expand-file-name (convert-standard-filename "~/.emacs.d/twitching"))
  "Name of the directory where the user's tweets are stored.")

(defvar *twitching-consumer-key* nil "Twitter consumer key.")

(defvar *twitching-consumer-secret* nil "Twitter consumer secret.")

(defvar *twitching-access-token* nil "Twitter access key.")

(defvar *twitching-access-token-secret* nil "Twitter access token secret.")

(defvar *twitching-oauth-access-token* nil "OAuth access token.")

(defvar *twitching-since-id* nil "Last status-id received from twitter.")

(defvar *twitching-favorites-since-id* nil "Last status-id received from twitter.")

(defvar *twitching-count* nil "Number of tweets to fetch.")

(defvar *twitching-page-number* nil "Page number to fetch.")

(defvar *twitching-page-limit* nil "Maximum number of pages to fetch.")

(defvar *twitching-include-entities* t "sets include_entities to 1 or 0")

(defun twitching-get-home-timeline ()
  "Gets the current user's home timeline as a list of
`twitching-status'es."
  (twitching-check-keys)
  (let* ((url "http://api.twitter.com/1/statuses/home_timeline.json")
         (*twitching-count* 200)
         (*twitching-page-limit* 10)
         (result (twitching-keep-getting-statuses url t)))
    (when result
      (setq *twitching-since-id* (cdr result)))
    (car result)))

(defun twitching-get-favorites ()
  (twitching-check-keys)
  (let* ((url "http://api.twitter.com/1/favorites.json")
         (*twitching-count* 'nil)       ; unused for favorites
         (*twitching-since-id* *twitching-favorites-since-id*)
         (result (twitching-keep-getting-statuses url t)))
    (when result
      (setq *twitching-favorites-since-id* (cdr result)))
    (car result)))

(defun twitching-keep-getting-statuses (url &optional fullyp)
  "Continually makes GET calls on URL by incrementing the page
number to fetch.  If FULLYP and `*twitching-since-id*' are nil,
however, it does this only once.  Returns the cons (statuses
. since_id) where since_id is the highest since_id in the
response or nil if there was no response."
  (let ((page 1)
        (continuep 't)
        (count (or *twitching-count* 20))
        statuses)
    (if (and (not *twitching-since-id*) (not fullyp))
        (setq statuses (twitching-get-statuses url (twitching-get-params)))
      (progn
        (while continuep
          (let* ((*twitching-page-number* page)
                 (params (twitching-get-params))
                 (stats (twitching-get-statuses url params "GET")))
            (if stats
                (setq statuses (append stats statuses)
                      page (1+ page))
              (setq continuep 'nil))
            (if (and (numberp *twitching-page-limit*)
                     (>= page *twitching-page-limit*))
                (setq continuep 'nil))))))
    (when statuses
      (let ((highest (reduce (lambda (x y) (if (string-lessp x y) y x))
                             statuses :key #'twitching-status-id)))
        (cons statuses highest)))))

(defun twitching-check-keys ()
  "Checks if `*twitching-consumer-key*'
`*twitching-consumer-secret*' `*twitching-access-token*'
`*twitching-access-token-secret*' have been set.  Requests user
input if they haven't.

If `*twitching-access-token*' `*twitching-access-token-secret*'."
  (unless *twitching-consumer-key*
    (setq *twitching-consumer-key*
          (read-string "Enter consumer key: ")))
  (unless *twitching-consumer-secret*
    (setq *twitching-consumer-secret*
          (read-string "Enter consumer secret: ")))
  (unless (or *twitching-access-token* *twitching-access-token-secret*)
    (let* ((oauth-enable-browse-url t)
           response)
      (setq response (oauth-authorize-app *twitching-consumer-key*
                                          *twitching-consumer-secret*
                                          +twitter-oauth-request-url+
                                          +twitter-oauth-access-url+
                                          +twitter-oauth-authorize-url+))
      (setq *twitching-access-token*
            (oauth-t-token (oauth-access-token-auth-t response))
            *twitching-access-token-secret*
            (oauth-t-token-secret (oauth-access-token-auth-t response)))))
  (setq *twitching-oauth-access-token*
        (make-oauth-access-token
         :consumer-key *twitching-consumer-key*
         :consumer-secret *twitching-consumer-secret*
         :auth-t (make-oauth-t
                  :token *twitching-access-token*
                  :token-secret *twitching-access-token-secret*))))

(defun twitching-get-statuses (url params-alist &optional method)
  "Main business logic method to get twitter statuses.  The
result is returned as a list of type `twitching-status'.

Makes a call to URL with PARAMS-ALIST added to the query-string.

METHOD determines the http method GET or POST.  Default
is GET."
  (let ((response (twitching-oauth-get-http-response
                   url params-alist (or method "GET"))))
    (when (twitching-request-success-p response)
      (let ((response-body (twitching-extract-response-body response))
            (json-false 'nil)
            (json-array-type 'list))
        (let ((statuses (json-read-from-string response-body)))
          (mapcar #'new-twitching-status statuses))))))

(defun twitching-star-tweet (tweet)
  "Favorite or Unfavorite TWEET depending upon its favorited status."
  (let* ((favoritedp (twitching-status-favoritedp tweet))
         (id (twitching-status-id tweet))
         (url (concat "http://api.twitter.com/1/favorites"
                      (if favoritedp "/destroy/" "/create/")
                      id
                      ".json"))
         (response (twitching-oauth-get-http-response url nil "POST")))
    (when (twitching-request-success-p response)
      (let* ((body (twitching-extract-response-body response))
             (status (json-read-from-string body))
             (new-tweet (new-twitching-status status)))
        ;; Because we don't set "include_entities=1" in this request,
        ;; they aren't sent by twitter.  So we set the favorited
        ;; status manually.
        (setf (twitching-status-favoritedp tweet)
              (twitching-status-favoritedp new-tweet))
        tweet))))

(defun twitching-oauth-get-http-response (url params method)
  "Form an oauth request from URL with PARAMS and METHOD and
return the response as a string."
  (let* ((url (twitching-form-url url params))
         (req (make-twitching-oauth-request url)))
    (setf (oauth-request-http-method req) method)
    (oauth-sign-request-hmac-sha1 req (oauth-access-token-consumer-secret
                                       *twitching-oauth-access-token*))
    (let* ((url (oauth-request-url req))
           (headers (oauth-request-to-header req))
           (request-method (oauth-request-http-method req))
           (response (url-retrieve-synchronously-as-string
                      url headers request-method)))
      response)))

(defun make-twitching-oauth-request (url)
  (oauth-make-request
   url
   (oauth-access-token-consumer-key *twitching-oauth-access-token*)
   (oauth-access-token-auth-t *twitching-oauth-access-token*)))

(defun twitching-request-success-p (response)
  "Returns t if RESPONSE contains \"HTTP/1.1 200 OK\""
  (string-match-p "HTTP/1.1 200 OK" response))

(defun twitching-extract-response-body (response)
  "Extracts the response body, ignoring the headers from
RESPONSE."
  (let ((content-start (string-match "\n\n" response)))
    (when (>= content-start 0)
      (let ((content (substring response (+ content-start 2)))
            (json-array-type 'list))
        content))))

(defun twitching-form-url (url params-alist)
  "Form the full url with its query parameters from URL and PARAMS-ALIST"
  (unless (string-match-p (concat (regexp-quote "?") "$") url)
    (setq url (concat url "?")))
  (dolist (param params-alist url)
    (setq url (concat url
                      (url-insert-entities-in-string2 (car param))
                      "="
                      (url-insert-entities-in-string2 (cdr param))
                      "&"))))

(defun twitching-get-params ()
  "Returns parameters since_id, count etc."
  (let (params)
    (when *twitching-since-id*
      (setq params (acons "since_id" (format "%s" *twitching-since-id*) params)))
    (when *twitching-count*
      (setq params (acons "count" (format "%s" *twitching-count*) params)))
    (when *twitching-include-entities*
      (setq params (acons "include_entities" "1" params)))
    (when *twitching-page-number*
      (setq params (acons "page" (format "%d" *twitching-page-number*) params)))
    params))


;;; General utility functions that should probably be elsewhere.
(defun url-retrieve-synchronously-as-string (url &optional headers request-method)
  "Retrieves the contents of URL and returns the response as a
string.  Passes HEADERS with the request and the request is made
as specified in REQUEST-METHOD.  By default REQUEST-METHOD is
GET."
  (let ((url-request-extra-headers (if url-request-extra-headers
                                       (append url-request-extra-headers headers)
                                     headers))
        (url-request-method (or request-method "GET"))
        (count 0)
        (limit 60)
        response)
    (url-retrieve url (lambda (s) (setq response (or (buffer-string) ""))))
    (while (and (null response) (< count limit))
      (setq count (1+ count))
      (sleep-for 1))
    (if (> count limit) (message "request timed out."))
    response))

(defun url-insert-entities-in-string2 (s)
  "Same as url-insert-entities-in-string but additionally
replaces spaces with %20."
  (replace-regexp-in-string " " "%20" (url-insert-entities-in-string s) nil))

(provide 'twitching)

;;; twitching.el ends here
