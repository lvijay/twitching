; -*- mode: emacs-lisp; paredit-mode: t; -*-
;;; twitching.el --- Twitter client library.
;;;
;;; Copyright (C) 2011 Vijay Lakshminarayanan
;;;
;;; Author: Vijay Lakshminarayanan <laksvij AT gmail.com>
;;; Version: 0.7.2
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
(eval-when-compile (require 'cl))
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
(defun start-twitching ()
  "Start timer to fetch home timeline."
  (interactive)
  (twitching-api-check-keys)
  (labels ((mintos (min) (print (* min 60))))
    (if *twitching-timer*
        (message "Timer already running.")
      (setq *twitching-timer*
            (run-with-timer 0
                            *twitching-timer-interval*
                            #'twitching-home-timeline-get
                            nil)))))

;;;###autoload
(defun stop-twitching ()
  "Stop the timer that fetches tweets."
  (interactive)
  (if *twitching-timer*
    (progn
      (cancel-timer *twitching-timer*)
      (setq *twitching-timer* nil)
      (message "Stopped twitching timer."))
    (message "twitching timer not running.")))

;;;###autoload
(defun twitching-home-timeline-get (n)
  "Fetch home timeline."
  (interactive "P")
  (let ((buffer (get-twitching-buffer))
        (tweets (twitching-api-get-home-timeline)))
    (twitching-write-tweets tweets buffer)
    (when n (switch-to-buffer buffer t)))
  (message "retrieved tweets"))

;;;###autoload
(defun twitching-show-favorites (&optional show-buffer-p)
  "Show favorited tweets.  With a prefix argument, switch to the
buffer after retrieval."
  (interactive "P")
  (let ((buffer (get-twitching-favorites-buffer))
        (tweets (twitching-api-get-favorites)))
    (with-current-buffer buffer (kill-buffer))
    (setq buffer (get-twitching-favorites-buffer))
    (twitching-write-tweets tweets buffer)
    (when show-buffer-p (switch-to-buffer buffer t)))
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

(defun get-twitching-buffer ()
  "Return the buffer used to show the user's Twitter home
timeline."
  (get-buffer-create "*Twitching*"))

(defun get-twitching-favorites-buffer ()
  "Return the buffer used to show the user's Twitter favorites."
  (get-buffer-create "*Favorite Tweets*"))


(defmacro* with-tweet-under-point (tweet-var (point &optional buffer) &rest body)
  "Macro that gets the tweet under POINT in BUFFER, binds it to
tweet-var for execution in BODY.  Raises (error \"No tweet under
point %d\" point) if there is no tweet under POINT.  BUFFER is
optional, the current buffer will be assumed if it is not
provided."
  (declare (indent 2))
  (let ((pt (gensym "POINT"))
        (buf (gensym "BUFFER")))
    `(let ((,pt ,point)
           (,buf ,(or buffer '(current-buffer))))
       (let ((,tweet-var (get-text-property ,pt 'tweet ,buf)))
         (if ,tweet-var
             (progn .,body)
           (error "No tweet under point %d" ,pt))))))

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
                               (form `(cdr-assoc ',json-field json-object))
                               (key-fn (third field)))
                          (if key-fn
                              `(,struct-field (preprop (funcall ,key-fn ,form)))
                              `(,struct-field (preprop ,form)))))
                      fields)))))))

(defmacro* with-url-retrieve ((url method &optional headers)
                              (response &rest argnames-values)
                              &body body)
  "Macro wrapper around `url-retrieve' that allows writing code
as if the URL retrieval were synchronous though it is actually
asyncronous.

URL is the url to fetch using METHOD.  HEADERS is an alist
of (HEADER-NAME . HEADER-VALUE) pairs where both the values are
strings.

RESPONSE is the variable that will hold the response string,
including HTTP headers.

ARGNAMES-VALUES is the list of arguments that will be used when
the HTTP response is received.  Each can be specified either as a
list of (VARIABLE-NAME VALUE-FORM) or just a symbol
VARIABLE-NAME.  If the first syntax is used then VALUE-FORM is
evaluated and bound to VARIABLE-NAME.  If only a symbol is
provided then the value is taken from the enclosing scope.

BODY is not executed if the response is in error."
  (declare (indent 2))
  (let ((status (gensym "status"))
        (request (gensym "request")))
    (destructuring-bind (argnames . values)
        (loop as argval in argnames-values
              if (consp argval)
                collect (first argval) into argnames and
                collect (second argval) into values
              else
                collect argval into argnames and
                collect argval into values
              finally return (cons argnames values))
      `(let ((url-request-method ,method)
             (url-request-extra-headers (append url-request-extra-headers
                                                ,headers)))
         (declare (special url-request-extra-headers))
         (url-retrieve ,url
                       (lambda (,status .,argnames)
                         (unwind-protect
                             (if ,status
                                 (message "Error: %S" ,status)
                               (let ((,response (buffer-string)))
                                 .,body))
                           (kill-buffer)))
                       (list .,values))))))


;;; struct definitions
;; Defines a twitter-user
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

;; Defines a twitter-entity
(twitching-defstruct twitching-entity
  (hashtags hashtags)
  (user_mentions mentions)
  (urls urls))

;; Defines a twitter-status
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
                                                 (:background "white")
                                                 (:foreground "black")
                                                 (:overline t))))

(defvar *twitching-user-name-category*
  (put '*twitching-user-name-category* 'face '((:weight bold)
                                               (:background "white")
                                               (:foreground "black")
                                               (:overline t))))

(defvar *twitching-timestamp-category*
  (put '*twitching-timestamp-category* 'face '((:background "white")
                                               (:foreground "black")
                                               (:overline t)
                                               (:weight bold))))

(defvar *twitching-separator-category*
  (put '*twitching-separator-category* 'face '((:overline t)
                                               (:background "white")
                                               (:foreground "black")
                                               (:weight bold))))

(defvar *twitching-separator*
  (propertize " | " 'category '*twitching-separator-category*))

(defvar *twitching-plaintext-category*
  (put '*twitching-plaintext-category* 'face '((:background "white")
                                               (:foreground "black"))))

(defvar *twitching-hashtags-category*
  (put '*twitching-hashtags-category* 'face '((:background "white")
                                             (:foreground "firebrick")
                                             (:slant italic))))

(defvar *twitching-mentions-category*
  (put '*twitching-mentions-category* 'face '((:background "white")
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

(defvar *twitching-newline-category*
  (put '*twitching-newline-category* 'face '((:background "white")
                                             (:foreground "white"))))

(defvar *twitching-newline*
  (propertize "\n" 'category '*twitching-newline-category*))

(defvar *twitching-fill-column* 70 "Set this to manipulate `fill-column'.")

(defun twitching-render-region (start end buffer)
  "Renders the region in START and END in BUFFER.  Reads
`twitching-status'es in the region and renders them if they do
not need filtering."
  (when (> start end) (rotatef start end))
  (with-twitching-buffer buffer
    (let* ((s (buffer-substring-no-properties start end))
           (s (twitching-filter-text s))
           result
           (read-start 0))
      (delete-region start end)
      (goto-char start)
      (insert s)
      (setq read-start 0)
      (while (ignore-errors (setq result (read-from-string s read-start)))
        (let* ((status (car result))
               (read-end (cdr result))
               (twt-start (+ start read-start))
               (twt-end (+ start read-end)))
          (set-text-properties twt-start twt-end nil)
          (put-text-property twt-start twt-end 'tweet status)
          (twitching-render-tweet status twt-start twt-end)
          (setq read-start read-end))))))

(defun twitching-render-tweet (tweet start end)
  (destructuring-bind (image title-text tweet-text)
      (twitching-rendering-components tweet)
    (let* ((nl *twitching-newline*))
      (when image
        (put-text-property start (incf start) 'display image)
        (setq title-text (concat " " title-text)))
      (put-text-property start (incf start) 'display (concat title-text nl))
      (put-text-property start end 'display (concat tweet-text nl)))))

(defun twitching-rendering-components (status)
  "Return a list of (IMAGE TITLE-TEXT STATUS-TEXT).

IMAGE is the image associated with STATUS, it is nil if the image
cannot be displayed.  This could be because Emacs cannot display
the image type or because the user does not have any image
associated with the account."
  (let* ((user (twitching-status-user status))
         (image (twitching-profile-get-image user))
         (title-text (twitching-decorate-title-text status))
         (tweet-text (twitching-decorate-status-text status)))
    (list image title-text tweet-text)))

(defun twitching-decorate-title-text (status)
  (let* ((user (twitching-status-user status))
         (screen-name (twitching-user-screen-name user))
         (screen-name (propertize screen-name
                         'category '*twitching-screen-name-category*))
         (user-name (twitching-user-name user))
         (user-name (propertize user-name
                         'category '*twitching-user-name-category*))
         (created-at (twitching-status-created-at status))
         (created-at (format-time-string "%a %b %d %H:%M:%S %z %Y"
                                         (date-to-time created-at)))
         (created-at (propertize created-at
                         'category '*twitching-timestamp-category*))
         (favoritedp (twitching-status-favoritedp status))
         (retweetedp (twitching-status-retweetedp status))
         (sep *twitching-separator*)
         (star *twitching-star*)
         (retweet *twitching-retweet*)
         (text (concat screen-name sep user-name sep created-at)))
    (when favoritedp (setq text (concat text star)))
    (when retweetedp (setq text (concat text sep retweet)))
    text))

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
               (mapcar (lambda (x) `(,@(cdr-assoc 'indices x) ,type)) list)))
         (hashtags (funcall fn hashtags 'hashtag))
         (mentions (funcall fn mentions 'mention))
         (urls (funcall fn urls 'url))
         (indices (sort (append hashtags mentions urls)
                        (lambda (idx1 idx2) (< (second idx1) (second idx2)))))
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
                           (txt (html-decode txt))
                           (type (third idx)))
                      (propertize txt 'category (cdr-assoc type properties))))
                  (reverse result)))
    (with-temp-buffer
      (text-mode)
      (insert (apply #'concat result))
      (let ((fill-column *twitching-fill-column*)
            (sentence-end-double-space 'nil)
            (use-hard-newlines 'nil))
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
If BUFFER is not provided, (current-buffer) is assumed."
  (when (not point) (setq point (find-twitching-status tweet buffer)))
  (when (not buffer) (setq buffer (current-buffer)))
  (let* ((status (get-text-property point 'tweet buffer))
         (id (twitching-status-id tweet))
         (p point))
    (if (and (twitching-status-p tweet)
             (string-equal id (twitching-status-id tweet)))
        (destructuring-bind (b lb ub) (get-twitching-tweet-bounds p buffer)
          (with-twitching-buffer buffer
            (delete-region lb ub)
            (save-excursion
              (let* ((text (format "%S\n" tweet))
                     (length (length text))
                     (b (or b lb))      ; b is nil for the first tweet
                     (ub (+ lb length)))
                (insert text)
                (set-text-properties lb ub 'nil buffer)
                (twitching-render-region b (point-max) buffer)))
            (goto-char lb)))
      'nil)))

(defun get-twitching-tweet-bounds (point buffer)
  "Return a list (of B LB UB) where start and end are the
starting and ending points of POINT in BUFFER.  Return nil if
POINT does not exist in BUFFER.

    +------------+ +------------+ +----------+
    | prev tweet | | this tweet | |next tweet|
    +------------+ +------------+ +----------+
    a            b lb    p     ub e          f

a, b, lb, p, ub, e and f are all points in BUFFER representing
the bounds of the tweets `prev tweet', 'this tweet' and `next
tweet'."
  (let* ((tweet (get-text-property point 'tweet buffer))
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
    (if (twitching-status-p tweet)
        (let* ((point-max (point-max))
               (b (previous-single-property-change p 'tweet buffer (point-min)))
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
          (list b lb ub))
      'nil)))


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
    (define-key keymap (kbd "s") 'twitching-favorite-tweet)
    (define-key keymap (kbd "o") 'twitching-open-link)
    (define-key keymap (kbd "O") 'twitching-open-all-links)
    (define-key keymap (kbd "q") 'bury-buffer)
    (define-key keymap (kbd "SPC") 'scroll-up)
    (define-key keymap (kbd "<backspace>") 'scroll-down)
    (define-key keymap (kbd "#") 'twitching-open-hashtag)
    (define-key keymap (kbd "@") 'twitching-open-mention)
    (let ((action-map (make-sparse-keymap)))
      (define-key action-map (kbd "f") 'twitching-follow-user-in-tweet)
      (define-key action-map (kbd "u") 'twitching-unfollow-user-in-tweet)
      (define-key action-map (kbd "F") 'twitching-follow-user)
      (define-key action-map (kbd "U") 'twitching-unfollow-user)
      (define-key keymap (kbd "a") action-map))
    (let ((filter-map (make-sparse-keymap)))
      (define-key filter-map (kbd "#") 'twitching-filter-hashtag)
      (define-key filter-map (kbd "@") 'twitching-filter-user)
      (define-key filter-map (kbd "w") 'twitching-filter-word)
      (define-key keymap (kbd "f") filter-map))
    (let ((remove-map (make-sparse-keymap)))
      (define-key remove-map (kbd "#") 'twitching-remove-hashtag)
      (define-key remove-map (kbd "@") 'twitching-remove-user)
      (define-key remove-map (kbd "w") 'twitching-remove-word)
      (define-key keymap (kbd "r") remove-map))
    (let ((copy-map (make-sparse-keymap)))
      (define-key copy-map (kbd "@") 'twitching-copy-tweet-mention)
      (define-key copy-map (kbd "m") 'twitching-copy-tweet-mention)
      (define-key copy-map (kbd "a") 'twitching-copy-tweet-author)
      (define-key copy-map (kbd "d") 'twitching-copy-tweet-date)
      (define-key copy-map (kbd "s") 'twitching-copy-tweet-status-id)
      (define-key copy-map (kbd "h") 'twitching-copy-tweet-hashtag)
      (define-key copy-map (kbd "u") 'twitching-copy-tweet-url)
      (define-key copy-map (kbd "t") 'twitching-copy-tweet-text)
      (define-key keymap (kbd "c") copy-map))
    (let ((group-map (make-sparse-keymap)))
      (define-key group-map (kbd "a") 'twitching-group-all)
      (define-key group-map (kbd "@") 'twitching-group-mention)
      (define-key keymap (kbd "g") group-map))
    keymap))

(define-derived-mode twitching-mode nil "Twitching"
  "Major mode for viewing tweets.

\\{twitching-mode-map}")

(defmacro with-twitching-buffer (buffer &rest body)
  (declare (indent 1))
  `(with-current-buffer ,buffer
     (twitching-mode)
     (toggle-read-only -1)
     (unwind-protect
         ,@body
       (toggle-read-only +1))))


;;; Filters
(defstruct (twitching-filter (:type vector) :named)
  documentation
  action
  args)

(defvar *twitching-filters* (list)
  "List of `twitching-filter'.")

(defun twitching-filter-filter-tweet-p (tweet)
  "Return t if TWEET must be filtered, nil otherwise."
  (loop for filter in *twitching-filters*
        while tweet
        if (invoke-twitching-filter filter tweet) return 't
        finally return 'nil))

(defun invoke-twitching-filter (filter tweet)
  "Return t if FILTER filters TWEET, nil otherwise."
  (apply (twitching-filter-action filter) tweet (twitching-filter-args filter)))

(defun twitching-filter-escape (word)
  (regexp-quote (downcase word)))

(defun twitching-filter-text (text)
  "Parse TEXT, read twitching-statuses, filter out tweets and
return the remaining text."
  (let ((read-start 0)
        result)
    (with-temp-buffer
      (while (ignore-errors (setq result (read-from-string text read-start)))
        (let ((status (car result))
              (read-end (cdr result)))
          (when (and (twitching-status-p status)
                     (not (twitching-filter-filter-tweet-p status)))
            (insert (format "%S\n" status)))
          (setq read-start read-end)))
      (buffer-string))))


;;; Mode interactive functions
(defun twitching-next-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (let* ((plusp (plusp n))
         direction
         limit
         (n (abs n))
         (buffer (current-buffer))
         tweet
         (point (point)))
    (if plusp
        (setq direction #'next-single-property-change
              limit (point-max))
        (setq direction #'previous-single-property-change
              limit (point-min)))
    (dotimes (i n)
      (setq tweet (get-text-property point 'tweet buffer)
            point (funcall direction point 'tweet buffer limit))
      (if (and tweet (eq tweet (get-text-property point 'tweet buffer)))
          (setq point (funcall direction point 'tweet buffer limit))))
    (goto-char point)))

(defun twitching-prev-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (twitching-next-tweet (- n)))

(defun twitching-favorite-tweet (point)
  "Favorite or unfavorite the tweet at POINT."
  (interactive "d")
  (with-tweet-under-point tweet (point)
    (twitching-api-star-tweet tweet)
    (twitching-rerender-tweet tweet point)))

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
  (if (zerop n)
      (with-tweet-under-point tweet ((point))
        (funcall browse-url-browser-function
                 (concat "http://twitter.com/#!/"
                         (twitching-user-screen-name
                          (twitching-status-user tweet)))))
    (unless (twitching-open-entity (point) #'twitching-entity-mentions
                                   'screen_name n
                                   "https://twitter.com/#!/")
        (message "No mention in tweet."))))

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
  (with-tweet-under-point status (point)
    (let* ((elem (twitching-get-nth-entity status entity-elem-fn key n))
           (url (concat url-prefix elem)))
      (when elem
        (funcall browse-url-browser-function url)))))

(defun twitching-follow-user-in-tweet (n point)
  "Follows the Nth user in the tweet under point.  Counting
starts at 1.  If N is zero, follows the user who has tweeted the
current tweet."
  (interactive "p\nd")
  (let ((status (get-text-property point 'tweet)))
    (twitching-un/follow-user-in-tweet n status nil)))

(defun twitching-unfollow-user-in-tweet (n point)
  "Unfollows the Nth user in the tweet under point.  Counting
starts at 1.  If N is zero, unfollows the user who has tweeted
the current tweet."
  (interactive "p\nd")
  (let ((status (get-text-property point 'tweet)))
    (twitching-un/follow-user-in-tweet n status t)))

(defun twitching-un/follow-user-in-tweet (n status unfollowp)
  (let (screen-name)
    (if status
        (progn
          (if (= n 0)
              (setq screen-name (twitching-user-screen-name
                                 (twitching-status-user status)))
            (setq screen-name (twitching-get-nth-entity
                               status
                               #'twitching-entity-mentions
                               'screen_name
                               n)))
          (if screen-name
              (twitching-un?follow-user screen-name unfollowp)
            (message
             (format "Could not find mention #%d in current tweet." n))))
      (message "No tweet at point."))))

(defun twitching-get-nth-entity (tweet entity-elem-fn key n)
  (let* ((entities (twitching-status-entities tweet))
         (elems (funcall entity-elem-fn entities))
         (elem (nth (1- n) elems)))
    (cdr-assoc key elem)))

(defun twitching-follow-user (screen-name)
  "Get user input for a Twitter SCREEN-NAME and start following
them."
  (interactive "sEnter Twitter screen-name: ")
  (twitching-un?follow-user screen-name nil))

(defun twitching-unfollow-user (screen-name)
  "Get user input for a Twitter SCREEN-NAME and stop following
them."
  (interactive "sEnter Twitter screen-name: ")
  (twitching-un?follow-user screen-name t))

(defun twitching-un?follow-user (screen-name unfollowp)
  (let ((action (format "%sfollow" (if unfollowp "un" ""))))
    (when (y-or-n-p (format "Going to %s '%s'.  Confirm: " action screen-name))
      (let ((status (twitching-api-follow-screen-name screen-name unfollowp)))
        (case status
          (200 (message (format "%s %sed." screen-name action)))
          (404 (message (format "%s does not exist" screen-name)))
          (403 (message (format "Already %sing %s" action screen-name)))
          (t (message (format "Received error code %d" status))))))))

(defun twitching-filter-hashtag (n point)
  "Creates a filter that filters the N-th hashtag in the tweet at
POINT."
  (interactive "p\nd")
  (with-tweet-under-point tweet (point)
    (let* ((elem-fn #'twitching-entity-hashtags)
           (key 'text)
           (ht-raw (twitching-get-nth-entity tweet elem-fn key n)))
      (if ht-raw
          (let* ((hashtag (twitching-filter-escape ht-raw))
                 (fn (lambda (tweet hashtag)
                       (let* ((entities (twitching-status-entities tweet))
                              (hashtags (twitching-entity-hashtags entities)))
                         (loop for hte in hashtags
                           if (string= hashtag (downcase (cdr-assoc 'text hte)))
                           return t
                           finally return nil))))
                 (doc (concat "Filter #" ht-raw))
                 (filter (make-twitching-filter :documentation doc
                                                :action fn
                                                :args (list hashtag))))
            (twitching-filter--action (concat "#" ht-raw) filter point))
        (error "No hashtag in tweet.")))))

(defun twitching-filter-user (n point)
  "Create a filter that filters the N-th user in the tweet at
POINT.  Counting starts at 1.  With a prefix argument of 0,
filters the user that has made the tweet."
  (interactive "p\nd")
  (with-tweet-under-point tweet (point)
    (let* ((mention (if (= n 0)
                        (twitching-user-screen-name
                         (twitching-status-user tweet))
                      (twitching-get-nth-entity tweet
                                                #'twitching-entity-mentions
                                                'screen_name
                                                n)))
           (fn #'do-twitching-filter-mention))
      (if mention
          (let* ((username (twitching-filter-escape mention))
                 (doc (concat "Filter @" mention))
                 (filter (make-twitching-filter :documentation doc
                                                :action fn
                                                :args (list username))))
            (twitching-filter--action (concat "@" mention) filter point))
        (error "No mention in tweet.")))))

(defun twitching-filter-word (word)
  (interactive "sEnter filter word: ")
  (let* ((werd word)
         (word (twitching-filter-escape word))
         (point (point))
         (fn (lambda (tweet word)
               (let ((case-fold-search t)
                     (text (twitching-status-text tweet)))
                 (string-match-p word text))))
         (doc (concat "Filter " word))
         (filter (make-twitching-filter :documentation doc
                                        :action fn
                                        :args (list word))))
    (twitching-filter--action werd filter point)))

(defun twitching-filter--action (doc filter point)
  "Common function for all filtering."
  (when (y-or-n-p (concat "Create new filter on " doc "? "))
    (push filter *twitching-filters*)
    (twitching-render-region (point-min) (point-max) (current-buffer))
    (goto-char (min point (point-max)))
    (message "Filtered buffer for %s" doc)))

(defun do-twitching-filter-mention (tweet mention)
  (let* ((entities (twitching-status-entities tweet))
         (mentions (twitching-entity-mentions entities))
         (username (twitching-user-screen-name
                    (twitching-status-user tweet)))
         (username (downcase username)))
    (or 
     (string-match-p mention username)
     (loop for me in mentions
           if (string-match-p mention (cdr-assoc 'screen_name me))
           return t
           finally return nil))))

(defun twitching-remove-hashtag (n point)
  "Remove the N-th hashtag in the tweet at POINT from all tweets
in current buffer."
  (interactive "p\nd")
  (twitching-filter-hashtag n point)
  (pop *twitching-filters*))

(defun twitching-remove-user (n point)
  (interactive "p\nd")
  (twitching-filter-user n point)
  (pop *twitching-filters*))

(defun twitching-remove-word (word)
  "Remove word from all tweets in the current buffer."
  (interactive "sEnter filter word: ")
  (twitching-filter-word word)
  (pop *twitching-filters*))

;;; copy methods
(defun twitching-copy-tweet-text (point)
  "Copies the text of tweet under POINT and places it in the
`kill-ring'."
  (interactive "d")
  (with-tweet-under-point tweet (point)
    (kill-new (twitching-status-text tweet))))

(defun twitching-copy-tweet-url (n point)
  "Copies the N-th url in the tweet under POINT."
  (interactive "p\nd")
  (with-tweet-under-point tweet (point)
    (let* ((elem-fn #'twitching-entity-urls)
           (key 'url)
           (url (twitching-get-nth-entity tweet elem-fn key n)))
      (if url
          (kill-new url)
        (error "URL #%d not found in tweet" n)))))

(defun twitching-copy-tweet-mention (n point)
  "Copies the N-th mention in the tweet under POINT."
  (interactive "p\nd")
  (with-tweet-under-point tweet (point)
    (let* ((elem-fn #'twitching-entity-mentions)
           (key 'screen_name)
           (mention (twitching-get-nth-entity tweet elem-fn key n)))
      (if mention
          (kill-new (concat "@" mention))
        (error "Mention #%d not found in tweet" n)))))

(defun twitching-copy-tweet-hashtag (n point)
  "Copies the N-th hashtag in the tweet under POINT."
  (interactive "p\nd")
  (with-tweet-under-point tweet (point)
    (let* ((elem-fn #'twitching-entity-hashtags)
           (key 'text)
           (hashtag (twitching-get-nth-entity tweet elem-fn key n)))
      (if hashtag
          (kill-new (concat "#" hashtag))
        (error "Mention #%d not found in tweet" n)))))

(defun twitching-copy-tweet-author (point full-name-p)
  "Copies the screen-name of the tweet under POINT.  With a
prefix argument, i.e., when FULL-NAME-P is non-nil, copies the
tweet's user-name."
  (interactive "d\nP")
  (with-tweet-under-point tweet (point)
    (let ((user (twitching-status-user tweet)))
      (if full-name-p
          (kill-new (twitching-user-name user))
        (kill-new (twitching-user-screen-name user))))))

(defun twitching-copy-tweet-date (point local-time-p)
  "Copies the date of the tweet under POINT.  With a prefix
argument, i.e., when LOCAL-TIME-P is non-nill, copies the tweet's
time as a local time."
  (interactive "d\nP")
  (with-tweet-under-point tweet (point)
    (let ((time (twitching-status-created-at tweet)))
      (if local-time-p
          (kill-new (format-time-string "%a %b %d %H:%M:%S %z %Y"
                                        (date-to-time time)))
        (kill-new time)))))

(defun twitching-copy-tweet-status-id (point)
  "Copies the status-id of the tweet under POINT."
  (interactive "d")
  (with-tweet-under-point tweet (point)
    (kill-new (format "https://twitter.com/#!/%s/status/%s"
                      (twitching-user-screen-name (twitching-status-user tweet))
                      (twitching-status-id tweet)))))


;;; Grouping
(defun twitching-group-all (point switchp &optional buffer-name)
  "Take all tweets from POINT until (point-max), sort them by
username and put them in BUFFER-NAME.  If provided, BUFFER-NAME
will be used, else the grouped tweets will be stored in
\"*Twitching Grouped All*\""
  (interactive "d\nP")
  (setq buffer-name (or buffer-name "*All Grouped Twitching*"))
  (let ((buffer (get-buffer-create buffer-name))
        (cb (current-buffer))
        (fn (lambda (x y)
              (let ((key (lambda (x)
                           (downcase (twitching-user-screen-name
                                      (twitching-status-user x))))))
                (string< (funcall key x)
                         (funcall key y)))))
        (tweets (list))
        text)
    (destructuring-bind (b lb ub) (get-twitching-tweet-bounds point cb)
      (setq text (buffer-substring-no-properties lb (point-max))))
    (with-twitching-buffer buffer
      (delete-region (point-min) (point-max))
      (let (result
            (read-start 0))
        (while (ignore-errors (setq result (read-from-string text read-start)))
          (let* ((status (car result))
                 (read-end (cdr result)))
            (setq read-start read-end)
            (push status tweets))))
      (setq tweets (sort tweets fn))
      (dolist (tweet tweets)
        (insert (format "%S\n" tweet)))
      (twitching-render-region (point-min) (point-max) buffer)
      (goto-char (point-min)))
    (if switchp
        (switch-to-buffer buffer)
      (message (concat "tweets grouped in buffer " buffer-name)))))

(defun twitching-group-mention (point switchp)
  "Group all tweets by the tweeter under POINT and put them in
the buffer (concat \"*Twitching Group \" SCREEN_NAME \"*\")
where SCREEN_NAME is the tweeter's screen name."
  (interactive "d\nP")
  (with-tweet-under-point tweet (point)
    (let* ((user (twitching-status-user tweet))
           (screen-name (twitching-user-screen-name user))
           (filter (make-twitching-filter
                    :documentation (format "Ignore all but %s" screen-name)
                    :action (lambda (&rest args)
                              (not (apply #'do-twitching-filter-mention args)))
                    :args (list screen-name)))
           (buffer-name (format "*Twitching Group %s*" screen-name)))
      (let ((*twitching-filters* (cons filter *twitching-filters*)))
        (twitching-group-all point switchp buffer-name)))))


;;; Twitter API interactions
(defvar *twitching-api-user-dir*
  (expand-file-name (convert-standard-filename "~/.emacs.d/twitching"))
  "Name of the directory where the user's tweets are stored.")

(defvar *twitching-api-consumer-key* nil "Twitter consumer key.")

(defvar *twitching-api-consumer-secret* nil "Twitter consumer secret.")

(defvar *twitching-api-access-token* nil "Twitter access key.")

(defvar *twitching-api-access-token-secret* nil "Twitter access token secret.")

(defvar *twitching-api-oauth-access-token* nil "OAuth access token.")

(defvar *twitching-api-since-id* nil "Last status-id received from twitter.")

(defvar *twitching-api-count* nil "Number of tweets to fetch.")

(defvar *twitching-api-page-number* nil "Page number to fetch.")

(defvar *twitching-api-page-limit* nil "Maximum number of pages to fetch.")

(defvar *twitching-api-include-entities* t "sets include_entities to 1 or 0")

(defun twitching-api-get-home-timeline ()
  "Gets the current user's home timeline as a list of
`twitching-status'es."
  (twitching-api-check-keys)
  (let* ((url "http://api.twitter.com/1/statuses/home_timeline.json")
         (*twitching-api-count* (or *twitching-api-count* 200))
         (*twitching-api-page-limit* (or *twitching-api-page-limit* 10))
         (result (twitching-api-keep-getting-statuses url t)))
    (when result
      (setq *twitching-api-since-id* (cdr result)))
    (car result)))

(defun twitching-api-get-favorites ()
  (twitching-api-check-keys)
  (let* ((url "http://api.twitter.com/1/favorites.json")
         (*twitching-api-count* 'nil)       ; unused for favorites
         (*twitching-api-since-id* 'nil)    ; unused for favorites
         (result (twitching-api-keep-getting-statuses url t)))
    (car result)))

(defun twitching-api-keep-getting-statuses (url &optional fullyp)
  "Continually makes GET calls on URL by incrementing the page
number to fetch.  If FULLYP and `*twitching-api-since-id*' are nil,
however, it does this only once.  Returns the cons (statuses
. since_id) where since_id is the highest since_id in the
response or nil if there was no response."
  (let ((page 1)
        (count (or *twitching-api-count* 20))
        statuses)
    (if (and (not *twitching-api-since-id*) (not fullyp))
        (setq statuses (twitching-api-get-statuses url
                                                   (twitching-api-get-params)))
      (block loop
        (while t
          (let* ((*twitching-api-page-number* page)
                 (params (twitching-api-get-params))
                 (stats (twitching-api-get-statuses url params "GET")))
            (if stats
                (setq statuses (nconc stats statuses)
                      page (1+ page))
              (return-from loop))
            (if (and (numberp *twitching-api-page-limit*)
                     (>= page *twitching-api-page-limit*))
                (return-from loop))))))
    (when statuses
      (setq statuses (sort statuses
                           (lambda (x y)
                             (string< (twitching-status-id x)
                                      (twitching-status-id y)))))
      (let ((highest (twitching-status-id (car (last statuses)))))
        (cons statuses highest)))))

(defun twitching-api-check-keys ()
  "Checks if `*twitching-api-consumer-key*'
`*twitching-api-consumer-secret*' `*twitching-api-access-token*'
`*twitching-api-access-token-secret*' have been set.  Requests user
input if they haven't.

If `*twitching-api-access-token*' `*twitching-api-access-token-secret*'."
  (unless *twitching-api-consumer-key*
    (setq *twitching-api-consumer-key*
          (read-string "Enter consumer key: ")))
  (unless *twitching-api-consumer-secret*
    (setq *twitching-api-consumer-secret*
          (read-string "Enter consumer secret: ")))
  (unless (or *twitching-api-access-token* *twitching-api-access-token-secret*)
    (let* ((oauth-enable-browse-url t)
           response)
      (setq response (oauth-authorize-app *twitching-api-consumer-key*
                                          *twitching-api-consumer-secret*
                                          +twitter-oauth-request-url+
                                          +twitter-oauth-access-url+
                                          +twitter-oauth-authorize-url+))
      (setq *twitching-api-access-token*
            (oauth-t-token (oauth-access-token-auth-t response))
            *twitching-api-access-token-secret*
            (oauth-t-token-secret (oauth-access-token-auth-t response)))))
  (setq *twitching-api-oauth-access-token*
        (make-oauth-access-token
         :consumer-key *twitching-api-consumer-key*
         :consumer-secret *twitching-api-consumer-secret*
         :auth-t (make-oauth-t
                  :token *twitching-api-access-token*
                  :token-secret *twitching-api-access-token-secret*))))

(defun twitching-api-get-statuses (url params-alist &optional method)
  "Main business logic method to get twitter statuses.  The
result is returned as a list of type `twitching-status'.

Makes a call to URL with PARAMS-ALIST added to the query-string.

METHOD determines the http method GET or POST.  Default
is GET."
  (let ((response (twitching-api-oauth-get-http-response
                   url params-alist (or method "GET"))))
    (when (and (stringp response) (twitching-api-request-success-p response))
      (let ((response-body (http-extract-response-body response))
            (json-false 'nil)
            (json-array-type 'list))
        (let ((statuses (json-read-from-string response-body)))
          (mapcar #'new-twitching-status statuses))))))

(defun twitching-api-star-tweet (tweet)
  "Favorite or Unfavorite TWEET depending upon its favorited status."
  (let* ((favoritedp (twitching-status-favoritedp tweet))
         (id (twitching-status-id tweet))
         (url (concat "http://api.twitter.com/1/favorites"
                      (if favoritedp "/destroy/" "/create/")
                      id
                      ".json"))
         (json-false 'nil)
         (response (twitching-api-oauth-get-http-response url nil "POST")))
    (when (twitching-api-request-success-p response)
      (let* ((body (http-extract-response-body response))
             (status (json-read-from-string body))
             (new-tweet (new-twitching-status status)))
        ;; Because we don't set "include_entities=1" in this request,
        ;; they aren't sent by twitter.  So we set the favorited
        ;; status manually.
        (setf (twitching-status-favoritedp tweet)
              (twitching-status-favoritedp new-tweet))
        tweet))))

(defun twitching-api-follow-screen-name (screen-name &optional unfollowp)
  "Follows twitter user with SCREEN-NAME.  If UNFOLLOWP is t,
then unfollows the user.  Return the HTTP status code."
  (let* ((url (concat "http://api.twitter.com/1/friendships/"
                      (if unfollowp "destroy" "create")
                      ".json"))
         (method (if unfollowp "DELETE" "POST"))
         (params `(("screen_name" . ,screen-name)))
         (response (twitching-api-oauth-get-http-response url params "POST")))
    ;; 200 => success
    ;; 404 => No such user
    (url-get-http-status-code response)))

(defun twitching-api-oauth-get-http-response (url params method)
  "Form an oauth request from URL with PARAMS and METHOD and
return the response as a string."
  (let* ((url (twitching-api-form-url url params))
         (req (make-api-twitching-oauth-request url method))
         (headers (oauth-request-to-header req))
         (response (url-retrieve-synchronously-as-string
                    url headers method)))
    response))

(defun make-api-twitching-oauth-request (url method)
  (let* ((token *twitching-api-oauth-access-token*)
         (req (oauth-make-request url
                                  *twitching-api-consumer-key*
                                  (oauth-access-token-auth-t token))))
    (setf (oauth-request-http-method req) method)
    (oauth-sign-request-hmac-sha1 req *twitching-api-consumer-secret*)
    req))

(defun twitching-api-request-success-p (response)
  "Returns t if RESPONSE contains \"HTTP/1.1 200 OK\""
  (= 200 (url-get-http-status-code response)))

(defun twitching-api-form-url (url params-alist)
  "Form the full url with its query parameters from URL and PARAMS-ALIST"
  (unless (string-match-p (concat (regexp-quote "?") "$") url)
    (setq url (concat url "?")))
  (dolist (param params-alist url)
    (setq url (concat url
                      (url-insert-entities-in-string2 (car param))
                      "="
                      (url-insert-entities-in-string2 (cdr param))
                      "&"))))

(defun twitching-api-get-params ()
  "Returns parameters since_id, count etc."
  (let (params)
    (when *twitching-api-since-id*
      (setq params (acons "since_id"
                          (format "%s" *twitching-api-since-id*)
                          params)))
    (when *twitching-api-count*
      (setq params (acons "count" (format "%s" *twitching-api-count*) params)))
    (when *twitching-api-include-entities*
      (setq params (acons "include_entities" "1" params)))
    (when *twitching-api-page-number*
      (setq params (acons "page"
                          (format "%d" *twitching-api-page-number*)
                          params)))
    params))


;;; Profile Images management
(defvar *twitching-profile-use-p* 'nil
  "Setq this to t to enable display of user profile pics with
tweets.")

(defvar *twitching-profile-directory*
  (concat (file-name-as-directory *twitching-api-user-dir*) "thumbnails")
  "Where the image thumbnails are stored.")

(eval-when-compile
  (let ((dir *twitching-profile-directory*))
    (if (not (file-exists-p dir))
        (make-directory dir t)
      (and (file-directory-p dir)
           (file-accessible-directory-p dir)))))

(defvar *twitching-profile-users-pending* '()
  "List of users whose profile pictures need to be downloaded.")

(defvar *twitching-profile-timer* nil
  "Timer that downloads and saves pending user's profile
images.")

(defvar *twitching-profile-user-map*
  ;; I follow around 138 people and 179 is a close enough prime that
  ;; with a rehash-threshold of 0.8 matches the number of people I
  ;; follow.  Feel free to modify it to suit your own needs.
  (make-hash-table :test 'equal :weakness nil :size 179)
  "Map of user profiles and their locations in the filesystem.")

(defun twitching-profile-get-image (user)
  "Returns the filename of USER's profile image url as
stored in the local filesystem.  Returns nil if
*twitching-profile-use-p* is nil."
  (if (not *twitching-profile-use-p*) 'nil
    (let* ((key (twitching-profile-user-key user))
           (val (twitching-profile-get-saved-image key)))
      (or
       val
       (if (twitching-profile-no-image-p key)
           'nil
         ;; This means that the image is not stored in the map
         (prog1
             ;; check if the file exists, if it does return the file
             (let ((image-file (twitching-profile-user-image-file user)))
               (if (file-exists-p image-file)
                   (twitching-profile-save-image key image-file)
                 'nil))
           (push user *twitching-profile-users-pending*)
           (when *twitching-profile-timer*
             (cancel-timer *twitching-profile-timer*))
           (setq *twitching-profile-timer*
                 (run-with-idle-timer
                  30
                  nil
                  #'twitching-profile-download-images))))))))

(defun twitching-profile-user-image-file (user)
  "Given the USER, return the file that it would be saved as."
  (let* ((url (twitching-user-profile-image-url user))
         (extension (file-name-extension url))
         (screen-name (twitching-user-screen-name user))
         (dir (file-name-as-directory *twitching-profile-directory*))
         (image-file (concat screen-name "." extension))
         (image-file (concat dir image-file)))
    image-file))

(defun twitching-profile-download-images ()
  "Download pending images and save them."
  (when *twitching-profile-timer*
    (cancel-timer *twitching-profile-timer*))
  (setq *twitching-profile-timer* nil
        ;; remove duplicate entries first, speeds up download.
        *twitching-profile-users-pending*
        (remove-duplicates *twitching-profile-users-pending*
                           :key #'twitching-user-screen-name
                           :test #'string-equal))
  (loop
   as i from 1 to 5                  ; at most 5 downloads in parallel
   as user = (pop *twitching-profile-users-pending*)
   while user
   do (let* ((key (twitching-profile-user-key user))
             (image-file (twitching-profile-user-image-file user))
             (url (twitching-user-profile-image-url user))
             (callback
              (lambda (status image-file key)
                (unwind-protect
                    (if (cdr-assoc :error status) nil
                      (let* ((response (buffer-string))
                             (response (http-extract-response-body response)))
                        (with-temp-buffer
                          (insert response)
                          (write-file image-file 'nil))
                        (twitching-profile-save-image key image-file)))
                  (kill-buffer))))
             (cbargs (list image-file key))
             (url-request-method "GET"))
        (if (not (file-exists-p image-file))
            (url-retrieve url callback cbargs)
          (if (not (twitching-profile-get-saved-image key))
              (twitching-profile-save-image key image-file)))))
  ;; start the timer again if there are more items to download
  (if *twitching-profile-users-pending*
      (setq *twitching-profile-timer*
            (run-at-time "20 sec"
                         nil
                         #'twitching-profile-download-images))))

(defun twitching-profile-user-key (user)
  "Given USER, return the key representation to store in
`*twitching-profile-user-map*'."
  (downcase (twitching-user-screen-name user)))

(defun twitching-profile-get-saved-image (key)
  "Gets KEY in `*twitching-profile-user-map*'."
  (if (not (stringp key)) (error "%s is not a valid key." key))
  (let ((val (gethash key *twitching-profile-user-map*)))
    (if (eq val :no-image-p)
        nil
      val)))

(defun twitching-profile-save-image (key image-file)
  "Saves IMAGE-FILE in `*twitching-profile-user-map*' under KEY."
  (let* ((image (ignore-errors (create-image image-file)))
         (value (or image :no-image-p)))
    (puthash key value *twitching-profile-user-map*)
    image))

(defun twitching-profile-no-image-p (key)
  "Returns t if the image represented by KEY exists but cannot be
shown by Emacs; nil otherwise."
  (eq (gethash key *twitching-profile-user-map*) :no-image-p))


;;; General utility functions that should probably be elsewhere.
(defun http-extract-response-body (response)
  "Extracts the response body, ignoring the headers from
RESPONSE."
  (let ((content-start (string-match-p "\n\n" response)))
    (when (>= content-start 0)
      (let ((content (substring response (+ content-start 2)))
            (json-array-type 'list))
        content))))

(defun url-retrieve-synchronously-as-string (url &optional headers request-method)
  "Retrieves the contents of URL and returns the response as a
string.  Passes HEADERS with the request and the request is made
as specified in REQUEST-METHOD.  By default REQUEST-METHOD is
GET."
  (let ((url-request-extra-headers (if url-request-extra-headers
                                       (append url-request-extra-headers headers)
                                     headers))
        (url-request-method (or request-method "GET"))
        (url-mime-encoding-string "gzip")
        buffer
        response)
    (setq buffer (url-retrieve-synchronously url))
    (unwind-protect
        (with-current-buffer buffer
          ;; check if response is gzipped and handle accordingly
          (url-http-handle-decompression buffer)
          (setq response (buffer-string)))
      (kill-buffer buffer))
    response))

(defun url-http-handle-decompression (response-buffer)
  "Returns RESPONSE-BUFFER, decompressing its body if necessary.
If RESPONSE-BUFFER contains the header \"Content-Encoding:
gzip\", it is assumed that the body is gzip'd and that portion is
gunzip'd.

This function always returns the RESPONSE-BUFFER."
  (with-current-buffer response-buffer
    (goto-char (point-min))
    (let* ((noerror t)
           (header-end (progn (search-forward "\n\n" nil noerror 1) (point)))
           (gzippedp (progn
                       (goto-char (point-min))
                       (search-forward-regexp "^content-encoding: *gzip$"
                                              header-end noerror 1))))
      (when gzippedp
        (let ((filename (make-temp-file "response" nil ".gz"))
              contents)
          (unwind-protect
              (progn
                (write-region header-end (point-max) filename nil nil)
                (with-auto-compression-mode
                  (let ((buffer (find-file filename)))
                    (setq contents (buffer-string))
                    (kill-buffer buffer))))
            (delete-file filename nil))
          (with-current-buffer response-buffer
            (delete-region header-end (point-max))
            (goto-char header-end)
            (insert contents))))
      response-buffer)))

(defun url-get-http-status-code (response)
  "Return the http status code in RESPONSE.  Return nil if
RESPONSE is not a valid HTTP/1.1 response.  The return value is a
number."
  (if (string-match "^HTTP/1[.]1 \\([0-9]*\\)" response 0)
      (string-to-number (match-string 1 response))
    (error "Could not find HTTP status code in response.")))

(defun url-insert-entities-in-string2 (s)
  "Same as url-insert-entities-in-string but additionally
replaces spaces with %20."
  (replace-regexp-in-string " " "%20" (url-insert-entities-in-string s) nil))

(defvar html-decode-replacements '(("&quot;" . "'") ("&rsquo;" . "'"))
  "Strings and their replacements.")

(defun html-decode (string)
  "Replace all substrings in STRING of the form &#[0-9]+; with the
equivalent string."
  (let ((rep (lambda (s)
               (if (string= s "&#;")
                   s
                 (let ((s (substring s 2 (1- (length s)))))
                   (string (string-to-number s))))))
        (reps html-decode-replacements))
    (loop for (str . repl) in reps
          do (setq str (regexp-quote str))
          do (setq string (replace-regexp-in-string str repl string)))
    (replace-regexp-in-string "&#[0-9]*;" rep string)))

(defun cdr-assoc (key list)
  "Return (cdr (assoc KEY LIST))"
  (cdr (assoc key list)))

(provide 'twitching)

;;; twitching.el ends here
