; -*- mode: emacs-lisp; paredit-mode: t; -*-
;;; twitching.el --- Twitter client library.
;;;
;;; Copyright (C) 2011 Vijay Lakshminarayanan
;;;
;;; Author: Vijay Lakshminarayanan <laksvij AT gmail.com>
;;; Version: 0.3.0
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


;;; dependencies
(require 'cl)
(require 'url)
(require 'url-util)
(require 'hmac-sha1)
(require 'oauth)
(require 'json)
(require 'browse-url)


;;; Twitter constants
(defconst +twitter-oauth-request-url+ "http://api.twitter.com/oauth/request_token"
  "Twitter's end point for the request token.")

(defconst +twitter-oauth-access-url+ "https://api.twitter.com/oauth/access_token"
  "Twitter's access token end point.")

(defconst +twitter-oauth-authorize-url+ "https://api.twitter.com/oauth/authorize"
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
  (let* ((twitching-buffer (get-twitching-buffer))
         (tweets (twitching-get-home-timeline))
         starting-line
         ending-line)
    (with-current-buffer twitching-buffer
      (twitching-mode)
      (save-excursion
        (goto-char (point-max))
        (setq starting-line (line-number-at-pos))
        (mapc (lambda (tweet)
                (let ((tweet (format "%S" tweet)))
                  (insert (concat tweet "\n"))))
              tweets)
        (setq ending-line (line-number-at-pos))))
    (with-current-buffer twitching-buffer
      (save-excursion
        (goto-line ending-line)
        (twitching-layout (point))
        (twitching-prev-tweet (- ending-line starting-line)))))
  (message "retrieved tweets"))

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


;;; struct definitions
(defmacro twitching-defstruct (struct-name &rest fields)
  "Macro that defines structures representing twitter responses.
STRUCT-NAME is the name of the represented object.

FIELDS is a list of triples of the form (json-object-field-name
struct-field-name key-function).  key-function is optional and is
IDENTITY by default.  If provided, it must be a function that
takes one argument."
  (declare (indent 1))
  (let ((constructor-name (intern (concat "make-" (symbol-name struct-name))))
        (new-fun-name (intern (concat "new-" (symbol-name struct-name)))))
    `(progn
       (defstruct (,struct-name (:type vector)
                                :named)
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
  (geo_enabled geo-enabled-p #'twitching-json-truth-value)
  (verified verifiedp #'twitching-json-truth-value)
  (following followingp #'twitching-json-truth-value)
  (contributors_enabled contributors-enabled-p #'twitching-json-truth-value)
  (url url)
  (location location)
  (id_str id)
  (lang lang)
  (listed_count listed-count)
  (description description)
  (screen_name screen-name)
  (utc_offset utc-offset)
  (notifications notificationsp #'twitching-json-truth-value)
  (favourites_count favorites-count)
  (statuses_count statuses-count)
  (is_translator translatorp #'twitching-json-truth-value)
  (name name)
  (profile_image_url profile-image-url)
  (friends_count friends-count)
  (time_zone time-zone)
  (follow_request_sent follow-request-sent-p #'twitching-json-truth-value)
  (protected protectedp #'twitching-json-truth-value)
  (followers_count followers-count))

;;; Defines a twitter-entity
(twitching-defstruct twitching-entity
  (hashtags hashtags)
  (user_mentions user-mentions)
  (urls urls))

;;; Defines a twitter-status
(twitching-defstruct twitching-status
  (created_at created-at)
  (retweeted retweetedp #'twitching-json-truth-value)
  (contributors contributors)
  (in_reply_to_status_id_str in-reply-to-status-id)
  (source source)
  (in_reply_to_screen_name in-reply-to-screen-name)
  (retweet_count retweet-count)
  (favorited favoritedp #'twitching-json-truth-value)
  (place place)
  (id_str id)
  (entities entities #'new-twitching-entity)
  (text text)
  (truncated truncatedp #'twitching-json-truth-value)
  (user user #'new-twitching-user)
  (geo geo)
  (coordinates coordinates))


;;; overlay
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

(defvar *twitching-status-line-category*
  (put '*twitching-status-line-category* 'face '((:weight normal)
                                                 (:background "white")
                                                 (:foreground "red4"))))

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

(defvar *twitching-star* (propertize "\x2605\ "
                                     'category '*twitching-star-category*)
  "String used to represent starred tweets.")

(defvar *twitching-retweet*
  (propertize "\x27F3\ " 'face '((:background "GreenYellow")))
  "String used to represent retweeted tweets.")

(defvar *twitching-newline* (propertize "\n"
                                        'face '((:background "white")
                                                (:foreground "white"))))

(defvar *twitching-fill-column* 70 "Set this to manipulate `fill-column'.")

(defun twitching-overlay-on-line ()
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (twitching-overlay start end)))

(defun twitching-overlay (start end)
  "Renders the line as a tweet specified by the region in START
  and END."
  (let* ((tweet-str (buffer-substring-no-properties start end))
         (status (read tweet-str))
         (line1 (twitching-decorate-title-text status))
         (line2 (twitching-decorate-status-text status))
         (newline *twitching-newline*)
         (display (concat line1 newline line2 newline))
         (overlay (make-overlay start end)))
    (overlay-put overlay 'tweet status)
    (overlay-put overlay 'display display)))

(defun twitching-decorate-title-text (status)
  (let* ((created-at (twitching-status-created-at status))
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
    (when favoritedp (setq line (concat line sep *twitching-star*)))
    (when retweetedp (setq line (concat *twitching-retweet* sep line)))
    line))

(defun twitching-decorate-status-text (status)
  "Decorates the status text."
  (let* ((text (twitching-status-text status))
         (entity (twitching-status-entities status))
         (urls (twitching-entity-urls entity))
         (mentions (twitching-entity-user-mentions entity))
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


;;; Define `twitching-mode'
(defvar twitching-mode-hook '()
  "Mode hook.")

;; keymap
(defvar twitching-mode-map
  (let ((keymap (make-keymap))
        (nodigits t))
    (suppress-keymap keymap nodigits)
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
    (define-key keymap (kbd "q") 'bury-buffer)
    (define-key keymap (kbd "SPC") 'twitching-page-down)
    (define-key keymap (kbd "<backspace>") 'twitching-page-up)
    keymap))

(define-derived-mode twitching-mode nil "Twitching"
  "Major mode for viewing tweets. \\{twitching-mode-map}")


;;; Mode interactive functions
(defun twitching-layout (point)
  (let* ((overlays (overlays-at point))
         (overlay (car overlays))
         (tweet (and overlay (overlay-get overlay 'tweet))))
    (when (and (not tweet) (not (eobp)))
      (mapc #'delete-overlay overlays)
      (twitching-overlay-on-line))))

(defun twitching-next-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (twitching-layout (point))            ; layout current line
  (let ((pos (plusp n))
        (n (abs n)))
    
    (dotimes (i n)                      ; layout all interim lines
      (goto-line (funcall (if pos #'1+ #'1-) (line-number-at-pos)))
      (twitching-layout (point)))))

(defun twitching-prev-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (twitching-next-tweet (- n)))

(defun twitching-open-link (n)
  "Open the N th url in tweet.  Ignored if tweet has no urls."
  (interactive "P")
  (when (eolp) (beginning-of-line))
  (let* ((n (or n 0))
         (overlays (overlays-at (point)))
         (fn (lambda (o) (overlay-get o 'tweet)))
         (overlays (mapcar fn overlays))
         (tweet (car overlays)))
    (when tweet
      (let* ((entity (twitching-status-entities tweet))
             (urls (twitching-entity-urls entity))
             (url (nth n urls)))
        (if url
            (funcall browse-url-browser-function (cdr (assoc 'url url)))
          (message "No URLs in this tweet."))))))

(defun twitching-favorite-tweet (point)
  "Favorite or unfavorite the tweet at POINT."
  (interactive "d")
  (beginning-of-line)
  (let* ((overlay (car (overlays-at (point))))
         (tweet (if overlay (overlay-get overlay 'tweet)))
         new-tweet)
    (if tweet
        (progn
          (delete-overlay overlay)
          (setq new-tweet (twitching-star-tweet tweet))
          (when new-tweet
            (let ((start (progn (beginning-of-line) (point)))
                  (end (progn (end-of-line) (point))))
              (delete-region start end)
              (insert (format "%S" new-tweet))
              (goto-char start)
              (twitching-layout start)
              (goto-char start))))
      (message "No tweet at point."))))

(defun twitching-create-filter ()
  "Create a twitter filter."
  (interactive))

(defun twitching-page-down (n)
  "Scroll down N pages."
  (interactive "p")
  (scroll-up n)
  (beginning-of-line))

(defun twitching-page-up (n)
  "Scroll up N pages."
  (interactive "p")
  (scroll-down n)
  (beginning-of-line))


;;; Twitter API interactions
(defvar *twitching-user-dir*
  (expand-file-name (convert-standard-filename "~/.emacs.d/twitching"))
  "Name of the directory where the user's tweets are stored.")

(defvar *twitching-consumer-key* nil "Set the twitter consumer key here.")

(defvar *twitching-consumer-secret* nil "Set the twitter consumer secret here.")

(defvar *twitching-access-token* nil "Set the twitter access key here.")

(defvar *twitching-access-token-secret* nil
  "Set the twitter access token secret here.")

(defvar *twitching-oauth-access-token* nil "OAuth access token.")

(defvar *twitching-since-id* nil "Last status-id received from twitter.")

(defvar *twitching-count* nil "Number of tweets to fetch")

(defvar *twitching-include-entities* t "sets include_entities to 1 or 0")

(defun twitching-get-home-timeline ()
  "Gets the current user's home timeline as a list of
`twitching-status'es."
  (twitching-check-keys)
  (let* ((url "http://api.twitter.com/1/statuses/home_timeline.json")
         (params (twitching-default-params))
         (statuses (twitching-get-statuses url params)))
    (when statuses
      (let ((highest (reduce (lambda (x y) (if (string-lessp x y) y x))
                             statuses :key #'twitching-status-id)))
        (setq *twitching-since-id* highest)))
    statuses))

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
  "Main business logic method to get twitter statuses.

Makes a call to URL with PARAMS-ALIST added to the query-string.

METHOD determines the http method GET or POST.  Default
is GET."
  (let ((response (twitching-oauth-get-http-response
                   url params-alist (or method "GET"))))
    (when (twitching-request-success-p response)
      (let ((response-body (twitching-extract-response-body response))
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
             (status (json-read-from-string body)))
        (new-twitching-status status)))))

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
  "Returns t if RESPONSE contains 'HTTP/1.1 200 OK'"
  ;; This is poorly implemented.  Splitting the entire response just
  ;; to check the first line is very inefficient.
  (let* ((resp response)
         (lines (split-string resp "[\n]" t)))
    (string= (car lines) "HTTP/1.1 200 OK")))

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
  (unless (string-ends-with-p url "?") (setq url (concat url "?")))
  (dolist (param params-alist url)
    (setq url (concat url
                      (url-insert-entities-in-string2 (car param))
                      "="
                      (url-insert-entities-in-string2 (cdr param))
                      "&"))))

(defun twitching-default-params ()
  "Returns parameters since_id, count etc., needed for methods
that return statuses."
  (let (params)
    (when *twitching-since-id*
      (setq params (acons "since_id" (format "%s" *twitching-since-id*) params)))
    (when *twitching-count*
      (setq params (acons "count" (format "%s" *twitching-count*) params)))
    (when *twitching-include-entities*
      (setq params (acons "include_entities" "1" params)))
    params))

(defun twitching-json-truth-value (val)
  "Returns t or nil depending upon the json truth value of VAL."
  (case val
    (:json-false nil)
    (:json-true t)
    (t val)))


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

(defun string-ends-with-p (string substring)
  "Return t if STRING ends with SUBSTRING"
  (let ((string-len (length string))
        (substring-len (length substring)))
    (if (> substring-len string-len)
        nil
      (let ((end (substring string (- string-len substring-len))))
        (string= substring end)))))

(defun string-starts-with-p (string substring)
  "Return t if STRING starts with SUBSTRING."
  (let ((string-len (length string))
        (substr-len (length substring)))
    (if (> substr-len string-len)
        nil
      (let ((start (substring string 0 substr-len)))
        (string= substring start)))))

(defun url-insert-entities-in-string2 (s)
  "Same as url-insert-entities-in-string, but in addition,
  replaces spaces with %20"
  (replace-regexp-in-string " " "%20" (url-insert-entities-in-string s) nil))

(provide 'twitching)

;;; twitching.el ends here
