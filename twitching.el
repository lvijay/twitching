; -*- mode: emacs-lisp; paredit-mode: t; -*-
;;; twitching.el --- Twitter client library.
;;;
;;; Copyright (C) 2011 Vijay Lakshminarayanan
;;;
;;; Author: Vijay Lakshminarayanan <laksvij AT gmail.com>
;;; Version: 0.0.1
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

(defun twitching-home-timeline-get ()
  "Fetch home timeline."
  (interactive)
  (let ((twitching-buffer (get-buffer-create "*Twitching*")))
    (with-current-buffer twitching-buffer
      (setq major-mode 'twitching-mode)
      (save-excursion
        (let ((starting-line (line-number-at-pos))
              ending-line)
          (goto-char (point-max))
          (mapcar (lambda (tweet)
                    (let* ((tweet-string (format "%S" tweet))
                           (tweet (replace-regexp-in-string "[\r\n]" " "
                                                            tweet-string)))
                      (insert (concat tweet "\n"))))
                  (twitching-get-home-timeline))
          (setq ending-line (line-number-at-pos))
          (twitching-prev-tweet (- ending-line starting-line))))))
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


;;; struct definitions
(defmacro twitching-defstruct (struct-name &rest fields)
  "Macro that defines structures representing twitter responses.
STRUCT-NAME is the name of the represented object.

FIELDS is a list of triples of the form (json-object-field-name
struct-field-name key-function).  key-function is optional and by
default is IDENTITY.  If provided, it must be a function that
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
         (,constructor-name
          .,(mapcan (lambda (field)
                      (let* ((json-field (first field))
                             (symbol-name (symbol-name (second field)))
                             (keyword-field (intern (concat ":" symbol-name)))
                             (struct-field keyword-field)
                             (key-fn (third field)))
                        (if key-fn
                            `(,struct-field (funcall ,key-fn (cdr (assoc ',json-field json-object))))
                          `(,struct-field (cdr (assoc ',json-field json-object))))))
                    fields))))))

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
(defvar *twitching-top-line-category*
  (put '*twitching-top-line-category* 'face '((:weight bold)
                                              (:slant italic)
                                              (:background "white")
                                              (:foreground "chocolate"))))

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

(defvar *twitching-empty-space-category*
  (put '*twitching-empty-space-category* 'face '((:background "white")
                                                 (:foreground "black")
                                                 (:underline t)
                                                 (:overline t))))

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
         (text (twitching-status-text status))
         (created-at (twitching-status-created-at status))
         (user (twitching-status-user status))
         (screen-name (twitching-user-screen-name user))
         (user-name (twitching-user-name user))
         (overlay (make-overlay start end))
         (newline (propertize "\n" 'face '((:background "white")
                                           (:foreground "black"))))
         (spaces (twitching-spaces))
         (sep " | ")
         (line1 (propertize (concat screen-name sep user-name sep created-at)
                            'category '*twitching-top-line-category*))
         (line2 (twitching-decorate-status-text status))
         (display (concat line1 newline line2 newline spaces)))
    (overlay-put overlay 'tweet status)
    (overlay-put overlay 'display display)))

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

(defun twitching-spaces ()
  (let* ((result '("")))
    (dotimes (i fill-column) (push " " result))
    (propertize (apply #'concat result)
                'category '*twitching-empty-space-category*)))


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
    (define-key keymap (kbd "o") 'twitching-open-link)
    keymap))

(define-derived-mode twitching-mode nil "Twitching"
  "Major mode for viewing tweets. \\{twitching-mode-map}")


;;; Mode interactive functions
(defun twitching-next-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (flet ((layout ()
           (let* ((overlays (overlays-at (point)))
                  (fn (lambda (o) (overlay-get o 'tweet)))
                  (twt-ovrlys (mapcar fn overlays))
                  (tweet (find t twt-ovrlys :test (lambda (x y) (and x y)))))
             (when (and (not tweet) (not (eobp)))
               (mapcar #'delete-overlay overlays)
               (twitching-overlay-on-line)))))
    (layout)                            ; layout current line
    (let ((pos (plusp n))
          (n (abs n)))
      (dotimes (i n)                    ; layout all interim lines
        (goto-line (funcall (if pos #'1+ #'1-) (line-number-at-pos)))
        (layout)))))

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

(defun twitching-create-filter ()
  "Create a twitter filter."
  (interactive))


;;; Twitter API interactions
(defvar *twitching-user-dir*
  (expand-file-name (convert-standard-filename "~/.emacs.d/twitching"))
  "Name of the directory where the user's tweets are stored.")

(defvar *twitching-consumer-key* nil "Set the twitter consumer key here.")

(defvar *twitching-access-token* nil "Set the twitter access key here.")

(defvar *twitching-consumer-secret* nil "Set the twitter consumer secret here.")

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
  (let ((url "http://api.twitter.com/1/statuses/home_timeline.json")
        (params (twitching-default-params)))
    (let ((statuses (twitching-get-statuses url
                                         params)))
      (when statuses
        (let ((highest (reduce (lambda (x y) (if (string-lessp x y) y x))
                               statuses :key #'twitching-status-id)))
          (when highest (setq *twitching-since-id* highest))))
      statuses)))

(defun twitching-check-keys ()
  "Checks if *twitching-consumer-key* *twitching-consumer-secret*
*twitching-access-token* *twitching-access-token-secret* have been set.
Requests user input if they haven't."
  (unless *twitching-consumer-key*
    (setq *twitching-consumer-key*
          (read-string "Enter consumer key: ")))
  (unless *twitching-consumer-secret*
    (setq *twitching-consumer-secret*
          (read-string "Enter consumer secret: ")))
  (unless *twitching-access-token*
    (setq *twitching-access-token*
          (read-string "Enter access token: ")))
  (unless *twitching-access-token-secret*
    (setq *twitching-access-token-secret*
          (read-string "Enter access token secret: ")))
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

(defun twitching-oauth-get-http-response (url params method)
  "Form an oauth request from URL with PARAMS and METHOD and
return the response as a string."
  (let* ((url (twitching-form-url url params-alist))
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

(defun twitching-request-success-p (response)
  "Returns t if RESPONSE contains 'HTTP/1.1 200 OK'"
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
                      (twitching-url-encode (car param))
                      "="
                      (twitching-url-encode (cdr param))
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
    (t nil)))


;;; General utility functions that should probably be elsewhere.
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

(defun twitching-url-encode (string)
  "Same as url-insert-entities-in-string, but in addition,
  replaces spaces with %20"
  (replace-regexp-in-string " " "%20" (url-insert-entities-in-string string) nil))

;;; twitching.el ends here
