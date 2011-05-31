; -*- mode: emacs-lisp; paredit-mode: t; -*-
;;; twitch.el --- Twitter client library.
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
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to the
;;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.

(require 'cl)
(require 'url)
(require 'url-util)
(require 'hmac-sha1)
(require 'oauth)
(require 'json)

(defvar *twitch-consumer-key* nil "Set the twitter consumer key here.")

(defvar *twitch-access-token* nil "Set the twitter access key here.")

(defvar *twitch-consumer-secret* nil "Set the twitter consumer secret here.")

(defvar *twitch-access-token-secret* nil "Set the twitter access token secret here.")

(defvar *twitch-since-id* nil "Last status-id received from twitter.")

(defvar *twitch-count* nil "Number of tweets to fetch")

(defvar *twitch-include-entities* t "sets include_entities to 1 or 0")

(defmacro twitch-defstruct (struct-name &rest fields)
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
                      (let ((json-field (car field))
                            (struct-field (intern (concat ":" (symbol-name (cadr field)))))
                            (key-fn (caddr field)))
                        (if key-fn
                            `(,struct-field (funcall ,key-fn (cdr (assoc ',json-field json-object))))
                          `(,struct-field (cdr (assoc ',json-field json-object))))))
                    fields))))))

;;; Defines a twitter-user
(twitch-defstruct twitch-twitter-user
  (created_at created-at)
  (geo_enabled geo-enabled-p #'twitch-json-truth-value)
  (verified verifiedp #'twitch-json-truth-value)
  (following followingp #'twitch-json-truth-value)
  (contributors_enabled contributors-enabled-p #'twitch-json-truth-value)
  (url url)
  (location location)
  (id_str id)
  (lang lang)
  (listed_count listed-count)
  (description description)
  (screen_name screen-name)
  (utc_offset utc-offset)
  (notifications notificationsp #'twitch-json-truth-value)
  (favourites_count favorites-count)
  (statuses_count statuses-count)
  (is_translator translatorp #'twitch-json-truth-value)
  (name name)
  (profile_image_url profile-image-url)
  (friends_count friends-count)
  (time_zone time-zone)
  (follow_request_sent follow-request-sent-p #'twitch-json-truth-value)
  (protected protectedp #'twitch-json-truth-value)
  (followers_count followers-count))

;;; Defines a twitter-entity
(twitch-defstruct twitch-twitter-entity
  (hashtags hashtags)
  (user_mentions user-mentions)
  (urls urls))

;;; Defines a twitter-status
(twitch-defstruct twitch-twitter-status
  (created_at created-at)
  (retweeted retweetedp #'twitch-json-truth-value)
  (contributors contributors)
  (in_reply_to_status_id_str in-reply-to-status-id)
  (source source)
  (in_reply_to_screen_name in-reply-to-screen-name)
  (retweet_count retweet-count)
  (favorited favoritedp #'twitch-json-truth-value)
  (place place)
  (id_str id)
  (entities entities #'new-twitch-twitter-entity)
  (text text)
  (truncated truncatedp #'twitch-json-truth-value)
  (user user #'new-twitch-twitter-user)
  (geo geo)
  (coordinates coordinates))

;;;###autoload
(defun twitch-get-home-timeline ()
  "Gets the current user's home timeline as a list of
`twitch-twitter-status'es."
  (interactive)
  (twitch-check-keys)
  (let ((url "http://api.twitter.com/1/statuses/home_timeline.json")
        (authenticatep t)
        (params (twitch-default-params)))
    (twitch-get-statuses url
                         params
                         authenticatep)))

(defun twitch-check-keys ()
  "Checks if *twitch-consumer-key* *twitch-consumer-secret*
*twitch-access-token* *twitch-access-token-secret* have been set.
Requests user input if they haven't."
  (unless *twitch-consumer-key*
    (setq *twitch-consumer-key*
          (read-string "Enter consumer key: ")))
  (unless *twitch-consumer-secret*
    (setq *twitch-consumer-secret*
          (read-string "Enter consumer secret: ")))
  (unless *twitch-access-token*
    (setq *twitch-access-token*
          (read-string "Enter access token: ")))
  (unless *twitch-access-token-secret*
    (setq *twitch-access-token-secret*
          (read-string "Enter access token secret: "))))

(defun twitch-get-statuses (url params-alist authenticatep &optional method)
  "Main business logic method.

Makes a call to URL with PARAMS-ALIST added to the query-string.

AUTHENTICATEP is t then authentication is enabled.  Currently
unused.

METHOD determines the http method GET or POST.  Default
is GET."
  (let* ((url (twitch-form-url url params-alist))
         (access-token (make-oauth-access-token
                        :consumer-key *twitch-consumer-key*
                        :consumer-secret *twitch-consumer-secret*
                        :auth-t (make-oauth-t
                                 :token *twitch-access-token*
                                 :token-secret *twitch-access-token-secret*)))
         (req (oauth-make-request
               url
               (oauth-access-token-consumer-key access-token)
               (oauth-access-token-auth-t access-token))))
    (setf (oauth-request-http-method req) (or method "GET"))
    (oauth-sign-request-hmac-sha1 req (oauth-access-token-consumer-secret
                                       access-token))
    (let* ((url (oauth-request-url req))
           (headers (oauth-request-to-header req))
           (request-method (oauth-request-http-method req))
           (response (url-retrieve-synchronously-as-string
                      url headers request-method)))
      (when (twitch-request-success-p response)
        (let ((response-body (twitch-extract-response-body response))
              (json-array-type 'list))
          (let ((statuses (json-read-from-string response-body)))
            (mapcar #'new-twitch-twitter-status statuses)))))))

(defun url-retrieve-synchronously-as-string (url &optional headers request-method)
  "Retrieves the contents of URL and returns the response as a
  string.  Passes HEADERS with the request and the request is
  made as specified in REQUEST-METHOD.  By default REQUEST-METHOD
  is GET."
  (let ((url-request-extra-headers (if url-request-extra-headers 
                                       (append url-request-extra-headers headers)
                                     headers))
        (url-request-method (or request-method "GET"))
        request)
    (url-retrieve url (lambda (s) (setq response (or (buffer-string) ""))))
    (while (null response) (sleep-for 1))
    response))

(defun twitch-request-success-p (response)
  "Returns t if RESPONSE contains 'HTTP/1.1 200 OK'"
  (let* ((resp response)
         (lines (split-string resp "[\n]" t)))
    (string= (car lines) "HTTP/1.1 200 OK")))

(defun twitch-extract-response-body (response)
  "Extracts the response body, ignoring the headers from
RESPONSE."
  (let ((content-start (string-match "\n\n" response)))
    (when (>= content-start 0)
      (let ((content (substring response (+ content-start 2)))
            (json-array-type 'list))
        content))))

(defun twitch-form-url (url params-alist)
  "Form the full url with its query parameters from URL and PARAMS-ALIST"
  (unless (string-ends-with-p url "?") (setq url (concat url "?")))
  (dolist (param params-alist url)
    (setq url (concat url
                      (twitch-url-encode (car param))
                      "="
                      (twitch-url-encode (cdr param))
                      "&"))))

(defun twitch-default-params ()
  "Returns parameters since_id, count etc., needed for methods
that return statuses."
  (let (params)
    (when *twitch-since-id*
      (setq params (acons "since_id" (format "%s" *twitch-since-id*) params)))
    (when *twitch-count*
      (setq params (acons "count" (format "%s" *twitch-count*) params)))
    (when *twitch-include-entities*
      (setq params (acons "include_entities" "1" params)))
    params))

(defun twitch-url-encode (string)
  "Same as url-insert-entities-in-string, but in addition,
  replaces spaces with %20"
  (replace-regexp-in-string " " "%20" (url-insert-entities-in-string string) nil))

(defun twitch-url-decode (encoded-string)
  "Inverse of url-insert-entities-in-string

Converts HTML entity references in ENCODED-STRING.  Returns a new
string with the result of the conversion.  Replaces these strings
as follows:
    &amp;  ==> &
    &lt;   ==> <
    &gt;   ==> >
    &quot; ==> \""
  (reduce #'(lambda (str escape)
              (replace-regexp-in-string (car escape) (cdr escape) str nil))
          '(("&quot;" . "\"") ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("%20" . " "))
          :from-end nil
          :initial-value encoded-string))

(defun twitch-json-truth-value (val)
  "Returns t or nil depending upon the json truth value of VAL."
  (cond (:json-false nil)
        (:json-true t)
        nil))

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

;; ********* TESTS *********

(when nil
  (let* ((original-string "5 > 3 && 3 < 4")
         (encoded-string (twitch-url-encode original-string))
         (decoded-string (twitch-url-decode encoded-string)))
    (assert (string-equal original-string decoded-string) t)))

;; TESTS end here

(provide 'twitch)

;;; twitch.el ends here
