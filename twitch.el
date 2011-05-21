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

(defstruct twitter-user
  utc-offset
  id
  screen-name
  name
  profile-image-url
  listed-count
  following
  website
  time-zone
  tweets-count
  followers-count
  location
  description
  friends-count
  verified
  geo-enabled
  lang)

(defstruct twitter-status
  id
  retweet-count
  user
  created-at
  in-reply-to-status-id
  in-reply-to-user-id
  source
  truncatedp
  geo
  coordinates
  contributors
  text
  entities)

;;;###autoload
(defun twitch-get-home-timeline ()
  "Gets the current user's home timeline."
  (interactive)
  (twitch-check-keys)
  (let ((url "http://api.twitter.com/1/statuses/home_timeline.json")
        (authenticate-p t)
        (params (twitch-default-params)))
    (twitch-get-statuses url
                         params
                         authenticate-p)))

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
         (req (oauth-make-request url
                                  (oauth-access-token-consumer-key access-token)
                                  (oauth-access-token-auth-t access-token))))
    (setf (oauth-request-http-method req) (or method "GET"))
    (oauth-sign-request-hmac-sha1 req (oauth-access-token-consumer-secret
                                       access-token))
    (oauth-request-to-header req)
    (let* ((url-request-extra-headers (if url-request-extra-headers 
                                          (append url-request-extra-headers 
                                                  (oauth-request-to-header req))
                                        (oauth-request-to-header req)))
           (url-request-method (oauth-request-http-method req))
           response)
      (url-retrieve (oauth-request-url req)
                    (lambda (status)
                      (setq response (buffer-string))))
      (while (null response)
        (sleep-for 1))
      (when (twitch-request-success-p response)
        (let ((response-body (twitch-extract-response-body response))
              (json-array-type 'list))
          (let ((statuses (json-read-from-string response-body)))
            statuses))))))

(defun twitch-request-success-p (response)
  "Returns t if RESPONSE contains 'HTTP/1.1 200 OK'"
  (let* ((resp *twitch-response*)
         (lines (split-string resp "[\n]" t)))
    (string= (car lines) "HTTP/1.1 200 OK")))

(defun twitch-extract-response-body (response)
  "Extracts the response body, ignoring the headers from
RESPONSE."
  (let ((content-start (string-match "\n\n" response)))
    (when (>= content-start 0)
      (let ((content (substring response (+ content-start 2)))
            (json-array-type 'list))
        (json-read-from-string content)))))

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

;;; twitch.el ends here
