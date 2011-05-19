; -*- mode: emacs-lisp; paredit-mode: t; -*-
;;; twitch.el --- Twitter client library.

;;; Copyright (C) 2011 Vijay Lakshminarayanan

;;; Author: Vijay Lakshminarayanan <laksvij AT gmail.com>
;;; Version: 0.0.1
;;; Keywords: twitter
;;; Contributors:

;;; This file is NOT part of GNU Emacs.

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

;;;###autoload
(defun twitch-get-home-timeline ()
  (twitch-get-statuses "http://api.twitter.com/1/statuses/home_timeline.json" (twitch-default-params) t))

(defun twitch-get-statuses (url params-alist authenticate-p)
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
    (setf (oauth-request-http-method req) "GET")
    (oauth-sign-request-hmac-sha1 req (oauth-access-token-consumer-secret access-token))
    (oauth-request-to-header req)
    (let* ((url-request-extra-headers (if url-request-extra-headers 
                                          (append url-request-extra-headers 
                                                  (oauth-request-to-header req))
                                        (oauth-request-to-header req)))
           (url-request-method (oauth-request-http-method req)))
      (view-buffer (url-retrieve-synchronously (oauth-request-url req)))
      )))

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

;; ********* TESTS *********

(when nil
  (let* ((original-string "5 > 3 && 3 < 4")
         (encoded-string (twitch-url-encode original-string))
         (decoded-string (twitch-url-decode encoded-string)))
    (assert (string-equal original-string decoded-string) t)))

;;; twitch.el ends here
