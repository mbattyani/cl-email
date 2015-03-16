;;; cl-email copyright 2015 Marc Battyani and contributors see LICENSE for the details
;;; The source repository of cl-email is here: https://github.com/mbattyani/cl-email

(in-package #:cl-user)

(defpackage #:email
  (:use :common-lisp :cl-smtp)
  (:export #:with-email
           #:*smtp-server*
           #:*smtp-port*
           #:*use-ssl*
           #:*credentials*
           #:*external-format*
           #:*from*
           #:*display-name*
           #:*envelope-sender*
           #:*reply-to*
           #:*local-host-name*
           #:*email-stream*
           #:*email-subject*
           #:*email-to*
           #:*email-cc*
           #:*email-bcc*
           #:with-email
           #:set-subject
           #:add-recipient
           #:add-cc
           #:add-bcc))
