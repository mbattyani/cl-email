(in-package #:cl-user)

;;; First change this and maybe some other parameters
(defparameter email:*smtp-server* "your-smtp-server.com")
(defparameter email:*credentials* '("username" "password"))

;;; A first simple test. Put yourself as a recipient!

(email:with-email (:ssl nil)
  (email:set-subject "test email from common lisp")
  (email:add-recipient "yourself@something.com")
  (format email:*email-stream* "this is a test"))
