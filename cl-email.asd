; -*- mode:common-lisp -*-

;;; cl-email copyright 2015 Marc Battyani and contributors see LICENSE for the details
;;; The source repository of cl-email is here: https://github.com/mbattyani/cl-email

(in-package #:asdf)

(defsystem :cl-email
  :name "cl-email"
  :version "0.9"
  :description "a small common lisp library to easily compose emails"
  :licence "Apache 2.0"
  :perform (load-op :after (op cl-email)
                    (pushnew :cl-email *features*))
  :components ((:file "package")
               (:file "email" :depends-on ("package")))
  :depends-on (#:cl-smtp #:cl-base64))
