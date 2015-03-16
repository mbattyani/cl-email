;;; cl-email copyright 2015 Marc Battyani and contributors see LICENSE for the details
;;; The source repository of cl-email is here: https://github.com/mbattyani/cl-email

(in-package #:email)

;;; Some default parameters because you obviously don't want to retype them all the time
(defparameter *smtp-server* "your-smtp-server.com")
(defparameter *smtp-port* 465)
(defparameter *use-ssl* :tls)
(defparameter *credentials* '("username" "password"))
(defparameter *external-format* :utf-8)
(defparameter *from* "someboby@something.com")
(defparameter *display-name* "Somebody")
(defparameter *envelope-sender* nil)
(defparameter *reply-to* nil)
(defparameter *local-host-name* nil)

(defvar *email-stream* nil)
(defvar *email-subject* "")
(defvar *email-to* nil)
(defvar *email-cc* nil)
(defvar *email-bcc* nil)

;;; I'm sure you too have plenty of with-unique-names and rebinding macros
;; reference implementations posted to comp.lang.lisp as
;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
;; <http://www.cliki.net/Common%20Lisp%20Utilities>

(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(mapcar (lambda (binding)
                   (check-type binding (or cons symbol))
                   (destructuring-bind (var &optional (prefix (symbol-name var)))
                       (if (consp binding) binding (list binding))
                     (check-type var symbol)
                     `(,var (gensym ,(concatenate 'string prefix "-")))))
                 bindings)
     ,@body))

(defmacro rebinding (bindings &body body)
  (loop for binding in bindings
        for var = (car (if (consp binding) binding (list binding)))
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let* ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                               ,,@body))))))

;;; Transform cl-smtp::send-attachment into a generic function
(fmakunbound 'cl-smtp::send-attachment)

;;; the original function
(defmethod cl-smtp::send-attachment (sock attachment boundary buffer-size external-format)
  (cl-smtp::send-attachment-header sock boundary attachment external-format)
  (cl-smtp::base64-encode-file (cl-smtp::attachment-data-pathname attachment) sock :buffer-size buffer-size))

(defclass email-attachment ()
  ((name :initarg :name
	 :accessor attachment-name)
   (data-pathname :initarg :data-pathname
	 :accessor attachment-data-pathname)
   (mime-type :initarg :mime-type :accessor attachment-mime-type)))

(defun set-subject (subject)
  (setf *email-subject* subject))

(defun add-recipient (email-address)
  (push email-address *email-to*))

(defun add-cc (email-address)
  (push email-address *email-to*))

(defun add-bcc (email-address)
  (push email-address *email-to*))

(defmacro with-email ((&key (host '*smtp-server*) (from '*from*)
                            (ssl '*use-ssl*) (port '*smtp-port*)
                            (display-name '*display-name*)
                            (reply-to '*reply-to*)
                            (credentials '*credentials*)
                            (envelope-sender '*envelope-sender*)
                            (external-format '*external-format*)
                            (local-hostname '*local-host-name*))
                      &body body)
  (rebinding (host from ssl port display-name credentials envelope-sender external-format local-hostname)
    (with-unique-names (text-msg html-msg)
      `(let ((,text-msg)
             (*email-subject* "")
             (*email-to* nil)
             (*email-cc* nil)
             (*email-bcc* nil)
             )
         (setf ,text-msg (with-output-to-string (*email-stream*) ,@body))
         (send-email ,host ,from *email-to* *email-subject* ,text-msg
                     :ssl ,ssl :port ,port :cc *email-cc* :bcc *email-bcc* :reply-to ,reply-to
                                        ;extra-headers html-message
                     :display-name ,display-name :authentication ,credentials
;                     attachments (buffer-size 256)
                     :envelope-sender ,envelope-sender
                     :external-format ,external-format :local-hostname ,local-hostname)))))
