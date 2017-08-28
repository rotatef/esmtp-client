;;;;  cl-esmtp-client
;;;;
;;;;  Copyright (C) 2017 Thomas Bakketun <thomas.bakketun@copyleft.no>
;;;;
;;;;  This library is free software: you can redistribute it and/or modify
;;;;  it under the terms of the GNU Lesser General Public License as published
;;;;  by the Free Software Foundation, either version 3 of the License, or
;;;;  (at your option) any later version.
;;;;
;;;;  This library is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU General Public License
;;;;  along with this library.  If not, see <http://www.gnu.org/licenses/>.


(in-package #:cl-esmtp-client)


(defvar *session*)


(define-condition protocol-error (error)
  ((session :initarg :session
            :reader session)
   (expected-reply-code :initarg :expected-reply-code
               :reader expected-reply-code)
   (reply-code :initarg :reply-code
               :reader reply-code)
   (command-line :initarg :command-line
                 :reader command-line)
   (reply-lines :initarg :reply-lines
                :reader reply-lines))
  (:report (lambda (condition stream)
             (print-unreadable-object (condition stream :type t)
               (format stream "~S: ~:[Got unparsable reply~;Got reply code ~:*~A~], expected ~A.~@[~%C: ~A~]~{~%S: ~A~}"
                       (session condition)
                       (reply-code condition)
                       (expected-reply-code condition)
                       (command-line condition)
                       (reply-lines condition))))))


(define-condition transient-error (protocol-error)
  ())


(define-condition permanent-error (protocol-error)
  ())


(defclass client ()
  ((host :initarg :host
         :reader host)
   (port :initarg :port
         :reader port)
   (ssl :initarg :ssl
        :reader ssl
        :initform nil)
   (ssl-options :initarg :ssl-options
                :reader ssl-options
                :initform nil)
   (auth-method :initarg :auth-method
                :reader auth-method
                :initform nil)
   (credentials :initarg :credentials
                :reader credentials
                :initform nil)
   (local-name :initarg :local-name
               :reader local-name
               :initform (machine-instance))))


(defmethod initialize-instance :after ((client client) &rest args)
  (declare (ignore args))
  (assert (slot-boundp client 'host) () "SMTP host name missing.")
  (unless (slot-boundp client 'port)
    (setf (slot-value client 'port) (if (ssl client) 465 587)))
  (when (credentials client)
    (unless (auth-method client)
      (setf (slot-value client 'auth-method) :plain))))



(defclass session ()
  ((client :initarg :client
           :reader client)
   (binary-stream :initarg :binary-stream
                  :accessor binary-stream)
   (tls-stream :accessor tls-stream :initform nil)
   (text-stream :initarg :stream
                :accessor text-stream)
   (greeting :accessor greeting)
   (hello-greeting :accessor hello-greeting)
   (features :accessor features
             :initform nil)
   (max-size :accessor max-size
             :initform nil)
   (trace-stream :initarg :trace-stream
                 :accessor trace-stream)
   (at-newline :initform t
               :accessor at-newline)
   (data-lines-count :accessor data-lines-count)
   (data-bytes-count :accessor data-bytes-count)))


(defvar *suppress-trace-column* nil)
(defun trace-log (prefix data)
  (when (trace-stream *session*)
    (when *suppress-trace-column*
      (setf data (fill (copy-seq data) #\* :start *suppress-trace-column*)))
    (format (trace-stream *session*) "~&~:[ ~;*~]~A: ~A" (secure-connection-p) prefix data)
    (finish-output (trace-stream *session*))))


(defmacro with-session ((client &key trace) &body body)
  `(do-with-session :client ,client
                    :trace ,trace
                    :function (lambda ()
                                ,@body)))


(defun do-with-session (&key client function trace)
  (usocket:with-client-socket (socket stream (host client) (port client)
                                      :element-type '(unsigned-byte 8))
    (let ((*session* (make-instance 'session
                                    :client client
                                    :trace-stream trace)))
      (setup-session stream)
      (multiple-value-prog1 (funcall function)
        (quit)))))


(defun setup-session (binary-stream)
  (setf (binary-stream *session*) binary-stream)
  (if (ssl (client *session*))
      (make-connection-secure)
      (setup-text-stream))
  (handshake))


(defun make-connection-secure ()
  (setf (tls-stream *session*) (apply #'cl+ssl:make-ssl-client-stream
                                      (cl+ssl:stream-fd (binary-stream *session*))
                                      (ssl-options (client *session*))))
  (setup-text-stream))


(defun setup-text-stream ()
  (setf (text-stream *session*)
        (flexi-streams:make-flexi-stream (or (tls-stream *session*)
                                             (binary-stream *session*))
                                         :external-format
                                         (flexi-streams:make-external-format
                                          :iso-8859-1 :eol-style :crlf))))


(defun handshake ()
  (read-greeting)
  (ehlo-or-helo)
  (starttls)
  (authenticate))


(defun secure-connection-p ()
  (tls-stream *session*))


(defun feature (feature)
  (assoc feature (features *session*)))


(defun read-reply ()
  "Returns three values, reply code, first reply line, list with remaining lines."
  (values-list
   (loop for line = (read-line (text-stream *session*))
         for long = (< 3 (length line))
         for more = (and long (char= (char line 3) #\-))
         for code = (parse-integer line :end 3 :junk-allowed t)
         for text = (if long (subseq line 4) "")
         collect line into raw-lines
         collect text into texts
         do (let ((*suppress-trace-column* nil))
              (trace-log :s line))
         while (and code more)
         finally (return (list code (first texts) (rest texts) raw-lines)))))


(defun check-reply (command-line expected-code)
  (multiple-value-bind (code first-line rest-lines raw-lines)
      (read-reply)
    (unless (eql code expected-code)
      (error (if (and (integerp code)
                      (= 4 (floor code 100)))
                 'transient-error
                 'permanent-error)
             :session *session*
             :reply-code code
             :expected-reply-code expected-code
             :command-line command-line
             :reply-lines raw-lines))
    (values first-line rest-lines)))


(defun valid-char-p (char)
  (or (char= #\Tab char)
      (<= 32 (char-code char) 127)))


(defun valid-line-p (line)
  (and (<= (length line) 998)
       (every #'valid-char-p line)))


(defun send-to-server (line)
  (assert (valid-line-p line) () "The line contains invalid characters or is too long: ~S" line)
  (write-line line (text-stream *session*))
  (finish-output (text-stream *session*)))


(defun send-command (expected-code format &rest args)
  (let ((command (apply #'format nil format args)))
    (trace-log :c command)
    (send-to-server command)
    (when expected-code
      (check-reply command expected-code))))


(defun read-greeting ()
  (setf (greeting *session*) (check-reply nil 220)))


(defun quit ()
  (send-command 221 "QUIT"))


(defun parse-feature (feature)
  (let ((name-end (position #\Space feature)))
    (cons (intern (string-upcase (subseq feature 0 name-end)) :keyword)
          (when name-end
            (subseq feature (1+ name-end))))))


(defun register-feature (feature)
  (case (car feature)
    (:size
     (when (cdr feature)
       (setf (max-size *session*) (parse-integer (cdr feature)))))))


(defun ehlo ()
  (multiple-value-bind (ehlo-greeting features)
      (send-command 250 "EHLO ~A" (local-name (client *session*)))
    (setf (hello-greeting *session*) ehlo-greeting)
    (setf (features *session*) (mapcar #'parse-feature features))))


(defun helo ()
  (setf (hello-greeting *session*)
        (send-command 250 "HELO ~A" (local-name (client *session*)))))


(defun ehlo-or-helo ()
  (handler-case (ehlo)
    (protocol-error ()
      (helo))))


(defun starttls ()
  (when (and (not (secure-connection-p))
             (feature :starttls))
    (send-command 220 "STARTTLS")
    (make-connection-secure)
    (ehlo-or-helo)))


(defun mail-from (address &key size)
  (send-command 250 "MAIL FROM:<~A>~@[ SIZE=~D~]"
                address
                (when (feature :size) size)))


(defun rcpt-to (address)
  (send-command 250 "RCPT TO:<~A>" address))


(defun string-to-utf8-base64 (string)
  (base64:usb8-array-to-base64-string
   (flex:string-to-octets string :external-format :utf-8)))


(defun authenticate ()
  (let ((auth-method (auth-method (client *session*))))
    (when auth-method
      (assert (secure-connection-p) () "Connection is not secure. Refusing to send password.")
      (ecase auth-method
        (:plain
         (auth-plain))
        (:login
         (auth-login))))))


(defun unwrap-secret (secret)
  (if (functionp secret)
      (funcall secret)
      secret))


(defun auth-plain ()
  (let* ((credentials (unwrap-secret (credentials (client *session*))))
         (username (unwrap-secret (first credentials)))
         (password (unwrap-secret (second credentials))))
    (let ((*suppress-trace-column* 11))
      (send-command 235 "AUTH PLAIN ~A"
                    (string-to-utf8-base64
                     (format nil "~A~C~A~C~A"
                             username
                             #\null
                             username
                             #\null
                             password))))))


(defun auth-login ()
  (let* ((credentials (unwrap-secret (credentials (client *session*))))
         (username (unwrap-secret (first credentials)))
         (password (unwrap-secret (second credentials))))
    (send-command 334 "AUTH LOGIN")
    (let ((*suppress-trace-column* 0))
      (send-command 334 (string-to-utf8-base64 username))
      (send-command 235 (string-to-utf8-base64 password)))))


(defun data-start ()
  (setf (data-lines-count *session*) 0
        (data-bytes-count *session*) 0)
  (send-command 354 "DATA"))


(defun data-line (line)
  (incf (data-lines-count *session*))
  (incf (data-bytes-count *session*) (+ (length line) 2))
  (when (and (plusp (length line))
             (char= #\. (char line 0)))
    (princ #\. (text-stream *session*)))
  (send-to-server line))


(defun data-bytes (bytes)
  (flex:with-input-from-sequence (in-bytes bytes)
    (let ((in (flex:make-flexi-stream in-bytes :external-format '(:latin1 :eol-style :crlf))))
      (loop for (line missing-newline-p) = (multiple-value-list (read-line in nil))
            while line
            do
            (when (and (at-newline *session*)
                       (plusp (length line))
                       (char= #\. (char line 0)))
              (princ #\. (text-stream *session*)))
            (princ line (text-stream *session*))
            (incf (data-bytes-count *session*) (length line))
            (unless missing-newline-p
              (incf (data-lines-count *session*))
              (terpri (text-stream *session*))
              (incf (data-bytes-count *session*) 2))
            (setf (at-newline *session*) (not missing-newline-p))))))


(defun data-end ()
  (unless (at-newline *session*)
    (incf (data-lines-count *session*))
    (terpri (text-stream *session*))
    (incf (data-bytes-count *session*) 2))
  (trace-log "-" (format nil "Data sent: ~A lines, ~A bytes."
                         (data-lines-count *session*)
                         (data-bytes-count *session*)))
  (send-command 250 "."))


(defun data (lines)
  (data-start)
  (dolist (line lines)
    (data-line line))
  (data-end))


(defun string-to-lines (data)
  (with-input-from-string (in data)
    (loop for line = (read-line in nil)
          while line
          collect (string-trim '(#\Return) line))))


(defun send-mail (mail-from rcpt-to data)
  (let* ((lines (string-to-lines data))
         (size (+ (reduce #'+ (mapcar #'length lines))
                  (* 2 (length lines)))))
    (mail-from mail-from :size size)
    (if (listp rcpt-to)
        (dolist (rcpt rcpt-to)
          (rcpt-to rcpt))
        (rcpt-to rcpt-to))
    (data lines)))
