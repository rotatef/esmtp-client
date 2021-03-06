;;;;  esmtp-client
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


(in-package #:esmtp-client)


(defvar *session*)


(define-condition client-error (error)
  ((session :initarg :session
            :reader session)
   (message :initarg :message
            :reader message))
  (:report (lambda (condition stream)
             (print-unreadable-object (condition stream :type t)
               (format stream "ESTMP error: ~A"
                       (message condition))))))


(define-condition protocol-error (client-error)
  ((expected-reply-code :initarg :expected-reply-code
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


(defclass settings ()
  ((host :initarg :host
         :reader host
         :initform nil)
   (port :initarg :port
         :reader port)
   (ssl :initarg :ssl
        :reader sslp)
   (starttls :initarg :starttls
             :reader starttlsp
             :initform t)
   (cl+ssl-options :initarg :cl+ssl-options
                   :reader cl+ssl-options
                   :initform nil)
   (credentials :initarg :credentials
                :reader credentials
                :initform nil)
   (local-name :initarg :local-name
               :reader local-name
               :initform (machine-instance))
   (trace-function :initarg :trace-function
                   :initarg :trace
                   :accessor trace-function
                   :initform nil)))


(defmethod initialize-instance :after ((settings settings) &rest args)
  (declare (ignore args))
  (with-slots (host port credentials trace-function ssl)
      settings
    (assert host () "SMTP host name missing.")
    (let ((port-bound (slot-boundp settings 'port))
          (ssl-bound (slot-boundp settings 'ssl)))
      (cond ((and (not port-bound)
                  (not ssl-bound))
             (setf port 587)
             (setf ssl nil))
            ((and port-bound
                  (not ssl-bound))
             (setf ssl (= port 465)))
            ((and (not port-bound)
                  ssl-bound)
             (setf port (if ssl 465 587)))))
    (setf trace-function
          (etypecase trace-function
            (stream
             (make-trace-to-stream trace-function))
            ((member t)
             (make-trace-to-stream *trace-output*))
            (function
             trace-function)
            (null
             nil)))))


(defclass session ()
  ((settings :initarg :settings)
   (binary-stream :initarg :binary-stream
                  :accessor binary-stream)
   (tls-stream :accessor tls-stream :initform nil)
   (text-stream :initarg :stream
                :accessor text-stream)
   (greeting)
   (hello-greeting)
   (extensions :initform nil)
   (at-newline :initform t
               :accessor at-newline)
   (data-lines-count :accessor data-lines-count)
   (data-bytes-count :accessor data-bytes-count)))


(defun greeting ()
  (slot-value *session* 'greeting))


(defun hello-greeting ()
  (slot-value *session* 'hello-greeting))


(defun extensions ()
  (slot-value *session* 'extensions))


(defun extensionp (extension)
  (assoc extension (extensions)))


(defun settings ()
  (slot-value *session* 'settings))


(defun trace-log (origin data)
  (when (trace-function (settings))
    (funcall (trace-function (settings)) origin data)))


(defun make-trace-to-stream (stream)
  (lambda (origin data)
    (if (eq :i origin)
        (format stream "~&<~A>" data)
        (format stream "~&~A: ~A" origin data))
    (finish-output stream)))


(defmacro with-session (settings &body body)
  "Opens a connetion to an SMTP server, executes the body, closes the connection.

The settings form is evaluated and should return a property list:

:host - The SMTP server hostname as a string.
:port - The TCP/IP port of the SMTP server.
  Default is 587 or 465 if :ssl is true.
:ssl - Establish encrypted SSL/TLS connection immediately.
  Default value is true if port is 465, otherwise false.
:starttls - Establish encrypted SSL/TLS connection using the STARTTLS command.
  Starttls is only used if the server supports it and the connection in not already encrypted.
  Default is true.
:cl+ssl-options - These options are forwared to cl+ssl:make-ssl-client-stream.
:credentials - Specifies login details. See make-credentials.
:local-name - The hostname used in HELO/EHLO commands.
:trace - Enables tracing of the communcation. One of:
  nil - No trace, the default
  t - Trace to *trace-output*
  character stream - Trace to that stream.
  function - A function taking two parameters origin and string. Origin is a keyword:
    :S - Data sent from the server
    :C - Data sent by the client
    :I - Internal event (eg. TLS negotiation complete)

  Some data is not traced, secret data (passwords) and the message data."
  `(do-with-session ,settings (lambda () ,@body)))


(defun do-with-session (settings function)
  (let ((*session* (make-instance 'session :settings (apply #'make-instance 'settings settings))))
    (usocket:with-client-socket (socket stream
                                        (host (settings))
                                        (port (settings))
                                        :element-type '(unsigned-byte 8))
      (setup-session stream)
      (multiple-value-prog1 (funcall function)
        (quit)))))


(defun setup-session (binary-stream)
  (setf (binary-stream *session*) binary-stream)
  (if (sslp (settings))
      (make-connection-secure)
      (setup-text-stream))
  (handshake))


(defun make-connection-secure ()
  (setf (tls-stream *session*) (apply #'cl+ssl:make-ssl-client-stream
                                      (cl+ssl:stream-fd (binary-stream *session*))
                                      `(,@(cl+ssl-options (settings))
                                        :hostname ,(host (settings))
                                        :verify :required)))
  (trace-log :i "TLS negotiation complete")
  (setup-text-stream))


(defun assert-secure-connection ()
  (assert (secure-connection-p) () "Connection is not secure. Refusing to send password."))


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
         do (trace-log :s line)
         while (and code more)
         finally (return (list code (first texts) (rest texts) raw-lines)))))


(defun check-reply (command-line expected-code)
  (multiple-value-bind (code first-line rest-lines raw-lines)
      (read-reply)
    (unless (if (listp expected-code)
                (member code expected-code)
                (eql code expected-code))
      (error (if (and (integerp code)
                      (= 4 (floor code 100)))
                 'transient-error
                 'permanent-error)
             :session *session*
             :reply-code code
             :expected-reply-code expected-code
             :command-line command-line
             :reply-lines raw-lines))
    (if (listp expected-code)
        (values code first-line rest-lines)
        (values first-line rest-lines))))


(defun valid-char-p (char)
  (or (char= #\Tab char)
      (<= 32 (char-code char) 127)))


(defun valid-line-p (line)
  (and (<= (length line) 998)
       (every #'valid-char-p line)))


(defun send-to-server (line &optional secret-data)
  (assert (valid-line-p line) () "The line contains invalid characters or is too long: ~S" line)
  (princ line (text-stream *session*))
  (when secret-data
    (funcall secret-data (text-stream *session*)))
  (terpri (text-stream *session*))
  (finish-output (text-stream *session*)))


(defun send-command (expected-code format &rest args)
  (let* ((last-arg (car (last args)))
         (secret-data (when (functionp last-arg)
                        last-arg))
         (args (if secret-data
                   (butlast args)
                   args))
         (command (apply #'format nil format args))
         (command-debug (if secret-data
                            (format nil "~A<secret data>" command)
                            command)))
    (trace-log :c command-debug)
    (send-to-server command secret-data)
    (if expected-code
        (check-reply command-debug expected-code)
        (read-reply))))


(defun read-greeting ()
  (setf (slot-value *session* 'greeting) (check-reply nil 220)))


(defun quit ()
  (send-command 221 "QUIT"))


(defun noop ()
  (send-command 250 "NOOP"))


(defun rset ()
  (send-command 250 "RSET"))


(defun max-size ()
  (cdr (extensionp :size)))


(defun parse-ehlo-line (line)
  (destructuring-bind (keyword . params)
      (loop for start = 0 then (1+ end)
            while (< start (length line))
            for next-space = (position #\Space line :start start)
            for end = (or next-space (length line))
            when (< 0 (- end start))
              collect (subseq line start end))
    (let ((keyword (intern (string-upcase keyword) :keyword)))
      (cons keyword
            (case keyword
              (:size
               (when params
                 (parse-integer (car params))))
              (:auth
               (loop for param in params collect (intern (string-upcase param) :keyword)))
              (otherwise
               params))))))


(defun ehlo ()
  (multiple-value-bind (ehlo-greeting ehlo-lines)
      (send-command 250 "EHLO ~A" (local-name (settings)))
    (setf (slot-value *session* 'hello-greeting) ehlo-greeting)
    (setf (slot-value *session* 'extensions) (mapcar #'parse-ehlo-line ehlo-lines))))


(defun helo ()
  (setf (slot-value *session* 'hello-greeting)
        (send-command 250 "HELO ~A" (local-name (settings)))))


(defun ehlo-or-helo ()
  (handler-bind ((permanent-error
                   (lambda (e)
                     (when (= 50 (floor (reply-code e) 10))
                       (return-from ehlo-or-helo
                         (helo))))))
    (ehlo)))


(defun starttls ()
  (when (and (starttlsp (settings))
             (not (secure-connection-p))
             (extensionp :starttls))
    (send-command 220 "STARTTLS")
    (make-connection-secure)
    (ehlo-or-helo)))


(defun mail-from (address &key size)
  (send-command 250 "MAIL FROM:<~A>~@[ SIZE=~D~]"
                address
                (when (extensionp :size) size)))


(defun rcpt-to (address)
  (send-command 250 "RCPT TO:<~A>" address))


(defun string-to-utf8-base64 (string)
  (base64:usb8-array-to-base64-string
   (flex:string-to-octets string :external-format :utf-8)))


(defvar *supported-auth-mechanisms* nil)
(defgeneric make-credentials-for (mechanism &key &allow-other-keys))
(defgeneric auth-for (mechanism credentials-fn))


(defun register-auth-mechanism (name &key (quality 1))
  (setf *supported-auth-mechanisms*
        (sort (remove-duplicates (cons (cons name quality)
                                       *supported-auth-mechanisms*)
                                 :key #'car
                                 :from-end t)
              #'>
              :key #'cdr)))


(defun make-credentials (&rest args &key username password)
  (declare (ignore username password))
  (loop for (mechanism . quality) in *supported-auth-mechanisms*
        for handler = (apply #'make-credentials-for mechanism args)
        when handler
          collect (cons mechanism handler)))


(defun authenticate ()
  (when (credentials (settings))
    (let* ((supported-mechanisms (mapcar #'car *supported-auth-mechanisms*))
           (credential-mechanisms (mapcar #'car (credentials (settings))))
           (usable-mechanisms (intersection supported-mechanisms
                                            (intersection credential-mechanisms
                                                          (extensionp :auth)))))
      (unless usable-mechanisms
        (error "Unable to authentciate, no common supported mechanism.~%Server supports: ~S~%Library supports: ~S~%Credentials support: ~S"
               (extensionp :auth)
               *supported-auth-mechanisms*
               credential-mechanisms))
      (loop for (mechanism . credentials-fn) in (credentials (settings))
            when (member mechanism usable-mechanisms)
              do (return (auth-for mechanism credentials-fn))))))


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
  (trace-log :i (format nil "Data sent: ~A lines, ~A bytes"
                        (data-lines-count *session*)
                        (data-bytes-count *session*)))
  (send-command 250 "."))


(defun data (data)
  "Send the mail message to the server. Returns server response message. This will often include
a local id of assinged to message.

The data object can be:
A string - Each line is separated by #\Linefeed or #\Return, #\Linefeed.
An octet vector - Lines should be separated by CRLF.
A list of strings - Each string is one line of the message.

Dots are automatically escaped by the library.
Lines should not be longer than 998 characters."
  (data-start)
  (etypecase data
    (list
     (dolist (line data)
       (data-line line)))
    ((vector (unsigned-byte 8))
     (data-bytes data))
    (string
     (with-input-from-string (in data)
       (loop for line = (read-line in nil)
             while line
             do (data (string-trim '(#\Return) line))))))
  (data-end))
