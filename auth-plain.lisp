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


(defun auth-plain-message (username password)
  (string-to-utf8-base64 (format nil "~A~C~A~C~A"
                                 username
                                 #\null
                                 username
                                 #\null
                                 password)))


(defmethod make-credentials-for ((m (eql :plain)) &key username password)
  (lambda (stream)
    (princ (auth-plain-message username password) stream)))


(defmethod auth-for ((m (eql :plain)) credentials-fn)
  (assert-secure-connection)
  (send-command 235 "AUTH PLAIN " credentials-fn))


(register-auth-mechanism :plain :quality 1)
