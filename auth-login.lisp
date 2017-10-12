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


(defmethod make-credentials-for ((m (eql :login)) &key username password)
  (lambda (stream phase)
    (ecase phase
      (:username (princ (string-to-utf8-base64 username) stream))
      (:password (princ (string-to-utf8-base64 password) stream)))))


(defmethod auth-for ((m (eql :login)) credentials-fn)
  (assert-secure-connection)
  (send-command 334 "AUTH LOGIN")
  (send-command 334 "" (lambda (stream)
                         (funcall credentials-fn stream :username)))
  (send-command 235 "" (lambda (stream)
                         (funcall credentials-fn stream :password))))


(register-auth-mechanism :login :quality 0.5)
