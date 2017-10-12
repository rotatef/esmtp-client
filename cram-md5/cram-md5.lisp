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


(in-package #:esmtp-client-cram-md5)


(defun auth-cram-md5-message (challenge username password)
  (let* ((key (flex:string-to-octets password :external-format :utf-8))
         (hmac (ironclad:make-hmac key :md5))
         (digest (progn (ironclad:update-hmac hmac challenge)
                        (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))
         (response (format nil "~A ~A" username digest)))
    (esmtp:string-to-utf8-base64 response)))


(defmethod esmtp:make-credentials-for ((m (eql :cram-md5)) &key username password)
  (lambda (challenge)
    (auth-cram-md5-message challenge username password)))


(defmethod esmtp:auth-for ((m (eql :cram-md5)) credentials-fn)
  (let* ((challenge-base64 (esmtp:send-command 334 "AUTH CRAM-MD5"))
         (challenge (base64:base64-string-to-usb8-array challenge-base64)))
    (esmtp:send-command 235 "~A" (funcall credentials-fn challenge))))


(esmtp:register-auth-mechanism :cram-md5 :quality 1.5)
