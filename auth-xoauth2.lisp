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


(defmethod make-credentials-for ((m (eql :xoauth2)) &key))


(defmethod auth-for ((m (eql :xoauth2)) credentials-fn)
  (assert-secure-connection)
  (send-command 235 "AUTH XOAUTH2 "
                (lambda (stream)
                  (funcall credentials-fn stream :xoauth2))))


(register-auth-mechanism :xoauth2)
