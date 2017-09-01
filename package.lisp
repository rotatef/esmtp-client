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

(defpackage #:cl-esmtp-client
  (:nicknames #:esmtp)
  (:use :common-lisp)
  (:export
   #:auth-for
   #:auth-plain-message
   #:data
   #:data-end
   #:data-bytes
   #:data-line
   #:data-start
   #:extensionp
   #:extensions
   #:hello-greeting
   #:greeting
   #:mail-from
   #:make-credentials
   #:make-credentials-for
   #:max-size
   #:noop
   #:permanent-error
   #:protocol-error
   #:register-auth-mechanism
   #:rcpt-to
   #:rset
   #:send-command
   #:string-to-utf8-base64
   #:transient-error
   #:with-session
   ))
