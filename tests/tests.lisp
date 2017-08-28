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

(in-package #:cl-esmtp-client-tests)


(defun talk-to-smtp-server (&key client mail-from rcpt-to data)
  (smtp::with-session (client)
    (smtp::mail-from mail-from)
    (smtp::rcpt-to rcpt-to)
    (when data
      (smtp::data-start)
      (smtp::data-bytes data)
      (smtp::data-end))))


(defun talk-to-gmail ()
  (talk-to-smtp-server :client (make-instance 'smtp::client
                                              :host "smtp.gmail.com"
                                              :port 465
                                              :ssl t)
                       :mail-from "thomas.bakketun@copyleft.no"
                       :rcpt-to "postmaster@gmail.com"))

(defun run ()
  (talk-to-gmail))
