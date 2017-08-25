;;;;  cl-smtp-submitter
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

(defsystem #:cl-smtp-submitter
  :name "cl-smtp-submitter"
  :licence "GNU Lesser General Public Licence 3.0"
  :author "Thomas Bakketun <thomas.bakketun@copyleft.no>"
  :description "A library for sending email via SMTP according to RFC 6409."
  :depends-on (:cl-base64
               :cl+ssl
               :usocket)
  :serial t
  :components ((:file "package")
               (:file "client")))
