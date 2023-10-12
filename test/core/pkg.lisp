;;;; Packages for symbol nicknames core tests
;;;

(in-package :cl-user)

(defpackage :org.tfeb.toys.symbol-nicknames/test/core
  (:use
   :cl
   :org.shirakumo.parachute
   :org.tfeb.toys.symbol-nicknames)
  (:import-from :org.tfeb.toys.symbol-nicknames
   #:*nickname-symbol*)
  (:export
   #:run-tests
   #:eql-thunk
   #:truthy
   #:with-clean-packages
   #:with-no-symbol-nicknames
   #:restarting))

(defpackage :org.tfeb.toys.symbol-nicknames/test/core/whitebox
  (:use
   :cl
   :org.shirakumo.parachute
   :org.tfeb.toys.symbol-nicknames/test/core
   :org.tfeb.toys.symbol-nicknames)
  ;; Things we want to expose
  (:import-from :org.tfeb.toys.symbol-nicknames
   #:*nickname-symbol*
   #:compute-nickname-symbol
   #:simple-package-error
   #:nicknamed-by))

(defpackage :org.tfeb.toys.symbol-nicknames/test/core/blackbox
  (:use
   :cl
   :org.shirakumo.parachute
   :org.tfeb.toys.symbol-nicknames/test/core
   :org.tfeb.toys.symbol-nicknames))
