;;;; Package for symbol nicknames tests
;;;

(in-package :cl-user)

(defpackage :org.tfeb.toys.symbol-nicknames/test
  (:use
   :cl
   :org.shirakumo.parachute
   :org.tfeb.toys.symbol-nicknames
   :org.tfeb.toys.symbol-nicknames/test/core)
  (:export
   :run-tests))
