;;;; ASDF sysdcl for symbol-nicknames
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.toys.symbol-nicknames/core"
  ;; this can be loaded in any implementation and will not infect it
  :description "Symbol nicknames core"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/symbol-nicknames"
  :in-order-to ((test-op (test-op "org.tfeb.toys.symbol-nicknames/test/core")))
  :components
  ((:file "core")))

(defsystem "org.tfeb.toys.symbol-nicknames/test/core"
  :description "Tests for symbol nicknames core"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/symbol-nicknames"
  :depends-on ("org.tfeb.toys.symbol-nicknames/core"
               "parachute")
  :pathname "test/core"
  :perform (test-op
            (op system)
            (declare (ignore op system))
            (symbol-call :org.tfeb.toys.symbol-nicknames/test/core
                         :run-tests
                         :package :org.tfeb.toys.symbol-nicknames/test/core))
  :serial t
  :components
  ((:file "pkg")
   (:file "preamble")
   (:file "whitebox")
   (:file "blackbox")))

(defsystem "org.tfeb.toys.symbol-nicknames"
  ;; This will infect implementations it knows how to infect
  :description "Symbol nicknames"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/symbol-nicknames"
  :depends-on ("org.tfeb.toys.symbol-nicknames/core")
  :in-order-to ((test-op (test-op "org.tfeb.toys.symbol-nicknames/test")))
  :components
   ((:file "infect-lispworks"
     :if-feature :lispworks)
    (:file "infect-sbcl"
     :if-feature :sbcl)
    (:file "infect-cmucl"
     :if-feature :cmucl)
    (:file "infect-nobody"
     :if-feature (:not (:or :sbcl :lispworks)))))

(defsystem "org.tfeb.toys.symbol-nicknames/test"
  :description "Tests for symbol nicknames"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/symbol-nicknames"
  :depends-on ("org.tfeb.toys.symbol-nicknames/test/core")
  :pathname "test/"
  :perform (test-op
            (op system)
            (declare (ignore op system))
            (symbol-call :org.tfeb.toys.symbol-nicknames/test/core
                         :run-tests
                         :packages '(:org.tfeb.toys.symbol-nicknames/test/core
                                     :org.tfeb.toys.symbol-nicknames/test)))
  :serial t
  :components
  ((:file "pkg")
   (:file "test")))
