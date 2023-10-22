;;;; White box tests for symbol nicknames core
;;;

(in-package :org.tfeb.toys.symbol-nicknames/test/core/whitebox)

(define-test "org.tfeb.toys.symbol-nicknames/test/core/whitebox"
  :parent (:org.tfeb.toys.symbol-nicknames/test/core
           "org.tfeb.toys.symbol-nicknames/test/core"))

(defpackage :org.tfeb.toys.symbol-nicknames/test/core/whitebox/p
  (:use))

(defvar *test-package* (find-package :org.tfeb.toys.symbol-nicknames/test/core/whitebox/p))

(define-test ("org.tfeb.toys.symbol-nicknames/test/core/whitebox"
              "compute-nickname-symbol")
  ;; A bunch of tests to check that COMPUTE-NICKNAME-SYMBOL is
  ;; actually doing what it is meant to.
  ;;
  ;; There probably should be more tests here.
  (let ((p *test-package*))
    (with-clean-packages (p)
      ;; Look for a symbol in P: it is not there
      (is-values (compute-nickname-symbol "FOO" p t nil)
        (eql nil) (eql nil) (eql nil))
      ;; Now try and look up a symbol which exists but is not present in
      ;; P.  This should raise an error.
      (fail (compute-nickname-symbol 'foo p t nil)
            'simple-package-error)
      ;; This should be OK though (no package checks)
      (is-values (compute-nickname-symbol 'foo p nil nil)
        (eql 'foo) (eql t) (eql nil))
      ;; As should this (current package)
      (is-values (compute-nickname-symbol 'foo *package* t nil)
        (eql 'foo) (eql t) (eql nil))
      ;; OK, FOO should not be in the test package
      (false (find-symbol "FOO" p))
      ;; Now check it's not in P then make it be in P and check it is
      (is-values (compute-nickname-symbol "FOO" p t nil)
        (eql nil) (eql nil) (eql nil))
      (is-values (compute-nickname-symbol "FOO" p t t)
        (eql-thunk (lambda ()
                     (find-symbol "FOO" p)))
        (eql t) (eql t))
      (true (find-symbol "FOO" p))
      ;; Now this should fail
      (fail (compute-nickname-symbol 'foo p t nil)
            'simple-package-error)
      ;; As should this
      (fail (compute-nickname-symbol 'foo p t t)
            'simple-package-error)
      ;; Now test the restart which should shadow things
      (isnt eql 'foo (find-symbol "FOO" p))
      (fail (restarting (:restart-name 'continue)
              (compute-nickname-symbol 'foo p t nil))
            'simple-package-error)
      (finish (restarting (:restart-name 'continue)
                (compute-nickname-symbol 'foo p t t)))
      (is eql 'foo (find-symbol "FOO" p))
      (true (member (find-symbol "FOO" p)
                    (package-shadowing-symbols p))))
    ;; Now a simpler intern test: this should just import a symbol
    ;; into P, there is no need to shadow this time
    (with-clean-packages (p)
      (fail (compute-nickname-symbol 'foo p t nil)
            'simple-package-error)
      (fail (compute-nickname-symbol 'foo p t t)
            'simple-package-error)
      (false (find-symbol "FOO" p))
      (fail (restarting (:restart-name 'continue)
              (compute-nickname-symbol 'foo p t nil))
            'simple-package-error)
      (finish (restarting (:restart-name 'continue)
                (compute-nickname-symbol 'foo p t t)))
      (is eql 'foo (find-symbol "FOO" p))
      (is eql '() (package-shadowing-symbols p)))))


(define-test ("org.tfeb.toys.symbol-nicknames/test/core/whitebox"
              "nicknames")
  (let ((p *test-package*))
    (with-clean-packages (p)
      (with-no-symbol-nicknames
        (false (find-symbol "FOO" p))
        (is-values (nickname-symbol "FOO" p)
          (eql nil) (eql nil))
        (false (find-symbol "FOO" p))
        (is eql 'car (setf (nickname-symbol "FOO" p) 'car))
        (is-values (nickname-symbol "FOO" p)
          (eql 'car) (eql t))
        (let ((s (find-symbol "FOO" p)))
          (true s)
          (is-values (gethash s *nickname-symbol*)
            (eql 'car) (truthy t))
          (is-values (nickname-symbol s)
            (eql 'car) (eql t))
          (delete-symbol-nickname "FOO" p)
          (is-values (nickname-symbol s)
            (eql nil) (eql nil)))))))

(define-test ("org.tfeb.toys.symbol-nicknames/test/core/whitebox"
              "repair-symbol-nicknames")
  ;; Some basic sanity
  (finish (repair-symbol-nicknames))
  (with-no-symbol-nicknames
    (is-values (repair-symbol-nicknames)
      (eql 0)
      (eql 0))
    (setf (nickname-symbol 'foo) 'bar)
    (fail (setf (nickname-symbol 'bar) 'foo))
    (fail (setf (nickname-symbol 'bar) 'bone))
    (finish (delete-symbol-nickname 'foo))
    (is-values (repair-symbol-nicknames)
      (eql 0)
      (eql 0))
    ;; Check repairs and errors
    (let ((*nickname-symbol* (make-hash-table)))
      (setf (get 'foo 'nicknamed-by) 1)
      (is-values (repair-symbol-nicknames)
        (eql 1)
        (eql 0)))
    (setf (gethash 'foo *nickname-symbol*) 'bar)
    (is-values (repair-symbol-nicknames)
      (eql 1)
      (eql 0))
    (remhash 'foo *nickname-symbol*)
    (is-values (repair-symbol-nicknames)
      (eql 1)
      (eql 0))
    ;; Check chains via the restart
    (setf (gethash 'foo *nickname-symbol*) 'bar
          (gethash 'bar *nickname-symbol*) 'bone)
    (fail (repair-symbol-nicknames))
    (is-values (restarting (:restart-name 'remove-nickname-source)
                 (repair-symbol-nicknames))
      (eql 2)
      (eql 1))
    (is-values (nickname-symbol 'bar)
      (eql 'bone)
      (eql t))
    (is-values (nickname-symbol 'foo)
      (eql nil)
      (eql nil))
    (delete-symbol-nickname 'bar)
    (setf (gethash 'foo *nickname-symbol*) 'bar
          (gethash 'bar *nickname-symbol*) 'bone)
    (fail (repair-symbol-nicknames))
    (is-values (restarting (:restart-name 'remove-nickname-target)
                 (repair-symbol-nicknames))
      (eql 2)
      (eql 1))
    (is-values (nickname-symbol 'bar)
      (eql nil)
      (eql nil))
    (is-values (nickname-symbol 'foo)
      (eql 'bar)
      (eql t))
    (delete-symbol-nickname 'foo))
    (is-values (repair-symbol-nicknames)
      (eql 0)
      (eql 0))
    ;; Check chains via arguments
    (setf (gethash 'foo *nickname-symbol*) 'bar
          (gethash 'bar *nickname-symbol*) 'bone)
    (fail (repair-symbol-nicknames))
    (is-values (repair-symbol-nicknames :remove-nickname-sources t)
      (eql 2)
      (eql 1))
    (is-values (nickname-symbol 'bar)
      (eql 'bone)
      (eql t))
    (is-values (nickname-symbol 'foo)
      (eql nil)
      (eql nil))
    (delete-symbol-nickname 'bar)
    (setf (gethash 'foo *nickname-symbol*) 'bar
          (gethash 'bar *nickname-symbol*) 'bone)
    (fail (repair-symbol-nicknames))
    (is-values (repair-symbol-nicknames :remove-nickname-targets t)
      (eql 2)
      (eql 1))
    (is-values (nickname-symbol 'bar)
      (eql nil)
      (eql nil))
    (is-values (nickname-symbol 'foo)
      (eql 'bar)
      (eql t))
    (delete-symbol-nickname 'foo)
    (is-values (repair-symbol-nicknames)
      (eql 0)
      (eql 0)))
