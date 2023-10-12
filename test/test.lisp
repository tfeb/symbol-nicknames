;;;; Symbol nicknames tests
;;;

(in-package :org.tfeb.toys.symbol-nicknames/test)

(define-test "org.tfeb.toys.symbol-nicknames/test")

(define-test ("org.tfeb.toys.symbol-nicknames/test"
              "infected")
  (true
   (member (string :org.tfeb.toys.symbol-nicknames) *modules*
           :test #'string=)
   "symbol nicknames has infected your Lisp"))

(define-test ("org.tfeb.toys.symbol-nicknames/test"
              "simple")
  :depends-on ("infected")
  (with-no-symbol-nicknames
    (setf (nickname-symbol 'foo) 'bar)
    (is-values (nickname-symbol 'foo)
      (eql 'bar)
      (truthy t))
    (fail (setf (nickname-symbol 'bar) 'foo))
    (is-values (find-symbol "FOO")
      (eql 'foo)
      (eql ':internal))
    (let ((*use-symbol-nicknames* t))
      (is-values (find-symbol "FOO")
        (eql 'bar)
        (eql ':nickname)))
    (delete-symbol-nickname 'foo)
    (is-values (find-symbol "FOO")
      (eql 'foo)
      (eql ':internal))))
