;;;; Black box tests for symbol nicknames core
;;;

(in-package :org.tfeb.toys.symbol-nicknames/test/core/blackbox)

(defpackage :org.tfeb.toys.symbol-nicknames/test/core/blackbox/p
  (:use))

(defvar *test-package* (find-package :org.tfeb.toys.symbol-nicknames/test/core/blackbox/p))

(define-test "org.tfeb.toys.symbol-nicknames/test/core/blackbox"
  :parent (:org.tfeb.toys.symbol-nicknames/test/core
           "org.tfeb.toys.symbol-nicknames/test/core"))

(define-test ("org.tfeb.toys.symbol-nicknames/test/core/blackbox"
              "nickname-symbol")
  ;; Try to make sure nickname-symbol does what it says it should
  (with-no-symbol-nicknames
    ;; This should create a symbol in *package*
    (setf (nickname-symbol "FOO") 'fish)
    (map-symbol-nicknames (lambda (n s)
                            (is eql 'foo n)
                            (is eql 'fish s)
                            (delete-symbol-nickname n)))
    ;; This should create an uninterned symbol
    (setf (nickname-symbol "FOO" nil) 'fish)
    (map-symbol-nicknames (lambda (n s)
                            (is equal "FOO" (symbol-name n))
                            (is eql nil (symbol-package n))
                            (false (eq n 'foo))
                            (is eql 'fish s)
                            (delete-symbol-nickname n))))
  (with-no-symbol-nicknames
    (let ((p *test-package*))
      (with-clean-packages (p)
        ;; This should signal an error which we continue from, and
        ;; unintern the symbol from p
        (intern "FOO" p)
        (let ((n (find-symbol "FOO" p)))
          (restarting (:restart-name 'continue)
            (setf (nickname-symbol n nil) 'fish))
          (false (find-symbol "FOO" p)))
        (map-symbol-nicknames (lambda (n s)
                                (is equal "FOO" (symbol-name n))
                                (is eql nil (symbol-package n))
                                (is eql 'fish s)
                                (delete-symbol-nickname n))))
      (with-clean-packages (p)
        (let ((n (make-symbol "FOO")))
          (finish (nickname-symbol n))
          (finish (nickname-symbol n nil))
          ;; This should intern n in p
          (restarting (:restart-name 'continue)
            (setf (nickname-symbol n p) 'fish))
          (is eql p (symbol-package n))
          (delete-symbol-nickname n))))))

(define-test ("org.tfeb.toys.symbol-nicknames/test/core/blackbox"
              "nickname-symbol-fancy-restart")
  ;; Check that the optional argument to the continue restart works
  (with-no-symbol-nicknames
    (let ((p *test-package*)
          (n 'foo))
      (with-clean-packages (p)
        (fail (setf (nickname-symbol n p) 'fish))
        (finish
         (handler-bind ((error
                         (lambda (e)
                           (import n p)
                           (invoke-restart (find-restart 'continue e) t))))
           (setf (nickname-symbol n p) 'fish)))
        (is eql n (find-symbol (symbol-name n) p))))))

(define-test ("org.tfeb.toys.symbol-nicknames/test/core/blackbox"
              "unintern")
  ;; Check uninterning of nicknames works properly
  (with-no-symbol-nicknames
    (let ((p *test-package*))
      (with-clean-packages (p)
        (is-values (find-symbol "FOO" p)
          (eql nil)
          (eql nil))
        (setf (nickname-symbol "FOO" p) 'foo)
        (is-values (nickname-symbol "FOO" p)
          (eql 'foo)
          (eql t))
        (is eql ':internal (nth-value 1 (find-symbol "FOO" p)))
        (delete-symbol-nickname "FOO" p)
        (is-values (nickname-symbol "FOO" p)
          (eql nil)
          (eql nil))
        (is-values (find-symbol "FOO" p)
          (eql nil)
          (eql nil)))
      ;; Now intern a symbol and check it is not uninterned when the
      ;; nickname is removed
      (let ((n (intern "FOO" p)))
        (is-values (find-symbol "FOO" p)
          (eql n)
          (eql ':internal))
        (setf (nickname-symbol "FOO" p) 'foo)
        (is-values (nickname-symbol "FOO" p)
          (eql 'foo)
          (eql t))
        (delete-symbol-nickname "FOO" p)
        (is-values (nickname-symbol "FOO" p)
          (eql nil)
          (eql nil))
        (is-values (find-symbol "FOO" p)
          (eql n)
          (eql ':internal))))))

(define-test ("org.tfeb.toys.symbol-nicknames/test/core/blackbox"
              "simple")
  ;; There must be more things to test
  (with-no-symbol-nicknames
   (is-values (repair-symbol-nicknames)
     (eql 0)
     (eql 0))
   (is eql 'bar (setf (nickname-symbol 'foo) 'bar))
   (is-values (nickname-symbol 'foo)
     (eql 'bar)
     (eql t))
   (let ((ns '()))
     (map-symbol-nicknames (lambda (n s)
                             (push (cons n s) ns)))
     (is equal '((foo . bar)) ns))
   (delete-symbol-nickname 'foo)
   (let ((ns '()))
     (map-symbol-nicknames (lambda (n s)
                             (push (cons n s) ns)))
     (is equal '() ns))))
