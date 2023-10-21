;;;; Preamble for symbol nicknames core tests
;;;

(in-package :org.tfeb.toys.symbol-nicknames/test/core)

(define-test "org.tfeb.toys.symbol-nicknames/test/core")

(defun eql-thunk (value thunk)
  (eql value (funcall thunk)))

(defun truthy (truthiness expected)
  ;; A test which checks truthiness
  (if truthiness
      expected
    (not expected)))

(defun call/clean-packages (thunk packages)
  (dolist (p packages)
    (do-all-symbols (s p)
      (unintern s p)))
    (unwind-protect
        (funcall thunk)
      (dolist (p packages)
        (do-all-symbols (s p)
          (unintern s p)))))

(defmacro with-clean-packages ((&rest packages) &body forms)
  ;; This is really a fixture I think, but this is so much easier and
  ;; clearer.
  `(call/clean-packages (lambda () ,@forms) (list ,@packages)))

(defun call/no-symbol-nicknames (thunk)
  (unwind-protect
      (let ((*nickname-symbol* (make-hash-table))
            (*use-symbol-nicknames* nil))
        ;; patch up possible wrong counts now there are no nicknames
        (repair-symbol-nicknames)
        (funcall thunk))
    ;; and now patch them back
    (repair-symbol-nicknames)))

(defmacro with-no-symbol-nicknames (&body forms)
  `(call/no-symbol-nicknames (lambda () ,@forms)))

(defmacro restarting ((&key (restart-name ''continue)
                            (condition-type 'error)
                            (arguments ()))
                      &body forms)
  ;; Simple-minded macro to invoke restarts
  `(handler-bind ((,condition-type
                   (lambda (c)
                     (let ((r (find-restart ,restart-name c)))
                       (when r
                         (invoke-restart r ,@arguments))))))
     ,@forms))

(define-test ("org.tfeb.toys.symbol-nicknames/test/core"
              "restarting")
  ;; Sanity
  (fail (restarting ()
          (error "fail")))
  (eql 1 (restarting ()
           (restart-case
               (error "fail")
             (continue ()
               1))))
  (let ((v (cons nil nil)))
    (eql v (restarting (:arguments (v))
             (restart-case
                 (error "fail")
               (continue (x)
                 x))))))

(defun run-tests (&key (package nil packagep)
                       (packages (if (not packagep)
                                     (list *package*)
                                   (list package)))
                       (report 'summary))
  ;; This is just for interactive development
  (dolist (package packages (values))
    (format t "~%* Tests for ~A~%" (typecase package
                                   (package (package-name package))
                                   (t (string package))))
    (test package :report report)))
