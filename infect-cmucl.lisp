;;;; Infect CMUCL
;;;

(in-package :org.tfeb.toys.symbol-nicknames)

#-CMUCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Not CMUCL"))

(provide :org.tfeb.toys.symbol-nicknames)

(deftype index ()
  `(integer 0 (,array-dimension-limit)))

(defconstant find-symbol*
  (if (boundp 'find-symbol*)
      (symbol-value 'find-symbol*)
    (symbol-function 'lisp::find-symbol*)))

(ext:without-package-locks
  (setf (symbol-function 'lisp::find-symbol*)
        (lambda (string length package)
          (declare (type simple-string string)
                   (type index length)
                   (optimize speed))
          (if *use-symbol-nicknames*
              (multiple-value-bind (s status)
                  (funcall find-symbol* string length package)
                (if status
                    (multiple-value-bind (target found)
                        (gethash s *nickname-symbol*)
                      (if found
                          (values target ':nickname)
                          (values s status)))
                    (values s status)))
              (funcall find-symbol* string length package)))))
