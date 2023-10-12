;;;; Infect SBCL
;;;

(in-package :org.tfeb.toys.symbol-nicknames)

#-SBCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Not SBCL"))

(provide :org.tfeb.toys.symbol-nicknames)

(deftype index ()
  `(integer 0 (,array-dimension-limit)))

(defconstant %find-symbol
  (if (boundp '%find-symbol)
      (symbol-value '%find-symbol)
    (symbol-function 'sb-impl::%find-symbol)))

(sb-ext:with-unlocked-packages (#:sb-impl)
  (setf (symbol-function 'sb-impl::%find-symbol)
        (lambda (string length package)
          (declare (type simple-string string)
                   (type index length)
                   (optimize speed))
          (if *use-symbol-nicknames*
              (multiple-value-bind (s status)
                  (funcall %find-symbol string length package)
                (if status
                    (multiple-value-bind (target found)
                        (gethash s *nickname-symbol*)
                      (if found
                          (values target ':nickname)
                          (values s status)))
                    (values s status)))
              (funcall %find-symbol string length package)))))
