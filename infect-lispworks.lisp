;;;; Infect LispWorks
;;;

(in-package :org.tfeb.toys.symbol-nicknames)

#-LispWorks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Not LispWorks"))

(provide :org.tfeb.toys.symbol-nicknames)

(deftype index ()
  `(integer 0 (,array-dimension-limit)))

(defadvice (system::find-symbol* symbol-nicknames :around) (name/tail length package)
  (declare (type string name/tail)
           (type index length)
           (type package package)
           (optimize speed))
  (if *use-symbol-nicknames*
      (multiple-value-bind (s status) (call-next-advice name/tail length package)
        (if status
            (multiple-value-bind (target found) (gethash s *nickname-symbol*)
              (if found
                  (values target ':nickname)
                (values s status)))
          (values s status)))
    (call-next-advice name/tail length package)))
