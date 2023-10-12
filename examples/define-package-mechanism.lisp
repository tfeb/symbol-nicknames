;;;; Using the extensible DEFINE-PACKAGE to allow the specification of symbol nicknames
;;;

(org.tfeb.tools.require-module:needs
 ((:org.tfeb.toys.symbol-nicknames
   :org.tfeb.conduit-packages/define-package
      :org.tfeb.hax.spam
      :org.tfeb.hax.collecting)
  :compile t))

(defpackage :org.tfeb.toys.symbol-nicknames/define-package
  (:use :cl
   :org.tfeb.toys.symbol-nicknames
   :org.tfeb.conduit-packages/define-package
   :org.tfeb.hax.spam
   :org.tfeb.hax.collecting))

(in-package :org.tfeb.toys.symbol-nicknames/define-package)

(defmethod initial-define-package-state ((mechanism (eql 'symbol-nicknames))
                                         name clauses)
  (declare (ignore name clauses))
  '())

(defmethod process-define-package-clause ((mechanism (eql 'symbol-nicknames))
                                          key clause state name clauses)
  (declare (ignore name clauses))
  (case key
    ((:symbol-nicknames)
     (matching (rest clause)
       ((list-of (any))
        (values
         (append state (rest clause)) t))
       (otherwise
        (error "botched symbol-nickname clause ~A" clause))))
    (otherwise
     (values nil nil))))

(defun install-package-symbol-nicknames (package-name specs)
  (let ((*use-symbol-nicknames* nil)
        (package (find-package package-name)))
    (unless package
      (error "no package ~A" package-name))
    (dolist (spec specs)
      (destructuring-bind (source-name target-package-maybe target-name) spec
        (let ((source (intern source-name package))
              (target (if target-package-maybe
                          (let ((target-package (find-package target-package-maybe)))
                            (unless target-package
                              (error "no target package ~A" target-package-maybe))
                            (multiple-value-bind (s status) (find-symbol target-name
                                                                         target-package)
                              (unless status
                                (error "no target symbol ~A in ~A"
                                       target-name target-package))
                              s))
                        (make-symbol target-name))))
          (setf (nickname-symbol source) target))))))

(defmethod compute-define-package-forms ((mechanism (eql 'symbol-nicknames))
                                         state package-name clauses)
  (declare (ignore clauses))
  (flet ((name ()
           (is-type '(or symbol string))))
    (if (null state)
        (values '() '() '())
      (values
       '()
       '()
       `((install-package-symbol-nicknames
          ',package-name
          ',(collecting
              (dolist (spec state)
                (matching spec
                  ((list-matches (name) (is-type 'symbol))
                   (destructuring-bind (source target) spec
                     (let ((tp (symbol-package target)))
                        (collect
                        (list (string source)
                              (if tp (string (package-name tp)) nil)
                              (string (symbol-name target)))))))
                  ((list-matches (name) (list-matches (name) (name)))
                   (destructuring-bind (source (tp tn)) spec
                     (collect
                      (list (string source)
                            (if tp (string tp) nil)
                            (string tn)))))
                  (otherwise
                   (error "bad symbol nickname spec ~A" spec)))))))))))

(pushnew 'symbol-nicknames *define-package-mechanisms*)

#||
(define-package :foo
  (:use)
  (:extends :cl)
  (:export #:void #:pairp)
  (:symbol-nicknames
   (#:void nil)
   (#:pairp consp)))

(define-package :bar
  (:use :foo))
||#