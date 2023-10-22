;;;; Making lowercase packages full of nicknames
;;;

(org.tfeb.tools.require-module:needs
 (:org.tfeb.toys.symbol-nicknames :compile t))

(defpackage :org.tfeb.toys.symbol-nicknames/lowercase-nickname-package
  (:use :cl
   :org.tfeb.toys.symbol-nicknames)
  (:export #:make-lowercase-nickname-package))

(in-package :org.tfeb.toys.symbol-nicknames/lowercase-nickname-package)

(defun make-package-from-package (name package maker
                                       &key (nicknames '()) (use '()))
  (let ((the-package (find-package package))
        (p (make-package name :nicknames nicknames :use use)))
    (do-external-symbols (s the-package p)
      (funcall maker p s the-package))))

(defun make-lowercase-nickname-package (name package &key (nicknames '()) (use '()))
  (let ((*use-symbol-nicknames* nil))
    (make-package-from-package name package
                               (lambda (p s package)
                                 (declare (ignore package))
                                 (let ((n (string-downcase (symbol-name s))))
                                   (setf (nickname-symbol n p) s)
                                   (export (find-symbol n p) p)))
                               :nicknames nicknames
                               :use use)))
