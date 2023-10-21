;;;; Symbol nicknames: core
;;;

(defpackage :org.tfeb.toys.symbol-nicknames
  (:use :cl)
  #+Lispworks
  (:use :lw)
  (:export
   #:nickname-symbol
   #:delete-symbol-nickname
   #:map-symbol-nicknames
   #:*use-symbol-nicknames*
   ;; fsck function and its two restart names
   #:repair-symbol-nicknames
   #:remove-nickname-source
   #:remove-nickname-target))

(in-package :org.tfeb.toys.symbol-nicknames)

(defvar *use-symbol-nicknames* nil)

(defvar *nickname-symbol*
  ;; Map nicknames to symbols they are nicknames for, should be weak
  ;; on its key if possible
  #+LispWorks
  (make-hash-table :weak-kind ':key)
  #+SBCL
  (make-hash-table :weakness ':key)
  #+CMUCL
  (make-hash-table :weak-p ':key)
  #-(or LispWorks SBCL)
  (make-hash-table))

(define-condition simple-package-error (package-error simple-error)
  ())

(defun compute-nickname-symbol (nickname package packagep modify)
  ;; Canonicalise a nickname.  If the nickname is a string this will
  ;; either find or create a suitable symbol, creating one only if
  ;; MODIFY is given.  If PACKAGE is not NIL and a symbol is being
  ;; created it will be interned in that package.  If the nickname is
  ;; a symbol and PACKAGEP is given it is returned after some sanity
  ;; checks which may generate errors, which have suitable restarts.
  ;; These checks are done only when a package is given.
  ;;
  ;; Return three values:
  ;; - the symbol (only has meaning if we succeeded)
  ;; - success?
  ;; - did we modify anything?
  (declare (type (or symbol string) nickname)
           (type (or package null) package))
  (let ((*use-symbol-nicknames* nil)
        (did-modify nil))
    (etypecase nickname
      (string
       (if package
           (multiple-value-bind (s status) (find-symbol nickname package)
             (cond
              (status
               (values s t nil))
              (modify
               (values (intern nickname package) t t))
              (t
               (values nil nil nil))))
         (if modify
             (values (make-symbol nickname) t t)
           (values nil nil nil))))
      (symbol
       (cond
        ((and packagep package)
         (multiple-value-bind (s status) (find-symbol (symbol-name nickname) package)
           (cond
            ((and status (eq s nickname))
             ;; this is fine
             )
            ((not status)
             ;; It's not in the package
             (restart-case (error 'simple-package-error
                                  :package package
                                  :format-control "Nickname ~S is not present in ~A"
                                  :format-arguments (list nickname (package-name package)))
                (continue (&optional (already-fixed nil))
                  :report (lambda (s)
                            (format s "Import ~S into ~A" nickname (package-name package)))
                  :test (lambda (c) (declare (ignore c)) modify)
                  (unless already-fixed (import nickname package))
                  (setf did-modify t))))
             (t
              (restart-case (error
                             'simple-package-error
                             :package package
                             :format-control "Nickname named ~A is present in ~A but is not ~S"
                             :format-arguments (list (symbol-name nickname)
                                                     (package-name package)
                                                     nickname))
                (continue (&optional (already-fixed nil))
                  :report (lambda (s)
                            (format s "Shadow the symbol ~S in ~A by ~S"
                                    s (package-name package) nickname))
                  :test (lambda (c) (declare (ignore c)) modify)
                  (unless already-fixed (shadowing-import (list nickname) package))
                  (setf did-modify t)))))))
        (packagep
         ;; package given as NIL
         (unless (not (symbol-package nickname))
           (restart-case (error 'simple-package-error
                                :package (symbol-package nickname)
                                :format-control "Nickname ~S has package ~A but should not"
                                :format-arguments (list nickname
                                                        (package-name (symbol-package nickname))))
             (continue (&optional (already-fixed nil))
               :report (lambda (s)
                         (format s "Unintern ~S from ~A" nickname (package-name
                                                                   (symbol-package nickname))))
               :test (lambda (c) (declare (ignore c)) modify)
               (unless already-fixed (unintern nickname (symbol-package nickname)))
               (setf did-modify t))))))
       (values nickname t did-modify)))))

(defun nickname-symbol (nickname &optional (package/name (if (symbolp nickname)
                                                             (symbol-package nickname)
                                                           *package*)
                                                         packagep))
  ;; Find the symbol for NICKNAME and return it and T, or NIL and NIL
  (declare (type (or symbol string) nickname)
           (type (or package string null) package/name))
  (let ((package (and package/name (find-package package/name))))
    (when (and package/name (not package))
      (error "no package ~A" package/name))
    (multiple-value-bind (nickname-symbol found modified)
          (compute-nickname-symbol nickname package packagep nil)
      (when modified
        (error "unexpectedly modified"))
      (if found
          (gethash nickname-symbol *nickname-symbol*)
        (values nil nil)))))

(defun (setf nickname-symbol) (target nickname &optional
                                      (package/name (if (symbolp nickname)
                                                        (symbol-package nickname)
                                                      *package*)
                                                    packagep))
  (declare (type (or symbol string) nickname)
           (type (or package string null) package/name)
           (type symbol target))
  (let ((package (and package/name (find-package package/name))))
    (when (and package/name (not package))
      (error "no package ~A" package/name))
    (when (nth-value 1 (gethash target *nickname-symbol*))
      (error "~S is a nickname" target))
    (multiple-value-bind (nickname-symbol found modified)
        (compute-nickname-symbol nickname package packagep t)
      (declare (ignore modified))
      (unless found
        (error "couldn't get a symbol for ~S" nickname))
      (when (eq nickname-symbol target)
        (error "~S can't be a nickname for itself" nickname-symbol))
      (when (> (get nickname-symbol 'nicknamed-by 0) 0)
        (error "~S is the target of a nickname" nickname-symbol))
      (multiple-value-bind (previous-target previous-nickname-p)
          (gethash nickname-symbol *nickname-symbol*)
        (when previous-nickname-p
          (decf (get previous-target 'nicknamed-by))))
      (setf (gethash nickname-symbol *nickname-symbol*) target)
      (incf (get target 'nicknamed-by 0))
      target)))

(defun delete-symbol-nickname (nickname &optional (package/name (if (symbolp nickname)
                                                                    (symbol-package nickname)
                                                                  *package*)
                                                                packagep))
  (declare (type (or symbol string) nickname)
           (type (or package string null) package/name))
  (let ((package (and package/name (find-package package/name))))
    (when (and package/name (not package))
      (error "no package ~A" package/name))
    (multiple-value-bind (nickname-symbol found modified)
        (compute-nickname-symbol nickname package packagep nil)
      (when modified
        (error "unexpectedly modified"))
      (when found
        (multiple-value-bind (target nicknamep) (gethash nickname-symbol *nickname-symbol*)
          (when nicknamep
            (remhash nickname-symbol *nickname-symbol*)
            (let ((c (decf (get target 'nicknamed-by))))
              (cond
               ((zerop c)
                (remprop target 'nicknamed-by))
               ((< c 0)
                (error "negative creep")))))))))
  nil)

(defun map-symbol-nicknames (f &optional (package/name nil packagep))
  (declare (type (or function symbol) f)
           (type (or package string null) package/name))
  (let ((package (and package/name (find-package package/name))))
    (when (and package/name (not package))
      (error "no package ~A" package/name))
    ;; Doing it like this allows us to add or remove nicknames as we
    ;; like, not just the one we're looking at.
    (let ((ns '()))
      (maphash (lambda (nickname symbol)
                 (when (or (not packagep)
                           (eq (symbol-package nickname) package))
                   (push (cons nickname symbol) ns)))
               *nickname-symbol*)
      (dolist (e ns nil)
        (funcall f (symbol-name (car e)) (symbol-package (car e)) ((cdr e))))))

(defun repair-symbol-nicknames (&optional (report nil reportp))
  ;; Repair symbol nicknames: deal with chains by asking the user, and
  ;; offering suitable restarts.  Repair counts without user
  ;; intervention. Return the number of repairs made, and the number of
  ;; nasties found.
  (flet ((report (control &rest args)
           (when reportp
             (format report "~&~?~%" control args)
             (finish-output report))))
    (let ((repairs 0)
          (nasties 0)
          (cleanup nil)
          (*use-symbol-nicknames* nil))
      ;; Build a list of target counts, finding and nuking any chains.
      ;; This is careful to remove the entries on a restart outside
      ;; MAPHASH which is always safe (though it's probably fine
      ;; anyway).
      (tagbody
       restart
       (when cleanup (funcall cleanup))
       (let ((target-counts (make-hash-table)))
         (maphash
          (lambda (nickname target)
            (when (nth-value 1 (nickname-symbol target))
              (restart-case
                  (error "~S is a nickname for ~S, which is a nickname itself for ~S"
                         nickname target (gethash target *nickname-symbol*))
                (remove-nickname-source ()
                  :report (lambda (s)
                            (format s "remove nickname symbol ~S and restart" nickname))
                  (setf cleanup (lambda ()
                                  (report "removing nickname ~S which points to a nickname"
                                          nickname)
                                  (remhash nickname *nickname-symbol*)))
                  (incf repairs)
                  (incf nasties)
                  (go restart))
                (remove-nickname-target ()
                  :report (lambda (s)
                            (format s "remove target symbol ~S and restart" target))
                  (setf cleanup (lambda ()
                                  (report "removing nickname ~S which is the target of a nickname"
                                          target)
                                  (remhash target *nickname-symbol*)))
                  (incf repairs)
                  (incf nasties)
                  (go restart))))
            (incf (gethash target target-counts 0)))
          *nickname-symbol*)
         ;; Now fix target counts.
         (do-all-symbols (s)
           (let ((tc (gethash s target-counts)))
             (if tc
                 (cond
                  ((not (get s 'nicknamed-by))
                   (report "target ~S missing count ~D" s tc)
                   (setf (get s 'nicknamed-by) tc)
                   (incf repairs))
                  ((/= tc (get s 'nicknamed-by))
                   (report "target ~S has count ~D, should be ~D"
                           s (get s 'nicknamed-by) tc)
                   (setf (get s 'nicknamed-by) tc)
                   (incf repairs)))
               (let ((sc (get s 'nicknamed-by)))
                 (when sc
                   (report "~S has count ~D, should not have a count"
                           s sc)
                   (remprop s 'nicknamed-by)
                   (incf repairs))))))))
      (values repairs nasties))))
