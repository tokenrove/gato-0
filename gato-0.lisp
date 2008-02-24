;;; trivial cat implementation
(in-package :gato)

(defclass environment () ((bindings :accessor bindings-of) (parent :accessor parent-of)))

(defun make-environment (&key parent)
  (aprog1 (make-instance 'environment)
    (setf (bindings-of it) (make-hash-table)
          (parent-of it) parent)))

(defmethod binding-of ((env environment) symbol)
  (aif (gethash symbol (bindings-of env)) it (binding-of (parent-of env) symbol)))
(defmethod binding-of (env symbol) nil)

;;; Environments in gato are immutable.  But we can cheat a little.
(defmethod bind ((env environment) symbol value)
  (aprog1 (if (binding-of env symbol) (make-environment :parent env) env)
    (setf (gethash symbol (bindings-of it)) value)))

(defclass block ()
  ((env :accessor environment-of)
   (type :accessor type-of)
   (code :accessor code-of)
   (source :accessor source-of)))

(defclass primitive (block) ())

(defun make-block ()
  (aprog1 (make-instance 'block)
    (setf (code-of it) ())))

(defun compile-cat-file (file)
  (with-open-file (stream file)
    (emit-code (read-block stream nil))))

(defmethod emit-code ((block block)))
(defmethod emit-code ((block primitive))
  (code-of block))

(defun metacat (block) block)

(defun typecheck (block) block)

(defun read-block (stream char)
  (declare (ignore char))
  (aprog1 (make-block)
    (setf (code-of it) (loop for x = (read stream nil) until (case x ((] } nil) t)) collect x))
    (typecheck it)))


(defparameter *cat-readtable* (make-cat-readtable))
(defun make-cat-readtable ()
  (aprog1 (copy-readtable)
    (setf (readtable-case it) :preserve)
    (set-macro-character #\[ #'read-block nil it)
    (set-macro-character #\{ #'read-block nil it)
    (set-macro-character #\} (lambda (s c) (declare (ignore s c))) nil it) ; to terminate blocks properly
    (set-macro-character #\] (lambda (s c) (declare (ignore s c))) nil it)
    (set-macro-character #\# nil nil it) ; disable reader dispatch
    (set-macro-character #\` nil nil it))) ; disable backquoting


#|
(defparameter *stack* ())
(defun push (x) (cl:push x *stack*))
(defun pop () (cl:pop *stack*))
(defun peek () (car *stack*))
(defun (setf peek) (v) (setf (car *stack*) v))

;; types
(define-cat-types int bool list var)

;; primitives
(define-cat-primitives ()
  (compose (('A -> 'B) ('B -> 'C) -> ('A -> 'C)))
  (cons (list 'a -> list))
  (uncons (list -> list var))
  (dec (int -> int)
       (decf (peek)))
  (dip ('A 'b ('A -> 'C) -> 'C 'b))
  (dup ('a -> 'a 'a))
  (eq ('a 'a -> bool))
  (if ('A bool ('A -> 'B) ('A -> 'B) -> 'B))
  (inc (int -> int))
  (pop ('a -> ) (pop))
  (qv ('a -> ( -> 'a)))
  (@ (( -> 'A) -> list))
 |#