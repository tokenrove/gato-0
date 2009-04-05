;;; trivial cat implementation
(in-package :gato)

(defclass environment () ((bindings :accessor bindings-of) (parent :accessor parent-of)))

(defun make-environment (&key parent)
  (aprog1 (make-instance 'environment)
    (setf (bindings-of it) (make-hash-table)
          (parent-of it) parent)))

(defgeneric binding-of (env sym))
(defmethod binding-of ((env environment) symbol)
  (aif (gethash symbol (bindings-of env)) it (binding-of (parent-of env) symbol)))
(defmethod binding-of (env symbol) nil)

;;; Environments in gato are immutable.  But we can cheat a little.
(defmethod bind ((env environment) symbol value)
  (aprog1 (if (binding-of env symbol) (make-environment :parent env) env)
    (setf (gethash symbol (bindings-of it)) value)))

(defclass block ()
  ((env :accessor environment-of)
   (type :accessor type-of :initarg :type)
   (code :accessor code-of :initarg :code)
   (source :accessor source-of)))

(defun make-block (code) (make-instance 'block :code code))

(defclass primitive (block)
  ((name :initarg :name)))

(defun make-primitive (name type body)
  (make-instance 'primitive :name name :type type :code (compile nil body)))

(defclass composed-block (block) ())

(defun make-composed-block (b a)
  (make-instance 'composed-block :code (list a b)))

(defun repl ()
  (let ((*readtable* *cat-readtable*))
    (loop (print (eval (read) *environment*)))))

(defun eval-cat-file (file)
  (with-open-file (stream file)
    (let ((*readtable* *cat-readtable*))
      (apply (read-block stream nil)))))

(defun compile-cat-file (file)
  (with-open-file (stream file)
    (let ((*readtable* *cat-readtable*))
      (emit-code (read-block stream nil)))))

(defmethod emit-code ((block block)) (code-of block))
(defmethod emit-code ((block primitive)) (code-of block))

(defun metacat (block) block)

(defun typecheck (block) block)

(defun read-block (stream char)
  (declare (ignore char))
  (aprog1 (make-block (parse-cat-code stream))
    (typecheck it)))

(defvar *environment* (make-environment))

(defun verify-type (block type) (declare (ignore block type)))

(defun read-define (stream)
  (let ((identifier (read stream)))
    (bind *environment* identifier
          (aprog1 (read stream)
            (when (eql it '|:|)
              (let ((type (read stream)))
                (setf it (read stream))
                (verify-type it type)))))))

(defun parse-cat-code (stream)
  (loop for x = (read stream nil)
     until (case x ((] } nil) t))
     if (eql x '|define|) do (read-define stream)
     else collect x))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-cat-readtable ()
    (aprog1 (copy-readtable)
      (setf (readtable-case it) :preserve)
      (set-macro-character #\[ #'read-block nil it)
      (set-macro-character #\{ #'read-block nil it)
      (set-macro-character #\} (lambda (s c) (declare (ignore s c))) nil it) ; to terminate blocks properly
      (set-macro-character #\] (lambda (s c) (declare (ignore s c))) nil it)
      (set-macro-character #\: nil nil it)
      (set-macro-character #\# nil nil it) ; disable reader dispatch
      (set-macro-character #\` nil nil it)))) ; disable backquoting
(defparameter *cat-readtable* (make-cat-readtable))

;;;; EVALUATION

(defmethod apply ((block block))
  (mapc (rcurry #'eval *environment*) (code-of block)))

(defmethod apply ((block composed-block))
  (mapc #'apply (code-of block)))

(defmethod apply ((primitive primitive))
  (funcall (code-of primitive)))

(defun eval (something environment)
  (typecase something
    (symbol (apply (binding-of environment something)))
    (function (funcall something))
    (t (push something))))

;;;; STACK

(defparameter *stack* ())
(defun push (x) (cl:push x *stack*))
(defun pop () (cl:pop *stack*))
(defmacro peek () `(car *stack*))
(defmacro npeek (n) `(elt *stack* ,n))
;;(defun (setf peek) (v) (setf (car *stack*) v))

(defmacro define-cat-types (&rest foo) (declare (ignore foo)))
;; types
(define-cat-types int bool list var)

(defmacro define-cat-primitives ((env) &body rest)
  `(progn
     ,@(loop for (id type . body) in rest
          collecting `(setf ,env
                            (bind ,env (intern (string-downcase (symbol-name ',id)))
                                  (make-primitive ',id ',type (lambda () ,@body)))))))
;; primitives
(define-cat-primitives (*environment*)
  (compose (('A -> 'B) ('B -> 'C) -> ('A -> 'C))
           (push (make-composed-block (pop) (pop))))
  (cons (list 'a -> list) (let ((a (pop)) (l (pop))) (push (cons a l))))
  (uncons (list -> list var) (destructuring-bind (head . tail) (pop) (push head) (push tail)))
  (dec (int -> int) (decf (peek)))
  (dip ('A 'b ('A -> 'C) -> 'C 'b)
       (let ((f (pop))
             (b (pop)))
         (apply f)
         (push b)))
  (dup ('a -> 'a 'a) (push (peek)))
  (eq ('a 'a -> bool) (push (eql (pop) (pop))))
  (if ('A bool ('A -> 'B) ('A -> 'B) -> 'B)
      (let ((false (pop))
            (true (pop))
            (p (pop)))
        (if p (apply true) (apply false))))
  (inc (int -> int) (incf (peek)))
  (pop ('a -> ) (pop))
  (quote ('a -> ( -> 'a)) (push (make-block (list (pop)))))
  (list (( -> 'A) -> list) (push (list (pop)))))


;; non-level-0 ones
(define-cat-primitives (*environment*)
  (not (bool -> bool) (setf (peek) (not (peek))))
  (while ('A (->) (-> bool) -> 'A)
    (loop with ? = (pop) and f = (pop)
       for x = (apply ?) while (pop) do (apply f)))
  (+ (int int -> int) (push (+ (pop) (pop))))
  (- (int int -> int) (let ((b (pop)) (a (pop))) (push (- a b))))
  (apply ('A ('A -> 'B) -> 'B) (apply (pop)))
  (swap ('a 'b -> 'b 'a) (rotatef (peek) (npeek 1)))
  (<= (int int -> bool) (let ((b (pop)) (a (pop))) (push (<= a b))))
  (write ('a ->) (print (pop)))
  (|#type| ('a ->) (print (type-of (pop)))))

(compile-cat-file "prelude.cat")
