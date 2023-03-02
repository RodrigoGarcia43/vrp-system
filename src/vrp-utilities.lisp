(in-package :vrp)

;;;{{{ symb and flatten

(defun mkstr (&rest args)
  "Returns a string with the concatenation of the args"
  (string-upcase
   (with-output-to-string (s)
     (dolist (a args) (princ a s)))))

(defun symb (&rest args)
  "Returns a symbol formed by the concatenation of the args."
  (values (intern (apply #'mkstr args))))

(defun flatten (x)
  "Flattens a structure."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;;;}}}

(defun make-keyword (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

;;;{{{ with-gensym
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym (symbol-name ',s))))
                   syms)
     ,@body))
;;;}}}

;;;{{{ syntatic sugar: then, else, while, for
(defmacro then (&body body)
  "This is just a wrapper for progn."
  `(progn ,@body))

(defmacro else (&body body)
  "This is just a wrapper for progn."
  `(progn ,@body))

(defmacro the-following (&body body)
  "This is just a wrapper for progn."
  `(progn ,@body))
;;;}}}

(defmacro make-keyword-list (&rest symbols)
  (let* ((l (mapcar (lambda (x) (list (make-keyword x) x))
                    symbols)))
    `',l))

(defun make-special-decls (&rest symbols)
  `(declare (special ,@symbols)))

(defmacro aif (condition then &optional else)
  `(let* ((it ,condition))
     (if it ,then ,else)))

(defmacro pp-expand (expr)
  `(print (macroexpand-1 ',expr)))

;;;{{{ macro with-elements

(defun remove-symbol-and-next-value-from-list (something list)
  "Removes all the instances of the sublist (something else) from the list. something is a symbol that was passed as the first argument, and else is the sexp that follows that symbol.

   The idea is to traverse the list, and not collecting something and the element following it."
  (loop for element in list
        with element-found = nil
        with removed-elements = 0
        if (and (eq element something)
                (not element-found)
                (= removed-elements 0))
           do (setf element-found t
                 removed-elements 1)
        else if (and element-found
                     (= removed-elements 1))
           do (setf element-found nil
                    removed-elements 0)
        else collect element))

(defmacro with-elements ((elements list) &body body)
  ;;;{{{ Documentation
  "Creates a context where the elements specified in elements are extracted
and removed from list.
Elements is a list of elements of the form: (var-name symbol-name)
var-name is the name used in the code, and symbol-name is the info
that should be extracted from list.
 Example:
   If list is '(:title hello :overlay 5 happy world)
   in the the call to the macro:
    (with-elements (((ovl :overlay)
                     (title :title)) list)
      code here...)
    the user can refer to the variables ovl with value 5 and title with value 'hello. The content of list in the code will be: '(happy world)."
  ;;;}}}

  (let* ((var-declarations
          (loop for (name keyword) in elements
                collect `(,name (getf ,list ,keyword))
                collect `(,list (remove-symbol-and-next-value-from-list
                                 ,keyword ,list)))))
    `(let* (,@var-declarations)
       ,@body)))

;;;}}}

(defparameter *vrp-debug* t
  "A variable that controls whether or not to output debug info.")

(defun round (x &optional (divisor 1))
  (declare (ignore divisor))
  (floor (+ x 1/2)))

(defun format-boxed (stream format-str &rest format-args)
  (let* ((string-to-print
          (if format-args
              (apply 'format `(nil ,format-str ,@format-args))
              (funcall 'format nil format-str)))
         (length (+ 2 (length string-to-print)))
         (=-line (make-string length :initial-element #\=)))
    (format stream "~a~% ~a~%~a~2%"
            =-line string-to-print =-line)))

(setf (symbol-function 'bformat) #'format-boxed)

(defun numbers-in-range (a &optional b)
    "Returns a list of numbers from a to b.  If optional arg b is ommited, return the numbers from 0 to a, both values included. If a > b, returns the empty list."
    (let* ((min (if b a 0))
           (max (if b b a)))
      (if b
          (if (<= a b)
              (loop for i from min to max collecting i))
          ;; else if b
          (loop for i from min to max collecting i))))

(defun numbers-in-range (a &optional b)
    "Returns a list of numbers from a to b.  If optional arg b is ommited, return the numbers from 0 to a, both values included"
    (let* ((min (if b a 0))
           (max (if b b a)))
    (if (< min max)
        (loop for i from min to max collecting i)
        (loop for i from min downto max collecting i))))

(defun manhattan-distance (coord1 coord2)
  "Returns the Manhattan distance between two points. Each point is a list with two elements."
  (+ (abs (- (first  coord1) (first  coord2)))
     (abs (- (second coord1) (second coord2)))))

(defun euclidean2d-distance (coord1 coord2)
  "Returns the euclidean 2d distance between two coordinates.instances of has-coordinates.  This distance is like the standard norm 2 distance, but the results are always integer."
  (round
   (sqrt
    (+ (expt (- (first coord1) (first coord2)) 2)
       (expt (- (second coord1) (second coord2)) 2)))))

(defun make-distance-matrix (coordinates
                             &optional
                               (distance-function #'euclidean2d-distance))
  "Returns a distance matrix from a list with coordinates.
  `coordinates' is a list with the coordinates for which we want to compute the distances, and `distance-function' is a function to calculate the distance between two coordinates."
  (let* ((n (length coordinates))
         (matrix (make-array (list n n)
                             :initial-element 0)))
    ;; ;; (format t "length: ~a, array: ~a~%" n matrix)
    (loop for c1 in coordinates
          for i1 from 0
          do (loop for c2 in coordinates
                   for i2 from 0
                   do (setf (aref matrix i1 i2)
                            (funcall distance-function c1 c2))))
    ;; finally return the matrix
    matrix))
