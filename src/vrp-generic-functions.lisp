(in-package :vrp)

;;;{{{ clone
(defgeneric clone (x)
  (:documentation "Returns a clone of the given instance."))

(defmethod clone ((x (eql nil)))
  "The clone of nil is nil."
  nil)

(defmethod clone ((x number))
  "The clone of a number is the number itself."
  x)

(defmethod clone ((x string))
  "The clone of a string is a new string."
  (copy-seq x))

(defmethod clone ((x symbol))
  "The clone of a string is a new string."
  (copy-symbol x t))

(defmethod clone ((l list))
  "The clone of a list is a new list with each element cloned."
  (loop for e in l
        collecting (clone e)))

(defmethod clone ((f function))
  "The clone of a function is the same function.  THIS IS NOT A CLONE."
   f)

(defun clone-1d-array (x)
  "This function returns a clone of a 1d array."
  (let* ((xsize (array-dimensions x))
         (n1 (first xsize))
         (adjustable (adjustable-array-p x))
         (fpointer (if (array-has-fill-pointer-p x)
                       (fill-pointer x)))
         (result (if adjustable
                     (then
                       (make-array (list n1) :adjustable t
                                   :fill-pointer fpointer))
                     (else
                       (make-array (list n1))))))
    (loop for i from 0 below n1
          doing (setf (aref result i)
                      (clone (aref x i))))
    ;; return result
    result))

(defun clone-2d-array (x)
  (let* ((xsize (array-dimensions x))
         (n1 (first xsize))
         (n2 (second xsize))
         (result (make-array (list n1 n2))))
    (loop for i from 0 below n1
          doing (loop for j from 0 below n2
                      doing (setf (aref result i j)
                                  (clone (aref x i j)))))
    ;; return result
    result))

(defmethod clone ((x array))
  (let* ((xsize (array-dimensions x))
         (n2 (second xsize)))
    (if n2 ;; it is a 2d array
        (then (clone-2d-array x))
        (else (clone-1d-array x)))))

(defmethod clone ((x hash-table))
  (let* ((result (make-hash-table)))
    (loop for key being the hash-key of x
           using (hash-value value)
          doing (setf (gethash (clone key) result)
                      (clone value)))
    ;; let's return result
    result))
;;;}}}

;;;{{{ obj=
(defgeneric obj= (x y)
  (:documentation "Returns t if two instances are equal."))

(defmethod obj= ((x t) (y t))
  "Unless explicitly stated, instances of two different classes are not obj="
  nil)

(defmethod obj= ((x symbol) (y symbol))
  "Two symbols are obj= if they are eq."
  (eq x y))

(defmethod obj= ((x number) (y number))
  "Two numbers are obj= if they are =."
  (= x y))

(defparameter *zero-tol* 1d-12)
(defmethod obj= ((x real) (y real))
  "Two numbers are obj= if they are =."
  (< (abs (- x y)) *zero-tol*))

(defmethod obj= ((x real) (y number))
  "Two numbers are obj= if they are =."
  (< (abs (- x y)) *zero-tol*))

(defmethod obj= ((x number) (y real))
  "Two numbers are obj= if they are =."
  (< (abs (- x y)) *zero-tol*))

(defmethod obj= ((x string) (y string))
  "Two strings are obj= if they are string=."
  (string= x y))

(defmethod obj= ((x list) (y list))
  "Two lists are obj= if every element are obj="
  (let* ((lists-are-equal nil))
    (setf lists-are-equal (= (length x) (length y)))
    (loop for e1 in x
          for e2 in y
          while lists-are-equal
          doing (unless (obj= e1 e2) (setf lists-are-equal nil)))
    ;; return lists-are-equal
    lists-are-equal))


;; help functions for the obj= for arrays

(defun obj=1-d-array (x y)
  "Checks if 2 1d array are obj=."
  (let* ((xsize (length x))
         (ysize (length y))
         (arrays-are-equal (= xsize ysize)))
    (loop for elt-x across x
          for elt-y across y
          while arrays-are-equal
          doing (unless (obj= elt-x elt-y)
                  (setf arrays-are-equal nil)))
    ;; return arrays-are-equal
    arrays-are-equal))

(defun obj=2d-array (x y)
  "Returns t if x and y are 2d array of the same size and all its elements are obj=."
  (let* ((xsize (array-dimensions x))
         (ysize (array-dimensions y))
         (n1 (first xsize))
         (n2 (second xsize))
         (arrays-are-equal
          (and (obj= (first ysize) n1)
               (obj= (second ysize) n2))))
    (loop for i from 0 below n1
          while arrays-are-equal
          doing (loop for j from 0 below n2
                      while arrays-are-equal
                      doing (setf arrays-are-equal
                                  (obj= (aref x i j)
                                        (aref y i j)))))
    ;; return arrays-are-equal
    arrays-are-equal))

(defmethod obj= ((x array) (y array))
  "Returns t if two arrays are equal. The arrays should be 1d or 2d."
  (let* ((xsize (array-dimensions x))
         (ysize (array-dimensions y))
         (arrays-may-be-equal (obj= xsize ysize)))
    (if arrays-may-be-equal
        (cond
          ((= (length xsize) 1) (obj=1-d-array x y))
          ((= (length xsize) 2) (obj=2d-array x y))))))

(defmethod obj= ((x hash-table) (y hash-table))
  (let* ((keysx nil)
         (keysy nil)
         (valsx nil)
         (valsy nil))
    (loop for key-x being the hash-key of x
          do (push key-x keysx)
          do (push (gethash key-x x) valsx))

    (loop for key-y being the hash-key of y
          do (push key-y keysy)
          do (push (gethash key-y y) valsy))

    (and (obj= keysx keysy)
         (obj= valsx valsy))))
;;;}}}
