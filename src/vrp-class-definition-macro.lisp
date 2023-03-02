(in-package :vrp)

(defun make-obj=-code (class-name slots)
  "Returns a list with the obj= methods.
class-name is the name of the class. slots is a list with the slots that should be used for comparison."

  `(
    (defmethod obj= ((x ,class-name) (y t))
      ,(format nil "An instance of ~a and something that is not a ~a are not obj=."
               class-name class-name)
      nil)
  
    (defmethod obj= ((y t) (x ,class-name))
      ,(format nil "Something that is not a ~a and an instance of ~a are not obj=."
               class-name class-name)
      nil)

    (defmethod obj= ((x ,class-name) (y ,class-name))
      (and 
       ,@(loop for s in slots
               collecting `(obj= (,s x) (,s y)))))))

(defun make-clone-code (class-name slots)
  "Returns a list with the clone method.
class-name is the name of the class. slots is a list with the slots that should be cloned."

  `(defmethod clone ((obj ,class-name))
       ,(format nil "Returns a clone of an instance of ~a."
                class-name)
       (make-instance (class-name (class-of obj)) 
       ,@(loop for x in slots
               collecting (make-keyword x) 
               collecting `(clone (,x obj))))))

(defun make-print-obj-code (class-name format-string slots)
  `(defmethod print-object ((obj ,class-name) stream)
     (format stream ,format-string
             ,@(mapcar (lambda (x) `(,x obj)) slots))))

(defun make-constructor-code (class-name constructor-name lambda-list)
  `(defun ,constructor-name ,lambda-list
     (make-instance ',class-name 
       ,@(flatten 
          (loop for slot in lambda-list
                if (and (atom slot)
                        (not (string= "&" (subseq (symbol-name slot) 0 1))))
                collect `(,(make-keyword slot) ,slot)
                if (listp slot)
                collect `(,(make-keyword (car slot)) ,(car slot)))))))

(defun make-is-a-class-name (class-name)
  (let* ((function-name (symb "is-a-" class-name)))
    `((defgeneric ,function-name (obj)
        (:documentation 
          ,(format nil "Returns obj if it is an instance of ~a."
                   class-name)))
      (defmethod ,function-name ((obj t))
          ,(format nil "By default, things are not instances of ~a." 
                   class-name)
          nil)
      (defmethod ,function-name ((obj ,class-name))
        ,(format nil "If obj is an instance of ~a, return it."
                 class-name)
          obj))))

(defun make-slot-definition (slot-data)
  (let* ((slot-name (first slot-data))
         (data-with-accessor
          (if (not (member :accessor slot-data))
              (append  `(,slot-name)
                        `(:accessor ,slot-name)
                        (rest slot-data))
              (else
                slot-data)))
         (data-with-initarg
          (if (not (member :initarg data-with-accessor))
              (append  data-with-accessor
                       `(:initarg ,(make-keyword slot-name)))
              (else
                data-with-accessor))))
    ;; return the final data
    data-with-initarg))

(defmacro def-vrp-class (class-name parents slots
                          &key documentation 
                            constructor 
                            print-object-string
                            slots-for-obj=
                            slots-for-clone)

  (let* (;; first we add the class definition
         (class-definition
          `(defclass ,class-name ,parents
               ;; the next line was added to create the slot data
                  ,(mapcar (lambda (x) (make-slot-definition x)) slots)
                   (:documentation ,documentation)))
         ;; the code for the constructor
         (constructor-code
          (if constructor
              (make-constructor-code class-name
                                      (car constructor)
                                      (cadr constructor))))
         ;; the code for the print-object
         (print-object-code
          (if print-object-string
              (make-print-obj-code
               class-name
               (car print-object-string) 
               (rest
                print-object-string))))         
         ;; the code for obj=
         (obj=-code
          (if slots-for-obj=
              (make-obj=-code class-name slots-for-obj=)))
         ;; the code for the clone method
         (clone-code
          (if slots-for-clone
              (make-clone-code class-name slots-for-clone)))
         ;; the code for is a class-name
         (is-a-class-name-code
          (make-is-a-class-name class-name))
         ;; the code with everything
         ;; initially it only has the class defintion and the
         ;; is-a-class-name function
         ;; the reverse is necessary because if it is not there
         ;; the methods are defined before the generic-function
         ;; and this creates warnings in sbcl.
         (code `(,@(reverse is-a-class-name-code) ,class-definition)))

    ;; here we add the code for everything
    ;; first the constructor
    (if constructor-code
        (push constructor-code code))
    ;; now we add the print-object-code, if it not nil
    (if print-object-code
        (push print-object-code code))
    ;; now we add the clone-code, if it not nil
    (if clone-code
        (push clone-code code))
    ;; here we reverse code that has the wrong order
    ;; because of the pushs
    (setf code (reverse code))
    ;; the obj=-code
    (if obj=-code
        (setf code (append code obj=-code)))
    `(progn 
       ,@code)))
