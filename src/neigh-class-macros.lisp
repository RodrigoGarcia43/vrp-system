(in-package :vrp)

(defmacro make-constructor (constructor-name class-name)
  `(defun ,constructor-name ()
     (make-instance ',class-name)))

(defmacro new-class (class-name
		     (&rest parents)
			(&rest slots)
			   (&key documentation)
			      (constructor-name))
  `(prog1
       (defclass ,class-name (,@parents)
	 (,@slots)
	 (:documentation ,documentation))
     (make-constructor ,constructor-name ,class-name)))
