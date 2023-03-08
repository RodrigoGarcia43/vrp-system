(let* ((a 1)
       (b 'hello)
       (pepe "jose")
       (l (make-keyword-list a b pepe)))
  (bformat t "Testing make-keyword-list")
  (print l))

(let* ((a 1)
       (b 'hello)
       (pepe "jose")
       (l (make-special-decls 'a 'b 'pepe)))
  (bformat t "Testing make-keyword-list")
  (print l))

(let* ((test-list `(hello world :hello 5 :blue "red")))
  (deftests "testing remove-symbol-and-next-value-from-list:"
    (check-list= `(hello world :blue "red")
                 (remove-symbol-and-next-value-from-list :hello test-list))
    (check-list= `(:hello 5 :blue "red")
                 (remove-symbol-and-next-value-from-list 'hello test-list))
    (check-list= `(hello world :hello 5)
                (remove-symbol-and-next-value-from-list :blue test-list))))

(let* ((list '(:title hello :overlay 5 happy world)))
 (with-elements (((ovl :overlay)
                  (title :title)) list)
   (deftests "Testing macro with-elements:"
     (check-= 5 ovl)
     (check-eq 'hello title)
     (check-list= '(happy world) list))))

(progn
  (print (numbers-in-range 3 8))
  (print (numbers-in-range 5))
  (print (numbers-in-range -3 5))
  (print (numbers-in-range 5 -5)))

(let* ((coords '((0 0) (1 2) (3 4) (5 6) (7 8) (9 1) (2 3) (4 5))))
  (format t "===========================
Testing manhattan distance:
===========================~2%")
  (loop for c1 in '((0 0) (1 2))
        doing (format t "From ~a:~%" c1)
        doing (loop for c2 in (rest coords)
                    do (format t "  Manhattan distance from ~a to ~a: ~a~%"
                               c1 c2
                               (manhattan-distance c1 c2))))))

(let* ((coords '((0 0) (1 2) (3 4) (5 6) (7 8) (9 1) (2 3) (4 5))))
  (format t "=============================
Testing euclidean2d-distance:
=============================~2%")
  (loop for c1 in '((0 0) (1 2))
        do (format t "From ~a:~%" c1)
        do (loop for c2 in (rest coords)
                 do (format t "  Euclidean2d-distance from ~a to ~a: ~a (~a)~%"
                            c1 c2
                            (euclidean2d-distance c1 c2)
                            (sqrt
                             (+ (expt (- (first c1) (first c2)) 2)
                                (expt (- (second c1) (second c2)) 2)))))))

(let* ((coords '((0 0) (1 2) (3 4) (5 6) (7 8) (9 1) (2 3) (4 5)))
       (manhattan-matrix
        (make-distance-matrix coords 'manhattan-distance))
       (euclidean2d-matrix
        (make-distance-matrix coords 'euclidean2d-distance))
       (n (length coords)))
  (format t "=============================
Testing make-distance-matrix:
=============================~2%")
  (format t "Manhattan-distance:~2%")
  (loop for i from 0 below n
        do (format t "  | ~2d" (aref manhattan-matrix i 0))
        do (loop for j from 1 below n
                 do (format t " ~2d" (aref manhattan-matrix i j)))
        do (format t " |~%"))
  (format t "~%Euclidean2d-distance:2~%")
  (loop for i from 0 below n
        do (format t "  | ~2d" (aref euclidean2d-matrix i 0))
        do (loop for j from 1 below n
                 do (format t " ~2d" (aref euclidean2d-matrix i j)))
        do (format t " |~%"))

  (format t "Checking the contents:~%")
  (loop for i from 0 below n
        for c1 in coords
        do (loop for j from 0 below n
                 for c2 in coords
                 do (check-= (aref manhattan-matrix i j)
                             (manhattan-distance c1 c2)))))

(in-package :vrp)

(print (check-t (= 2 (+ 1 1))))
(print (check-t (= 2 (+ 1 3))))

(let* ((*vrp-unit-testing-indent* 6))
  (check-t (= 4 (+ 1 3))))

(print (check-t (= 2 (+ 1 1))))
(print (check-t (= 2 (+ 1 3))))

(let* ((*vrp-unit-testing-indent* 6))
  (check-t (= 4 (+ 1 3))))

(print (check-nil (= 3 (+ 1 1))))
(print (check-nil (= 2 (+ 1 3))))

(pp-expand (make-check str= string=))

(check-= 2 (+ 1 1))
(check-= 4 (+ 1 3))
(check-= 6 (+ 1 3))

(let* ((*vrp-unit-testing-display-output* nil))
  (check-= 2 (+ 1 1))
  (check-= 4 (+ 1 3))
  (check-= 6 (+ 1 3)))

(let* ((*vrp-unit-testing-display-results* nil)
       (*vrp-unit-testing-display-output* nil))
  (check-= 2 (+ 1 1))
  (check-= 4 (+ 1 3))
  (check-= 6 (+ 1 3)))

(check-eq 'd 'd)
(check-eq 'hello "hello")

(check-str= "d" "d")
(check-str= "hello" (format nil "he~a" "llo"))

(check-list= '(1 2 3) '(1 2 3))
(check-list= '(1 2 3) (append '(1 2) (list 3)))
(check-list= '(1 2 3) (append '(1) (list 2)))

(check-obj= '(1 2 3) '(1 2 3))
(check-obj= 5 5)
(check-obj= 'hello 'hello)

(print (make-obj=-code 'cvrp-client '(id demand)))

(print (make-clone-code 'cvrp-client '(id demand delivery)))

(print (make-print-obj-code 'cvrp-client "<client ~a, ~a ~a>" 
  '(id delivery demand)))

(print
 (make-constructor-code 'cvrp-client 'new-cvrp-client '(id &key (demand 0) (delivery 0))))

(print 
  (make-is-a-class-name 'cvrp-client))

(print (make-slot-definition '(id)))
(print (make-slot-definition '(id :accessor ide)))
(print (make-slot-definition '(id :initform :pepe)))
(print (make-slot-definition '(id :accessor ide :initform 6)))

(pp-expand
 (def-vrp-class cvrp-client ()
   ((id) (demand) (delivery))
   :documentation "A class to represent a client with capacity."
   :constructor (new-cvrp-client (id &key (demand 0) (delivery 0)))
   :print-object-string ("<cvrp-client ~a, ~a, ~a>" id demand delivery)
   :slots-for-obj= (id demand delivery)
   :slots-for-clone (id demand delivery)))

(pp-expand
 (def-vrp-class cvrp-client ()
   ((id) (demand) (delivery))
   :documentation "A class to represent a client with capacity."))

(pp-expand
 (def-vrp-class cvrp-client ()
   ((id) (demand) (delivery))
   :documentation "A class to represent a client with capacity."
   :slots-for-obj= (id demand delivery)
   :slots-for-clone (id demand delivery)))
