(in-package :vrp)

(defparameter a-n33-k6-coords
 '((34  31) (45  55) (70  80) (81  70) (85  61) (59  55) (45  60)
   (50  64) (80  64) (75  90) (25  40) ( 9  66) ( 1  44) (50  54)
   (35  45) (71  84) ( 1   9) (25  54) (45  59) (45  71) (66  84)
   (11  35) (81  46) (85  10) (75  20) (15  21) (90  45) (15   0)
   (31  26) (10  95) ( 6   6) (51   5) (26  36)))

(defparameter a-n33-k6-distance-matrix
 (make-distance-matrix a-n33-k6-coords))

(defparameter a-n33-k6-demands
 '(26 17  6 15  7  5 15 16 17  1 21 66 25 16 11  7 
   17 17 22 10 25 16  7 21 11 21 11 21 22 25  2 22))

(defparameter a-n33-k6-problem
  (make-basic-cvrp-problem a-n33-k6-distance-matrix
                           a-n33-k6-demands
                           100
                           1))

(defparameter ff-a-n33-k6-problem
  (make-finite-fleet-cvrp-problem a-n33-k6-distance-matrix
                                  a-n33-k6-demands
                                  `(100 100 100 100 100 100)
                                  1))
