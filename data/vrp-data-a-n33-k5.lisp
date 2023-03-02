(in-package :vrp)

(defparameter A-n33-k5-coords
 '((42 68) (77 97) (28 64) (77 39) (32 33) (32 8) 
   (42 92) (8 3) (7 14) (82 17) (48 13) (53 82) 
   (39 27) (7 24) (67 98) (54 52) (72 43) (73 3) 
   (59 77) (58 97) (23 43) (68 98) (47 62) (52 72) 
   (32 88) (39 7) (17 8) (38 7) (58 74) (82 67) 
   (42 7) (68 82) (7 48)))

(defparameter A-n33-k5-distance-matrix
 (make-distance-matrix A-n33-k5-coords))

(defparameter A-n33-k5-demands
 '(5 23 14 13 8 18 19 10 18 20 5 9 23 9 18 
   10 24 13 14 8 10 19 14 13 14 2 23 15 8 20 
   24 3))

(defparameter A-n33-k5-problem
  (make-basic-cvrp-problem A-n33-k5-distance-matrix
                           A-n33-k5-demands
                           100
                           1))
