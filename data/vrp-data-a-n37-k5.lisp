(in-package :vrp)

(defparameter A-n37-k5-coords
 '((38 46) (59 46) (96 42) (47 61) (26 15) (66 6) 
   (96 7) (37 25) (68 92) (78 84) (82 28) (93 90) 
   (74 42) (60 20) (78 58) (36 48) (45 36) (73 57) 
   (10 91) (98 51) (92 62) (43 42) (53 25) (78 65) 
   (72 79) (37 88) (16 73) (75 96) (11 66) (9 49) 
   (25 72) (8 68) (12 61) (50 2) (26 54) (18 89) 
   (22 53)))

(defparameter A-n37-k5-distance-matrix
 (make-distance-matrix A-n37-k5-coords))

(defparameter A-n37-k5-demands
 '(16 18 1 13 8 23 7 27 1 3 6 24 19 2 5 
   16 7 4 22 7 23 16 2 2 9 2 12 1 9 23 
   6 19 7 7 20 20))

(defparameter A-n37-k5-problem
  (make-basic-cvrp-problem A-n37-k5-distance-matrix
                           A-n37-k5-demands
                           100
                           1))
