(in-package :vrp)

(make-classical-criterion rarebf ((select-route r1)
				  (select-client c1 from r1)
				  (select-route r2)
				  (select-subroute z1 from r2)
				  (insert-client c1 into r2)
				  (insert-subroute z1 into r1)))

(defparameter *code-rarebf* '((select-route r1)
			      (select-client c1 from r1)
			      (select-route r2)
			      (select-subroute z1 from r2)
			      (insert-client c1 into r2)
			      (insert-subroute z1 into r1)))

(make-classical-criterion rarebhf ((select-route r1)
				   (select-client c1 from r1)
				   (select-route r2)
				   (select-subroute z1 from r2)
				   (insert-client c1 into r2)
				   (reverse-subroute z1)
				   (insert-subroute z1 into r1)))

(defparameter *code-rarebhf* '((select-route r1)
			      (select-client c1 from r1)
			      (select-route r2)
			      (select-subroute z1 from r2)
			      (insert-client c1 into r2)
			      (reverse-subroute z1)
			      (insert-subroute z1 into r1)))

(make-classical-criterion ref ((select-route r1)
			       (select-subroute z1 from r1)
			       (insert-subroute z1 into r1)))

(defparameter *code-ref* '((select-route r1)
			  (select-subroute z1 from r1)
			  (insert-subroute z1 into r1)))

(make-classical-criterion rerf ((select-route r1)
				(select-subroute c1 from r1)
				(select-route r2)
				(insert-subroute c1 into r2)))

(defparameter *code-rerf* '((select-route r1)
			   (select-subroute c1 from r1)
			   (select-route r2)
			   (insert-subroute c1 into r2)))

(make-classical-criterion rereg ((select-route r1)
				 (select-subroute c1 from r1)
				 (select-route r2)
				 (select-subroute c2 from r2)
				 (swap-subroutes c1 c2)))

(defparameter *code-rereg* '((select-route r1)
			    (select-subroute c1 from r1)
			    (select-route r2)
			    (select-subroute c2 from r2)
			    (swap-subroutes c1 c2)))

(make-classical-criterion rehf ((select-route r1)
				(select-subroute z1 from r1)
				(reverse-subroute z1)
				(insert-subroute z1 into r1)))

(defparameter *code-rehf* '((select-route r1)
			   (select-subroute z1 from r1)
			   (reverse-subroute z1)
			   (insert-subroute z1 into r1)))

(make-classical-criterion rehrf ((select-route r1)
				 (select-subroute z1 from r1)
				 (select-route r2)
				 (reverse-subroute z1)
				 (insert-subroute z1 into r2)))

(defparameter *code-rehrf* '((select-route r1)
			    (select-subroute z1 from r1)
			    (select-route r2)
			    (reverse-subroute z1)
			    (insert-subroute z1 into r2)))

(make-classical-criterion rehreg ((select-route r1)
				  (select-subroute c1 from r1)
				  (select-route r2)
				  (select-subroute c2 from r2)
				  (reverse-subroute c1)
				  (swap-subroutes c1 c2)))

(defparameter *code-rehreg* '((select-route r1)
			     (select-subroute c1 from r1)
			     (select-route r2)
			     (select-subroute c2 from r2)
			     (reverse-subroute c1)
			     (swap-subroutes c1 c2)))

(make-classical-criterion rerehg ((select-route r1)
				  (select-subroute c1 from r1)
				  (select-route r2)
				  (select-subroute c2 from r2)
				  (reverse-subroute c2)
				  (swap-subroutes c1 c2)))

(defparameter *code-rerehg* '((select-route r1)
			     (select-subroute c1 from r1)
			     (select-route r2)
			     (select-subroute c2 from r2)
			     (reverse-subroute c2)
			     (swap-subroutes c1 c2)))

(make-classical-criterion rehrehg ((select-route r1)
				   (select-subroute c1 from r1)
				   (select-route r2)
				   (select-subroute c2 from r2)
				   (reverse-subroute c1)
				   (reverse-subroute c2)
				   (swap-subroutes c1 c2)))

(defparameter *code-rehrehg* '((select-route r1)
			      (select-subroute c1 from r1)
			      (select-route r2)
			      (select-subroute c2 from r2)
			      (reverse-subroute c1)
			      (reverse-subroute c2)
			      (swap-subroutes c1 c2)))

(defun unflatten-list (lista num-subl)
  (let* ((total-len (length lista))
	 (pos-gen (random-sample-from-range 2 total-len))
	 (pos-list (loop for i from 1 to (1- num-subl)
		      collecting
			(funcall pos-gen)))
	 (slice-ranges nil))
    (setf pos-list (sort (push (1+ total-len)
			       pos-list)
			 #'<))
    (setf slice-ranges (loop for i from 1 to (1- num-subl)
			  collecting
			    (cons (nth (1- i) pos-list)
				  (1- (nth i pos-list)))))
    (push (cons 1 (1- (first pos-list)))
	  slice-ranges)
    (loop for (start . end) in slice-ranges
       collecting
	 (slice lista start end))))

(defun generate-random-solution (vrp-problem num-routes)
  (let* ((permutation-gen (random-sample-from-range 1 (length (clients vrp-problem))))
	 (cur-client (funcall permutation-gen))
	 (clients-list nil))
    ;; here we create the client permutation
    (loop while (not (null cur-client))
       do
	 (push cur-client clients-list)
	 (setf cur-client (funcall permutation-gen)))
    ;; here we unflatten the client list
    (unflatten-list clients-list num-routes)))

(defparameter *emno-problems* (list a-n32-k5-problem a-n65-k9-problem a-n80-k10-problem))

(defparameter *emno-neighborhood-criteria*
  (list *code-rarebf* *code-rarebhf* *code-ref* *code-rerf* *code-rereg*
	*code-rehf* *code-rehrf* *code-rehreg* *code-rerehg* *code-rehrehg*))

(defparameter a-n32-k5-problem-sol1
  (with-basic-cvrp-solution (s1 ((31 10 18 11 29 12)
				 (23 14)
				 (1 27 13 30 4 3 28 15 8 20 25 19)
				 (22 5)
				 (6 24 7 2 26 21 9 17 16))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-sol2
  (with-basic-cvrp-solution (s1 ((22 31)
				 (8)
				 (3 7 14 11 5)
				 (29 28 12 24 30 20 23 25 4 1 6 16 27 10 2)
				 (15 21 9 13 18 26 19 17))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-sol3
  (with-basic-cvrp-solution (s1 ((6)
				 (11 31)
				 (25 8 2 14 13 27 15 30 17 4)
				 (23 28 1 16 3)
				 (18 26 10 19 22 24 5 9 20 29 21 7 12))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-sol4
  (with-basic-cvrp-solution (s1 ((15 9)
				 (1 30 16)
				 (11 29 22 2 4 27 13)
				 (18 17)
				 (20 31 23 19 5 3 21 26 6 25 14 10 28 12 7 24 8))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-sol5
  (with-basic-cvrp-solution (s1 ((24 31 7 1 20 23 26)
				 (10 17 8)
				 (3 19 22 9 16 13)
				 (6 27 21 28 25 11 15)
				 (14 4 5 12 2 30 18 29))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-sol6
  (with-basic-cvrp-solution (s1 ((31 27 1 22 26)
				 (4 9 19 30 12 8 13 2)
				 (17 23 5)
				 (20 6 21 16 7 28)
				 (10 24 29 18 3 11 25 14 15))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-sol7
  (with-basic-cvrp-solution (s1 ((29 31 6 30 7)
				 (9)
				 (3 17 4 28 14 13 10 25 20 1 23 27 2 11 5 26)
				 (19)
				 (12 8 21 16 24 22 15 18))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-sol8
  (with-basic-cvrp-solution (s1 ((4 31 2 9)
				 (28 5 21 8 13 3 19 30 17 26 25)
				 (1 15 23 29 6)
				 (24 22 12 10)
				 (18 11 20 16 27 7 14))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-sol9
  (with-basic-cvrp-solution (s1 ((31 23)
				 (18)
				 (30)
				 (16 15 27 25 29 4 8 9 1 20 2 17 3)
				 (12 10 24 6 11 7 5 26 19 13 21 22 28 14))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-sol10
  (with-basic-cvrp-solution (s1 ((28 14 8 31 5)
				 (10 29 17 16 24 22 4 23 20 27 13)
				 (19)
				 (3 12 18 11 30 6 1 26 15)
				 (25 2 21 7 9))
				a-n32-k5-problem)
    s1))

(defparameter a-n65-k9-problem-sol1
  (with-basic-cvrp-solution (s1 ((26 34)
				 (12 60)
				 (9 39 1 2 64 61 30 47 53 44 18)
				 (23 25 46 29 10 58 63 3 59 28 15 37 45 31 54)
				 (36 62 43)
				 (22 16)
				 (48 24 51 20 56 21 19)
				 (33 35 8 27 41 50 42 4 5 49 14)
				 (7 57 55 6 52 11 32 13 38 40 17))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-sol2
  (with-basic-cvrp-solution (s1 ((14)
				 (13 8 64 16 35 49 25 31 1)
				 (33 54)
				 (55 4 41 42 26 46 18 7 11 29 28 58 38 27 51 57 56 36 22 34 20 10 5 63 32 40 59
				     15 50 24 52 47 12 3 60 6)
				 (19 53 48 37 21 62)
				 (61)
				 (9)
				 (43 44)
				 (2 39 45 23 30 17))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-sol3
  (with-basic-cvrp-solution (s1 ((58)
				 (49 59 64)
				 (61 4 9 15 22)
				 (12 57 35 30 43 46)
				 (25 38 13 20 8 19 27 18 26 40 10 45 36 7)
				 (48 17 52)
				 (47 16 21 23 28 50 62 41 55 44 6 63 37 2 32 24 14 51 60 31 56)
				 (42 53 33 5 54 11 1)
				 (3 39 34 29))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-sol4
  (with-basic-cvrp-solution (s1 ((34 14 50 64 4 53 7 52 18 15 58 9 11 56)
				 (1)
				 (60)
				 (6 13 62 21 25 40 36 55 12 42 32 51 48 8 35)
				 (63)
				 (49)
				 (33 31 5 37)
				 (23 24 28 47 17 16 22 10 46 39 20 3 2 44 61 59 57 26 30 54 29)
				 (45 38 41 19 27 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-sol5
  (with-basic-cvrp-solution (s1 ((16 64)
				 (5)
				 (6 48 58)
				 (37 46 35 33 51)
				 (10 62 59 34)
				 (15 55 30 47 20 57 27 52 2 3 11 4 14 40 1 41 29 9 53 28 31 22 19 38 25 60 56
				     39 17 50 63 36 12 61)
				 (26 8 49 43)
				 (24 18 32)
				 (23 44 21 7 45 54 42 13))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-sol6
  (with-basic-cvrp-solution (s1 ((64 14 35 42 63)
				 (39 40 11 31 1 29 9 22 44 50)
				 (51 54 4 34 19 6 30 59 3)
				 (37 58 48 26 7 38 56 32 13)
				 (36 25)
				 (28 27 10 55 57 8 47 45 18)
				 (20)
				 (23 49 52 33 21 43 2 46 62 61 12)
				 (53 16 24 17 60 15 41 5))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-sol7
  (with-basic-cvrp-solution (s1 ((29 4 48 64 16 63 50 44 10 37 2 54 51 23 21 59)
				 (57 11)
				 (45)
				 (53 49 7 61 3 36 31)
				 (62 26 25 39 28 1 38 41 30 14 35 19 33)
				 (52 55 22 32 43 13 46 8 24)
				 (5 20)
				 (60 12 34 47 18 17 6)
				 (9 42 27 15 56 58 40))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-sol8
  (with-basic-cvrp-solution (s1 ((50 49 29 62 64 21 1 11 28 39)
				 (24 23 60 35 37 7 40 30 4 51 56 20 52)
				 (57)
				 (54 16 27)
				 (12 3 17 5)
				 (14 48)
				 (55 59 47 63 32 8 42 6 15 44)
				 (34 18 2 22 9 46 33 43 31 10 41 61 19 53 13)
				 (26 58 36 25 38 45))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-sol9
  (with-basic-cvrp-solution (s1 ((13 41 64 39 17 36)
				 (11 47 25 60 40 14 26 58 46 48 24 42 30 49 20 9 7 51 31 44 4 1)
				 (52 19 57 8 62)
				 (35 54 63)
				 (16 59 29 21 10 38 55 2 34 5 22)
				 (53 6 56 28 50 32 43)
				 (61 45 33 3 15)
				 (18)
				 (23 27 37 12))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-sol10
  (with-basic-cvrp-solution (s1 ((27 40 12 41 64 59 51 58 42 3 38 30 54 20 32 28 48 26 4)
				 (57 56 8)
				 (50 1 35 25 22 34 36 17 23 6 21 44 62 31 14 39)
				 (16 15 7)
				 (61)
				 (47 63 52 49 24 33 11 46 19)
				 (10 2 18 43)
				 (9 13)
				 (53 29 5 60 37 45 55))
				a-n65-k9-problem)
    s1))

(defparameter a-n80-k10-problem-sol1
  (with-basic-cvrp-solution (s1 ((77 53 8 65 25 51 16 28)
				 (19 26 48 50 29 39 54 79 63 35 69 21 33)
				 (10 75 15 71 66 64 52 31 59 9 55)
				 (40 37 67 47)
				 (1 45 23 12 32 36 38 42)
				 (56 76 60 70 57 61 74 41 44 20 6 73)
				 (13 7 4 58 30 49 46 18)
				 (11 22 2 3 78 43 72 68 34 27)
				 (5 17 62)
				 (14 24))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-sol2
  (with-basic-cvrp-solution (s1 ((61 44 11 40 16 55 34 79 4 25 70 38 75 12 69 13 37 59 50 62)
				 (73 78 63 36)
				 (30 7 49 31 48 19 20 74 52 53 68 45 77 57)
				 (33 72 6)
				 (46 67 32 43 18 54 17 60 26 3 10 29)
				 (42 56 51 66 28 2)
				 (76 8 27 64 14 23 41 35 71 22 47)
				 (1)
				 (65 39 24 5)
				 (9 21 15 58))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-sol3
  (with-basic-cvrp-solution (s1 ((79 69 74 65 2 51 20 14 28 19 16 4 17 5 47 8 75 46 29 72 39 59 35 71)
				 (68 30 1 49 62)
				 (73 13 44 12 60 63)
				 (76 78 53)
				 (40 15 43 58 33 25 21 32)
				 (42 37 31 24 22 34 36 48 52)
				 (3 54 38 45 61 64 70 50 11)
				 (10)
				 (77 23 26 66 67 55 56 6)
				 (27 7 9 57 41 18))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-sol4
  (with-basic-cvrp-solution (s1 ((79 29 11 31 67 18 59 60)
				 (74 68)
				 (20 12 47 17 39 40 73 30 32 72 36 37 53 46 38 45)
				 (14 62 16 35 21 64 9 26 65 4) (58 43 71 19 6 8)
				 (69 70)
				 (13 61 55 22)
				 (78 28 56 51 34 77 66)
				 (2 25 48 41 75 7 50)
				 (3 63 24 54 15 1 52 27 23 57 10 42 49 76 33 44 5))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-sol5
  (with-basic-cvrp-solution (s1 ((52 3 79 53)
				 (45 16 18 20 36 21 75)
				 (58 50 32)
				 (77 76 11 42 2 4 62 31 14 26 35 29 17 70 12 34 72 66 6 49 63)
				 (59)
				 (61 39 48 67 73)
				 (19 23 43 30)
				 (54 28 44 27 13 37 65 55 57 51 33 9 15 69 8 7 10 64 40)
				 (25 78 1 41 60 68)
				 (5 46 22 38 24 56 71 74 47))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-sol6
  (with-basic-cvrp-solution (s1 ((70 79 3 24 12 68 17 67)
				 (2 64 45 71 46 21 47)
				 (14 49 58 60 40 31 69 28 35 30 11 77 18 55 6 26 13 48)
				 (34 74 22 36 20 37 42 32 53 29 73 57)
				 (4 27 33 43 72)
				 (59 63 41 7 19 8 25 54 52 51 75 1 23 44 50 66 56 38 76 61)
				 (65 5)
				 (15)
				 (10 62 9 39)
				 (78 16))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-sol7
  (with-basic-cvrp-solution (s1 ((79 45 22)
				 (27 74)
				 (70 37 54 68 62 18 49)
				 (43 8)
				 (16 71 56 75 72 31 41 77 73 19 66)
				 (44)
				 (21 14 57 69 25 76 2 28 46 47 20 65 36 60 32 1 61 5 11 4 26 59 53 40 64 7 63
				     48 33 3 17 38 34 50 42 9)
				 (51 29 23 30)
				 (67 15 39 12 10 24 6 52 78 35 58)
				 (55 13))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-sol8
  (with-basic-cvrp-solution (s1 ((71 79 37 60 54 7 68 21 17)
				 (63 48 25 10 36 33 46 59 30 32 31 39 24 4 27 69 8 73 19 11 28 57 43 15)
				 (67 20 55 1 66)
				 (56 42 49 29 2 64 14 40 65 44)
				 (6 13 22 51 16 12 62 18 53 45 35)
				 (78 77 38 72 26 75 9 50)
				 (47 76 41 5 70)
				 (34)
				 (58 74 52 61)
				 (23 3))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-sol9
  (with-basic-cvrp-solution (s1 ((23 28)
				 (70 79 40 44)
				 (39)
				 (46 53)
				 (6 14 56)
				 (12 75 18 3 16 51 61 65 34 66 49 54 21 36 29 4 15 73 71 64 41 30 74)
				 (1 59 27 20 35 52 31 22 60 43 77 50 19 37 7 47)
				 (45 68 10 55 76 13 58 9 62 72 11 24 5 38 57 63)
				 (26 69 78)
				 (33 25 48 32 8 2 67 42 17))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-sol10
  (with-basic-cvrp-solution (s1 ((8)
				 (31 44 47 59 5 35 28 67 7 29 69 68 70)
				 (30 39 27 4 45 23 36 65 26 37 19 38 10 48 57 73 16 78)
				 (79 6)
				 (76)
				 (17 72 25)
				 (40 56 2 53 24 1 49)
				 (75 60 63 12 11 41 42 21 3 55 50 13 61 51 18)
				 (34 22 52 32 74 71 64 66 15 54 77 9 14 46 58 43)
				 (62 20 33))
				a-n80-k10-problem)
    s1))

(defparameter *emno-problem-solutions*
  (list (list a-n32-k5-problem-sol1 a-n32-k5-problem-sol2
	      a-n32-k5-problem-sol3 a-n32-k5-problem-sol4
	      a-n32-k5-problem-sol5 a-n32-k5-problem-sol6
	      a-n32-k5-problem-sol7 a-n32-k5-problem-sol8
	      a-n32-k5-problem-sol9 a-n32-k5-problem-sol10)
	(list a-n65-k9-problem-sol1 a-n65-k9-problem-sol2
	      a-n65-k9-problem-sol3 a-n65-k9-problem-sol4
	      a-n65-k9-problem-sol5 a-n65-k9-problem-sol6
	      a-n65-k9-problem-sol7 a-n65-k9-problem-sol8
	      a-n65-k9-problem-sol8 a-n65-k9-problem-sol10)
	(list a-n80-k10-problem-sol1 a-n80-k10-problem-sol2
	      a-n80-k10-problem-sol3 a-n80-k10-problem-sol4
	      a-n80-k10-problem-sol5 a-n80-k10-problem-sol6
	      a-n80-k10-problem-sol7 a-n80-k10-problem-sol8
	      a-n80-k10-problem-sol9 a-n80-k10-problem-sol10)))

(defparameter *neigh-names*
	 (list
	  (list (list "emno-results/cost-exploration/neigh-1-1-1" "emno-results/cost-exploration/neigh-1-1-2"
		      "emno-results/cost-exploration/neigh-1-1-3" "emno-results/cost-exploration/neigh-1-1-4"
		      "emno-results/cost-exploration/neigh-1-1-5" "emno-results/cost-exploration/neigh-1-1-6"
		      "emno-results/cost-exploration/neigh-1-1-7" "emno-results/cost-exploration/neigh-1-1-8"
		      "emno-results/cost-exploration/neigh-1-1-9" "emno-results/cost-exploration/neigh-1-1-10")
		(list "emno-results/cost-exploration/neigh-1-2-1" "emno-results/cost-exploration/neigh-1-2-2"
		      "emno-results/cost-exploration/neigh-1-2-3" "emno-results/cost-exploration/neigh-1-2-4"
		      "emno-results/cost-exploration/neigh-1-2-5" "emno-results/cost-exploration/neigh-1-2-6"
		      "emno-results/cost-exploration/neigh-1-2-7" "emno-results/cost-exploration/neigh-1-2-8"
		      "emno-results/cost-exploration/neigh-1-2-9" "emno-results/cost-exploration/neigh-1-2-10")
		(list "emno-results/cost-exploration/neigh-1-3-1" "emno-results/cost-exploration/neigh-1-3-2"
		      "emno-results/cost-exploration/neigh-1-3-3" "emno-results/cost-exploration/neigh-1-3-4"
		      "emno-results/cost-exploration/neigh-1-3-5" "emno-results/cost-exploration/neigh-1-3-6"
		      "emno-results/cost-exploration/neigh-1-3-7" "emno-results/cost-exploration/neigh-1-3-8"
		      "emno-results/cost-exploration/neigh-1-3-9" "emno-results/cost-exploration/neigh-1-3-10"))
	  (list (list "emno-results/cost-exploration/neigh-2-1-1" "emno-results/cost-exploration/neigh-2-1-2"
		      "emno-results/cost-exploration/neigh-2-1-3" "emno-results/cost-exploration/neigh-2-1-4"
		      "emno-results/cost-exploration/neigh-2-1-5" "emno-results/cost-exploration/neigh-2-1-6"
		      "emno-results/cost-exploration/neigh-2-1-7" "emno-results/cost-exploration/neigh-2-1-8"
		      "emno-results/cost-exploration/neigh-2-1-9" "emno-results/cost-exploration/neigh-2-1-10")
		(list "emno-results/cost-exploration/neigh-2-2-1" "emno-results/cost-exploration/neigh-2-2-2"
		      "emno-results/cost-exploration/neigh-2-2-3" "emno-results/cost-exploration/neigh-2-2-4"
		      "emno-results/cost-exploration/neigh-2-2-5" "emno-results/cost-exploration/neigh-2-2-6"
		      "emno-results/cost-exploration/neigh-2-2-7" "emno-results/cost-exploration/neigh-2-2-8"
		      "emno-results/cost-exploration/neigh-2-2-9" "emno-results/cost-exploration/neigh-2-2-10")
		(list "emno-results/cost-exploration/neigh-2-3-1" "emno-results/cost-exploration/neigh-2-3-2"
		      "emno-results/cost-exploration/neigh-2-3-3" "emno-results/cost-exploration/neigh-2-3-4"
		      "emno-results/cost-exploration/neigh-2-3-5" "emno-results/cost-exploration/neigh-2-3-6"
		      "emno-results/cost-exploration/neigh-2-3-7" "emno-results/cost-exploration/neigh-2-3-8"
		      "emno-results/cost-exploration/neigh-2-3-9" "emno-results/cost-exploration/neigh-2-3-10"))
	  (list (list "emno-results/cost-exploration/neigh-3-1-1" "emno-results/cost-exploration/neigh-3-1-2"
		      "emno-results/cost-exploration/neigh-3-1-3" "emno-results/cost-exploration/neigh-3-1-4"
		      "emno-results/cost-exploration/neigh-3-1-5" "emno-results/cost-exploration/neigh-3-1-6"
		      "emno-results/cost-exploration/neigh-3-1-7" "emno-results/cost-exploration/neigh-3-1-8"
		      "emno-results/cost-exploration/neigh-3-1-9" "emno-results/cost-exploration/neigh-3-1-10")
		(list "emno-results/cost-exploration/neigh-3-2-1" "emno-results/cost-exploration/neigh-3-2-2"
		      "emno-results/cost-exploration/neigh-3-2-3" "emno-results/cost-exploration/neigh-3-2-4"
		      "emno-results/cost-exploration/neigh-3-2-5" "emno-results/cost-exploration/neigh-3-2-6"
		      "emno-results/cost-exploration/neigh-3-2-7" "emno-results/cost-exploration/neigh-3-2-8"
		      "emno-results/cost-exploration/neigh-3-2-9" "emno-results/cost-exploration/neigh-3-2-10")
		(list "emno-results/cost-exploration/neigh-3-3-1" "emno-results/cost-exploration/neigh-3-3-2"
		      "emno-results/cost-exploration/neigh-3-3-3" "emno-results/cost-exploration/neigh-3-3-4"
		      "emno-results/cost-exploration/neigh-3-3-5" "emno-results/cost-exploration/neigh-3-3-6"
		      "emno-results/cost-exploration/neigh-3-3-7" "emno-results/cost-exploration/neigh-3-3-8"
		      "emno-results/cost-exploration/neigh-3-3-9" "emno-results/cost-exploration/neigh-3-3-10"))
	  (list (list "emno-results/cost-exploration/neigh-4-1-1" "emno-results/cost-exploration/neigh-4-1-2"
		      "emno-results/cost-exploration/neigh-4-1-3" "emno-results/cost-exploration/neigh-4-1-4"
		      "emno-results/cost-exploration/neigh-4-1-5" "emno-results/cost-exploration/neigh-4-1-6"
		      "emno-results/cost-exploration/neigh-4-1-7" "emno-results/cost-exploration/neigh-4-1-8"
		      "emno-results/cost-exploration/neigh-4-1-9" "emno-results/cost-exploration/neigh-4-1-10")
		(list "emno-results/cost-exploration/neigh-4-2-1" "emno-results/cost-exploration/neigh-4-2-2"
		      "emno-results/cost-exploration/neigh-4-2-3" "emno-results/cost-exploration/neigh-4-2-4"
		      "emno-results/cost-exploration/neigh-4-2-5" "emno-results/cost-exploration/neigh-4-2-6"
		      "emno-results/cost-exploration/neigh-4-2-7" "emno-results/cost-exploration/neigh-4-2-8"
		      "emno-results/cost-exploration/neigh-4-2-9" "emno-results/cost-exploration/neigh-4-2-10")
		(list "emno-results/cost-exploration/neigh-4-3-1" "emno-results/cost-exploration/neigh-4-3-2"
		      "emno-results/cost-exploration/neigh-4-3-3" "emno-results/cost-exploration/neigh-4-3-4"
		      "emno-results/cost-exploration/neigh-4-3-5" "emno-results/cost-exploration/neigh-4-3-6"
		      "emno-results/cost-exploration/neigh-4-3-7" "emno-results/cost-exploration/neigh-4-3-8"
		      "emno-results/cost-exploration/neigh-4-3-9" "emno-results/cost-exploration/neigh-4-3-10"))
	  (list (list "emno-results/cost-exploration/neigh-5-1-1" "emno-results/cost-exploration/neigh-5-1-2"
		      "emno-results/cost-exploration/neigh-5-1-3" "emno-results/cost-exploration/neigh-5-1-4"
		      "emno-results/cost-exploration/neigh-5-1-5" "emno-results/cost-exploration/neigh-5-1-6"
		      "emno-results/cost-exploration/neigh-5-1-7" "emno-results/cost-exploration/neigh-5-1-8"
		      "emno-results/cost-exploration/neigh-5-1-9" "emno-results/cost-exploration/neigh-5-1-10")
		(list "emno-results/cost-exploration/neigh-5-2-1" "emno-results/cost-exploration/neigh-5-2-2"
		      "emno-results/cost-exploration/neigh-5-2-3" "emno-results/cost-exploration/neigh-5-2-4"
		      "emno-results/cost-exploration/neigh-5-2-5" "emno-results/cost-exploration/neigh-5-2-6"
		      "emno-results/cost-exploration/neigh-5-2-7" "emno-results/cost-exploration/neigh-5-2-8"
		      "emno-results/cost-exploration/neigh-5-2-9" "emno-results/cost-exploration/neigh-5-2-10")
		(list "emno-results/cost-exploration/neigh-5-3-1" "emno-results/cost-exploration/neigh-5-3-2"
		      "emno-results/cost-exploration/neigh-5-3-3" "emno-results/cost-exploration/neigh-5-3-4"
		      "emno-results/cost-exploration/neigh-5-3-5" "emno-results/cost-exploration/neigh-5-3-6"
		      "emno-results/cost-exploration/neigh-5-3-7" "emno-results/cost-exploration/neigh-5-3-8"
		      "emno-results/cost-exploration/neigh-5-3-9" "emno-results/cost-exploration/neigh-5-3-10"))
	  (list (list "emno-results/cost-exploration/neigh-6-1-1" "emno-results/cost-exploration/neigh-6-1-2"
		      "emno-results/cost-exploration/neigh-6-1-3" "emno-results/cost-exploration/neigh-6-1-4"
		      "emno-results/cost-exploration/neigh-6-1-5" "emno-results/cost-exploration/neigh-6-1-6"
		      "emno-results/cost-exploration/neigh-6-1-7" "emno-results/cost-exploration/neigh-6-1-8"
		      "emno-results/cost-exploration/neigh-6-1-9" "emno-results/cost-exploration/neigh-6-1-10")
		(list "emno-results/cost-exploration/neigh-6-2-1" "emno-results/cost-exploration/neigh-6-2-2"
		      "emno-results/cost-exploration/neigh-6-2-3" "emno-results/cost-exploration/neigh-6-2-4"
		      "emno-results/cost-exploration/neigh-6-2-5" "emno-results/cost-exploration/neigh-6-2-6"
		      "emno-results/cost-exploration/neigh-6-2-7" "emno-results/cost-exploration/neigh-6-2-8"
		      "emno-results/cost-exploration/neigh-6-2-9" "emno-results/cost-exploration/neigh-6-2-10")
		(list "emno-results/cost-exploration/neigh-6-3-1" "emno-results/cost-exploration/neigh-6-3-2"
		      "emno-results/cost-exploration/neigh-6-3-3" "emno-results/cost-exploration/neigh-6-3-4"
		      "emno-results/cost-exploration/neigh-6-3-5" "emno-results/cost-exploration/neigh-6-3-6"
		      "emno-results/cost-exploration/neigh-6-3-7" "emno-results/cost-exploration/neigh-6-3-8"
		      "emno-results/cost-exploration/neigh-6-3-9" "emno-results/cost-exploration/neigh-6-3-10"))
	  (list (list "emno-results/cost-exploration/neigh-7-1-1" "emno-results/cost-exploration/neigh-7-1-2"
		      "emno-results/cost-exploration/neigh-7-1-3" "emno-results/cost-exploration/neigh-7-1-4"
		      "emno-results/cost-exploration/neigh-7-1-5" "emno-results/cost-exploration/neigh-7-1-6"
		      "emno-results/cost-exploration/neigh-7-1-7" "emno-results/cost-exploration/neigh-7-1-8"
		      "emno-results/cost-exploration/neigh-7-1-9" "emno-results/cost-exploration/neigh-7-1-10")
		(list "emno-results/cost-exploration/neigh-7-2-1" "emno-results/cost-exploration/neigh-7-2-2"
		      "emno-results/cost-exploration/neigh-7-2-3" "emno-results/cost-exploration/neigh-7-2-4"
		      "emno-results/cost-exploration/neigh-7-2-5" "emno-results/cost-exploration/neigh-7-2-6"
		      "emno-results/cost-exploration/neigh-7-2-7" "emno-results/cost-exploration/neigh-7-2-8"
		      "emno-results/cost-exploration/neigh-7-2-9" "emno-results/cost-exploration/neigh-7-2-10")
		(list "emno-results/cost-exploration/neigh-7-3-1" "emno-results/cost-exploration/neigh-7-3-2"
		      "emno-results/cost-exploration/neigh-7-3-3" "emno-results/cost-exploration/neigh-7-3-4"
		      "emno-results/cost-exploration/neigh-7-3-5" "emno-results/cost-exploration/neigh-7-3-6"
		      "emno-results/cost-exploration/neigh-7-3-7" "emno-results/cost-exploration/neigh-7-3-8"
		      "emno-results/cost-exploration/neigh-7-3-9" "emno-results/cost-exploration/neigh-7-3-10"))
	  (list (list "emno-results/cost-exploration/neigh-8-1-1" "emno-results/cost-exploration/neigh-8-1-2"
		      "emno-results/cost-exploration/neigh-8-1-3" "emno-results/cost-exploration/neigh-8-1-4"
		      "emno-results/cost-exploration/neigh-8-1-5" "emno-results/cost-exploration/neigh-8-1-6"
		      "emno-results/cost-exploration/neigh-8-1-7" "emno-results/cost-exploration/neigh-8-1-8"
		      "emno-results/cost-exploration/neigh-8-1-9" "emno-results/cost-exploration/neigh-8-1-10")
		(list "emno-results/cost-exploration/neigh-8-2-1" "emno-results/cost-exploration/neigh-8-2-2"
		      "emno-results/cost-exploration/neigh-8-2-3" "emno-results/cost-exploration/neigh-8-2-4"
		      "emno-results/cost-exploration/neigh-8-2-5" "emno-results/cost-exploration/neigh-8-2-6"
		      "emno-results/cost-exploration/neigh-8-2-7" "emno-results/cost-exploration/neigh-8-2-8"
		      "emno-results/cost-exploration/neigh-8-2-9" "emno-results/cost-exploration/neigh-8-2-10")
		(list "emno-results/cost-exploration/neigh-8-3-1" "emno-results/cost-exploration/neigh-8-3-2"
		      "emno-results/cost-exploration/neigh-8-3-3" "emno-results/cost-exploration/neigh-8-3-4"
		      "emno-results/cost-exploration/neigh-8-3-5" "emno-results/cost-exploration/neigh-8-3-6"
		      "emno-results/cost-exploration/neigh-8-3-7" "emno-results/cost-exploration/neigh-8-3-8"
		      "emno-results/cost-exploration/neigh-8-3-9" "emno-results/cost-exploration/neigh-8-3-10"))
	  (list (list "emno-results/cost-exploration/neigh-9-1-1" "emno-results/cost-exploration/neigh-9-1-2"
		      "emno-results/cost-exploration/neigh-9-1-3" "emno-results/cost-exploration/neigh-9-1-4"
		      "emno-results/cost-exploration/neigh-9-1-5" "emno-results/cost-exploration/neigh-9-1-6"
		      "emno-results/cost-exploration/neigh-9-1-7" "emno-results/cost-exploration/neigh-9-1-8"
		      "emno-results/cost-exploration/neigh-9-1-9" "emno-results/cost-exploration/neigh-9-1-10")
		(list "emno-results/cost-exploration/neigh-9-2-1" "emno-results/cost-exploration/neigh-9-2-2"
		      "emno-results/cost-exploration/neigh-9-2-3" "emno-results/cost-exploration/neigh-9-2-4"
		      "emno-results/cost-exploration/neigh-9-2-5" "emno-results/cost-exploration/neigh-9-2-6"
		      "emno-results/cost-exploration/neigh-9-2-7" "emno-results/cost-exploration/neigh-9-2-8"
		      "emno-results/cost-exploration/neigh-9-2-9" "emno-results/cost-exploration/neigh-9-2-10")
		(list "emno-results/cost-exploration/neigh-9-3-1" "emno-results/cost-exploration/neigh-9-3-2"
		      "emno-results/cost-exploration/neigh-9-3-3" "emno-results/cost-exploration/neigh-9-3-4"
		      "emno-results/cost-exploration/neigh-9-3-5" "emno-results/cost-exploration/neigh-9-3-6"
		      "emno-results/cost-exploration/neigh-9-3-7" "emno-results/cost-exploration/neigh-9-3-8"
		      "emno-results/cost-exploration/neigh-9-3-9" "emno-results/cost-exploration/neigh-9-3-10"))
	  (list (list "emno-results/cost-exploration/neigh-10-1-1" "emno-results/cost-exploration/neigh-10-1-2"
		      "emno-results/cost-exploration/neigh-10-1-3" "emno-results/cost-exploration/neigh-10-1-4"
		      "emno-results/cost-exploration/neigh-10-1-5" "emno-results/cost-exploration/neigh-10-1-6"
		      "emno-results/cost-exploration/neigh-10-1-7" "emno-results/cost-exploration/neigh-10-1-8"
		      "emno-results/cost-exploration/neigh-10-1-9" "emno-results/cost-exploration/neigh-10-1-10")
		(list "emno-results/cost-exploration/neigh-10-2-1" "emno-results/cost-exploration/neigh-10-2-2"
		      "emno-results/cost-exploration/neigh-10-2-3" "emno-results/cost-exploration/neigh-10-2-4"
		      "emno-results/cost-exploration/neigh-10-2-5" "emno-results/cost-exploration/neigh-10-2-6"
		      "emno-results/cost-exploration/neigh-10-2-7" "emno-results/cost-exploration/neigh-10-2-8"
		      "emno-results/cost-exploration/neigh-10-2-9" "emno-results/cost-exploration/neigh-10-2-10")
		(list "emno-results/cost-exploration/neigh-10-3-1" "emno-results/cost-exploration/neigh-10-3-2"
		      "emno-results/cost-exploration/neigh-10-3-3" "emno-results/cost-exploration/neigh-10-3-4"
		      "emno-results/cost-exploration/neigh-10-3-5" "emno-results/cost-exploration/neigh-10-3-6"
		      "emno-results/cost-exploration/neigh-10-3-7" "emno-results/cost-exploration/neigh-10-3-8"
		      "emno-results/cost-exploration/neigh-10-3-9" "emno-results/cost-exploration/neigh-10-3-10"))))


(defun neigh-costs-for-neighborhood (c p s)
  (let* ((path (make-pathname :name (nth (1- s) (nth (1- p) (nth (1- c) *neigh-names*)))
			      :type "txt"))
	 (str (open path :direction :output
		    :if-exists :supersede))
	 (code (nth (1- c) *emno-neighborhood-criteria*))
	 (solution (nth (1- s) (nth (1- p) *emno-problem-solutions*)))
	 (problem (nth (1- p) *emno-problems*))
	 (neigh (build-neighborhood-tree code solution))
	 ;; here we crete the exahustive generator
	 (sol-generator (exhaustive-exploration neigh))
	 ;; the current neighbor-solution
	 (current-sol (funcall sol-generator))
	 ;; :vrp system stuff
	 (work-cop (basic-working-copy solution))
	 (current-delta-cost 0)
	 (action (delta-cvrp-action)))

    (format t "Neighborhood: ~A ~A ~A~%" c p s)

    (prepare-solution-for-neighborhood-exploration work-cop)
    ;; this is the line added to be able
    ;; to compute the delta-cost.
    (initialize-action-for-delta-cost-computation work-cop problem action)

    (loop while (not (null current-sol))
       do
	 (setf current-delta-cost (delta-cost (from-coordinates-to-operations current-sol)
					      work-cop ;; the working-copy
					      problem ;; the problem
					      action ;; the action
					      ))
	 (format str "~A~%" current-delta-cost)
	 (setf current-sol (funcall sol-generator)))

  (close str)))

(defun neigh-combinations (c-list p-list s-list)
  (loop for k in c-list
     doing
       (loop for i in p-list
	  doing
	    (loop for j in s-list
	       doing
		 (neigh-costs-for-neighborhood k i j)))))

(defparameter *data-problems* (list a-n32-k5-problem a-n65-k9-problem a-n80-k10-problem))

(defparameter *data-neighborhood-criteria*
  (list *code-rarebf* *code-rarebhf* *code-ref* *code-rerf* *code-rereg*
	*code-rehf* *code-rehrf* *code-rehreg* *code-rerehg* *code-rehrehg*))

(defparameter a-n32-k5-problem-c1-s1
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 18 14 13 12 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c1-s2
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 20 18 11 22 10 9 8 14 13 12)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c1-s3
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 20 18 11 22 10 9 8 14 13 12)
				 (29 21 19 17 31 28 16)
				 (27 25 15 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c1-s4
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 20 18 11 22 9 8 23 14 13 12)
				 (29 21 19 17 31 28 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c1-s5
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 20 18 11 22 9 8 28 23 14 12)
				 (29 13 21 19 17 31 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c1-s6
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (20 29 18 11 22 9 8 28 23 14 12)
				 (26 13 21 19 17 31 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c1-s7
  (with-basic-cvrp-solution (s1 ((7 6 8 4 3 2 1)
				 (20 5 29 18 11 22 9 28 23 14 12)
				 (26 13 21 19 17 31 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c1-s8
  (with-basic-cvrp-solution (s1 ((6 8 18 11 4 3 2 1)
				 (20 5 29 22 9 28 23 14 7 12)
				 (26 13 21 19 17 31 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c1-s9
  (with-basic-cvrp-solution (s1 ((14 6 8 11 4 3 2 1)
				 (20 5 29 22 9 18 28 23 7 12)
				 (26 13 21 19 17 31 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c1-s10
  (with-basic-cvrp-solution (s1 ((14 8 11 4 2 3 6 1)
				 (20 5 29 22 9 18 28 23 7 12)
				 (26 13 21 19 17 31 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s1
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 18 14 13 12 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s2
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 18 14 13 12 11 10 9 8)
				 (29 28 31 21 19 17 16)
				 (27 25 15 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s3
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 18 22 11 10 9 8 20)
				 (29 28 31 21 19 17 16)
				 (27 25 15 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s4
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 23 18 22 11 9 8 20)
				 (29 28 31 21 19 17 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s5
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 14 23 18 22 11 9 8 28 29 20)
				 (31 21 19 17 13 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s6
  (with-basic-cvrp-solution (s1 ((5 4 2 3 6 7 1)
				 (26 12 14 23 18 22 11 9 8 28 29 20)
				 (31 21 19 17 13 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s7
  (with-basic-cvrp-solution (s1 ((5 4 2 3 6 7 1)
				 (26 12 14 23 28 8 11 18 9 22 29 20)
				 (31 21 19 17 13 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s8
  (with-basic-cvrp-solution (s1 ((5 4 3 6 7 1 12)
				 (26 14 2 23 28 8 11 18 9 22 29 20)
				 (31 21 19 17 13 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s9
  (with-basic-cvrp-solution (s1 ((5 8 4 6 7 1 12)
				 (26 14 3 2 23 28 11 18 9 22 29 20)
				 (31 21 19 17 13 16)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c2-s10
  (with-basic-cvrp-solution (s1 ((5 8 4 6 7 1 12)
				 (26 14 3 2 23 28 11 18 9 22 29 20)
				 (16 21 31 19 17 13)
				 (27 25 10 15 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s1
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 18 8 11 9 22 10 20 14 13 12)
				 (29 15 19 17 21 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s2
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (14 13 12 26 18 8 11 9 22 10 20)
				 (29 15 19 17 21 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s3
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (13 12 26 14 18 8 11 9 22 10 20)
				 (29 15 19 17 21 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s4
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 11 10 9 8 22 20 18 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s5
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 18 11 10 9 8 22 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s6
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 18 11 9 8 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s7
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 18 11 9 8 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s8
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 18 8 11 9 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s9
  (with-basic-cvrp-solution (s1 ((5 4 3 2 7 6 1)
				 (26 18 8 11 9 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c3-s10
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 18 8 11 9 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s1
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 18 14 13 12 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s2
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 11 10 9 8 22 20 18 14 13 12)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s3
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 11 10 9 8 22 20 18 14 13 12)
				 (29 15 21 19 17 16)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s4
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 11 10 9 8 22 20 18 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s5
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 18 11 10 9 8 22 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s6
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 18 11 9 8 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s7
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 18 11 9 8 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s8
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 18 8 11 9 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s9
  (with-basic-cvrp-solution (s1 ((5 4 3 2 7 6 1)
				 (26 18 8 11 9 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c4-s10
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 18 8 11 9 22 10 20 14 13 12)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s1
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 18 14 13 12 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s2
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 10 9 8 11 18 14 13 12)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s3
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 10 9 8 11 18 14 13 12)
				 (29 21 19 17 16 24)
				 (31 28 27 25 15 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s4
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 27 9 8 11 18 14 13 12)
				 (29 21 19 17 16 24)
				 (31 28 22 20 10 25 15 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s5
  (with-basic-cvrp-solution (s1 ((7 28 22 4 3 2 1)
				 (26 27 9 8 11 18 14 13 12)
				 (29 21 19 17 16 24)
				 (31 6 5 20 10 25 15 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s6
  (with-basic-cvrp-solution (s1 ((7 28 22 4 3 2 1)
				 (26 27 9 8 11 18 14 13 12)
				 (29 21 19 17 16 24)
				 (31 6 23 10 25 15 5 20)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s7
  (with-basic-cvrp-solution (s1 ((7 28 22 4 3 2 1)
				 (26 27 21 19 17 12)
				 (29 9 8 11 18 14 13 16 24)
				 (31 6 23 10 25 15 5 20)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s8
  (with-basic-cvrp-solution (s1 ((7 28 22 4 3 2 1)
				 (26 27 21 19 17 12)
				 (29 9 8 11 18 14 13 16 24)
				 (31 6 23 15 10 25 5 20)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s9
  (with-basic-cvrp-solution (s1 ((7 28 22 4 3 2 1)
				 (26 27 21 19 17 12)
				 (16 13 29 9 8 11 18 14 24)
				 (31 6 23 15 10 25 5 20)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c5-s10
  (with-basic-cvrp-solution (s1 ((7 28 22 4 3 2 1)
				 (12 21 19 17 26 27)
				 (16 13 29 9 8 11 18 14 24)
				 (31 6 23 15 10 25 5 20)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s1
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (12 13 26 14 18 8 11 9 22 10 20)
				 (29 15 17 19 21 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s2
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 10 9 8 11 12 13 14 18 20)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s3
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 18 11 8 9 10 22 20)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s4
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 18 11 8 9 10 22 20)
				 (29 15 21 19 17 16)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s5
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 18 11 8 9 10 22 20)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s6
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 14 18 11 8 9 10 22 20)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s7
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 14 18 11 8 9 22 10 20)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s8
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 14 18 11 8 9 22 10 20)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s9
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 14 18 11 8 9 22 10 20)
				 (29 15 17 19 21 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c6-s10
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 14 18 8 11 9 22 10 20)
				 (29 15 17 19 21 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s1
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 18 14 13 12 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s2
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 10 9 8 11 12 13 14 18 20)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s3
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 18 11 8 9 10 22 20)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s4
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 18 11 8 9 10 22 20)
				 (29 15 21 19 17 16)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s5
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 18 11 8 9 10 22 20)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s6
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 14 18 11 8 9 10 22 20)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s7
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 14 18 11 8 9 22 10 20)
				 (29 15 21 19 17 16)
				 (31 23 28 27 25 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s8
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 14 18 11 8 9 22 10 20)
				 (29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s9
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 18 11 8 9 22 10 20)
				 (14 29 15 21 19 17 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c7-s10
  (with-basic-cvrp-solution (s1 ((5 4 3 2 6 7 1)
				 (26 12 13 18 11 8 9 22 10 20)
				 (14 29 15 17 19 21 16)
				 (31 23 28 25 27 24)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s1
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 18 14 13 12 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s2
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 20 18 22 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s3
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 20 18 22 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 7 6 5 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s4
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 20 18 22 11 10 9 8)
				 (29 21 19 17 16 24)
				 (31 7 6 5 25 15 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s5
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 20 18 22 11 10 9 8)
				 (29 15 25 5 16 24)
				 (31 7 6 21 19 17 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s6
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 20 10 9 22 11 18 8)
				 (29 15 25 5 16 24)
				 (31 7 6 21 19 17 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s7
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 20 10 9 22 11 18 8)
				 (29 15 25 5 16 24)
				 (31 21 19 17 23 6 7)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s8
  (with-basic-cvrp-solution (s1 ((16 28 4 3 2 1)
				 (26 12 13 14 20 10 9 22 11 18 8)
				 (29 15 25 5 27 24)
				 (31 21 19 17 23 6 7)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s9
  (with-basic-cvrp-solution (s1 ((16 28 4 3 2 23 6)
				 (26 12 13 14 20 10 9 22 11 18 8)
				 (29 15 25 5 27 24)
				 (31 21 19 17 1 7)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c8-s10
  (with-basic-cvrp-solution (s1 ((16 28 4 3 2 23 6)
				 (26 12 13 14 18 8 11 22 9 10 20)
				 (29 15 25 5 27 24)
				 (31 21 19 17 1 7)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s1
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 18 14 13 12 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s2
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 20 18 22 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s3
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 20 18 22 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 7 6 5 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s4
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 20 18 22 11 10 9 8)
				 (29 21 19 17 16 24)
				 (31 7 6 5 25 15 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s5
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 18 22 11 8 9 10 20)
				 (29 21 19 17 16 24)
				 (31 7 6 5 25 15 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s6
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 18 22 11 8 9 10 20)
				 (29 15 25 5 16 24)
				 (31 7 6 21 19 17 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s7
  (with-basic-cvrp-solution (s1 ((27 28 4 3 2 1)
				 (26 12 13 14 18 22 11 8 9 10 20)
				 (29 15 25 5 16 24)
				 (7 6 23 17 19 21 31)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s8
  (with-basic-cvrp-solution (s1 ((16 28 4 3 2 1)
				 (26 12 13 14 18 22 11 8 9 10 20)
				 (29 15 25 5 27 24)
				 (7 6 23 17 19 21 31)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s9
  (with-basic-cvrp-solution (s1 ((16 28 4 3 2 23 6)
				 (26 12 13 14 18 22 11 8 9 10 20)
				 (29 15 25 5 27 24)
				 (7 1 17 19 21 31)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c9-s10
  (with-basic-cvrp-solution (s1 ((16 28 4 3 2 23 6)
				 (26 12 13 14 18 22 11 8 9 10 20)
				 (29 15 25 5 27 24)
				 (7 17 19 31 21 1)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s1
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 22 20 18 14 13 12 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s2
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 20 18 22 11 10 9 8)
				 (29 21 19 17 16 15)
				 (31 28 27 25 24 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s3
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 14 20 18 22 11 10 9 8)
				 (29 21 19 17 16 24)
				 (31 28 27 25 15 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s4
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 11 22 18 8 9 10 20 14)
				 (29 21 19 17 16 24)
				 (31 28 27 25 15 23)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s5
  (with-basic-cvrp-solution (s1 ((7 6 5 4 3 2 1)
				 (26 12 13 11 22 18 8 9 10 20 14)
				 (29 21 19 17 16 24)
				 (31 28 23 15 25 27)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s6
  (with-basic-cvrp-solution (s1 ((7 6 5 25 15 1)
				 (26 12 13 11 22 18 8 9 10 20 14)
				 (29 21 19 17 16 24)
				 (31 28 23 2 3 4 27)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s7
  (with-basic-cvrp-solution (s1 ((7 6 5 25 15 1)
				 (27 4 11 22 18 8 9 10 20 14)
				 (29 21 19 17 16 24)
				 (31 28 23 2 3 13 12 26)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s8
  (with-basic-cvrp-solution (s1 ((7 6 5 25 15 1)
				 (27 20 10 9 22 18 8 11 4 14)
				 (29 21 19 17 16 24)
				 (31 28 23 2 3 13 12 26)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s9
  (with-basic-cvrp-solution (s1 ((7 6 5 25 15 1)
				 (27 20 10 9 22 18 8 11 4 29)
				 (14 21 19 17 16 24)
				 (31 28 23 2 3 13 12 26)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n32-k5-problem-c10-s10
  (with-basic-cvrp-solution (s1 ((25 5 15 6 7 1)
				 (27 20 10 9 22 18 8 11 4 29)
				 (14 21 19 17 16 24)
				 (31 28 23 2 3 13 12 26)
				 (30))
				a-n32-k5-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 9 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 32 26 22 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 21 44 20 16 15)
				 (57 35 28 27 25 24 23)
				 (38 32 26 22 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (59 52 19 17 51 50 49 47 46)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 21 44 20 16 15)
				 (27 35 25 24 23 57 28)
				 (38 32 26 22 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (59 52 19 17 51 50 49 47 46)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s6
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 21 44 20 16 15)
				 (27 35 25 24 23 57 28)
				 (38 32 26 22 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (17 51 50 49 47 46 54)
				 (56 59 52 19 55 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s7
  (with-basic-cvrp-solution (s1 ((6 5 4 3 50 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 21 44 20 16 15)
				 (27 35 25 24 23 57 28)
				 (38 32 26 22 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (17 51 49 47 46 54 11 7)
				 (56 59 52 19 55 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s8
  (with-basic-cvrp-solution (s1 ((6 4 3 50 2 38 32 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 21 44 20 16 15)
				 (27 35 25 24 23 57 28)
				 (26 22 34 31 30 5 29)
				 (61 42 41 40 39 9 37 36)
				 (17 51 49 47 46 54 11 7)
				 (56 59 52 19 55 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s9
  (with-basic-cvrp-solution (s1 ((6 4 3 50 2 38 32 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 21 44 20 16 15)
				 (27 35 25 24 23 57 28)
				 (26 22 34 31 30 5 29)
				 (39 9 37 36 41 61 42 40)
				 (17 51 49 47 46 54 11 7)
				 (56 59 52 19 55 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c1-s10
  (with-basic-cvrp-solution (s1 ((6 4 3 50 2 38 32 1)
				 (18 43 48 33 13 12 10 8)
				 (45 21 44 20 16 14 15)
				 (27 35 25 24 23 57 28)
				 (26 22 34 31 30 5 29)
				 (39 9 37 36 41 61 42 40)
				 (17 51 49 47 46 54 11 7)
				 (56 59 52 19 55 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 9 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 32 26 22 21 20 19 17 15)
				 (57 35 28 27 25 24 23)
				 (38 16 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 32 26 22 21 20 19 17 15)
				 (57 35 28 27 25 24 23)
				 (38 16 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (53 59 52 51 50 49 47 46)
				 (44 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 32 26 22 21 20 19 17 15)
				 (28 57 23 24 25 35 27)
				 (38 16 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (53 59 52 51 50 49 47 46)
				 (44 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s6
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 32 21 20 19 51 17 15)
				 (28 57 23 24 25 35 27)
				 (38 16 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (53 59 52 50 49 22 26 47 46)
				 (44 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s7
  (with-basic-cvrp-solution (s1 ((11 7 5 4 3 16 38 2 1)
				 (18 43 14 13 12 10 8)
				 (48 33 45 32 21 20 19 51 17 15)
				 (28 57 23 24 25 35 27)
				 (6 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (53 59 52 50 49 22 26 47 46)
				 (44 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s8
  (with-basic-cvrp-solution (s1 ((11 7 5 4 3 16 38 2 1)
				 (43 14 13 12 10 8 32 45)
				 (48 33 18 21 20 19 51 17 15)
				 (28 57 23 24 25 35 27)
				 (6 34 31 30 29)
				 (61 42 41 40 39 9 37 36)
				 (53 59 52 50 49 22 26 47 46)
				 (44 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s9
  (with-basic-cvrp-solution (s1 ((11 7 5 4 3 16 38 2 1)
				 (43 14 13 12 10 8 32 45)
				 (48 33 18 21 20 19 51 17 15)
				 (28 57 23 24 25 35 27)
				 (6 34 31 30 29)
				 (40 61 42 41 37 36 9 39)
				 (53 59 52 50 49 22 26 47 46)
				 (44 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c2-s10
  (with-basic-cvrp-solution (s1 ((11 7 5 4 3 16 38 2 1)
				 (43 14 13 12 10 8 32 45)
				 (63 48 33 18 21 19 51 17 15)
				 (28 57 23 24 25 35 27)
				 (6 34 31 30 29)
				 (40 61 42 41 37 36 9 39)
				 (53 59 52 50 49 22 26 47 46)
				 (44 56 55 54)
				 (64 62 60 20 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 32 21 20 19 17 26 22 16 15)
				 (27 35 25 24 23 57 28)
				 (31 34 30 38 33 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (54 53 56 55)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 32 21 20 19 17 26 22 16 15)
				 (27 35 25 24 23 57 28)
				 (31 34 30 38 33 29)
				 (43 37 36 61 42 41 40 39)
				 (44 59 52 51 50 49 47 46)
				 (54 53 56 55)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 16 32 21 20 19 17 26 22 15)
				 (27 35 25 24 23 57 28)
				 (31 34 30 38 33 29)
				 (43 37 36 61 42 41 40 39)
				 (44 59 52 51 50 49 47 46)
				 (54 53 56 55)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 16 32 21 20 19 17 26 22 15)
				 (27 35 25 24 23 57 28)
				 (31 34 30 38 33 29)
				 (43 37 36 61 42 41 40 39)
				 (44 59 52 50 49 47 46 51)
				 (54 53 56 55)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 16 20 32 21 19 17 26 22 15)
				 (27 35 25 24 23 57 28)
				 (31 34 30 38 33 29)
				 (43 37 36 61 42 41 40 39)
				 (44 59 52 50 49 47 46 51)
				 (54 53 56 55)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s6
  (with-basic-cvrp-solution (s1 ((5 4 3 2 1 11 7 6)
				 (18 13 12 10 8 14 9)
				 (48 45 16 20 32 21 19 17 26 22 15)
				 (27 35 25 24 23 57 28)
				 (31 34 30 38 33 29)
				 (43 37 36 61 42 41 40 39)
				 (44 59 52 50 49 47 46 51)
				 (54 53 56 55)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s7
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 13 12 10 8)
				 (48 45 32 21 20 19 17 26 22 16 15)
				 (57 28 27 35 25 24 23)
				 (31 30 38 34 33 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s8
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 32 21 20 19 17 26 22 16 15)
				 (57 28 27 35 25 24 23)
				 (31 30 38 34 33 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s9
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 32 21 20 19 17 26 22 16 15)
				 (57 28 27 35 25 24 23)
				 (31 34 30 38 33 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c3-s10
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 32 21 20 19 17 26 22 16 15)
				 (57 28 27 35 25 24 23)
				 (31 34 30 38 33 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (54 53 56 55)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 9 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 13 12 10 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 13 12 10 8)
				 (26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 48 45 32 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 13 12 10 8)
				 (26 22 21 20 19 17 16 35 15)
				 (57 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 48 45 32 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 13 12 10 8)
				 (26 22 21 20 19 17 16 35 15)
				 (57 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (44 59 52 51 50 49 47 46)
				 (56 55 54 48 45 32 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s6
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 13 12 10 8)
				 (26 22 21 20 19 17 16 35 15)
				 (57 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (56 55 54 48 45 32 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s7
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 13 12 10 8)
				 (26 22 21 20 19 17 16 35 15)
				 (57 28 27 25 24 23)
				 (31 30 38 34 33 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (56 55 54 48 45 32 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s8
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 13 12 10 8)
				 (26 22 21 20 19 17 16 35 15)
				 (57 28 27 25 24 23)
				 (31 30 38 34 33 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (45 32 53 56 55 54 48)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s9
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (26 22 21 20 19 17 16 35 15)
				 (57 28 27 25 24 23)
				 (31 30 38 34 33 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (45 32 53 56 55 54 48)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c4-s10
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (26 22 21 20 19 17 16 35 15)
				 (57 28 27 25 24 23)
				 (31 34 30 38 33 29)
				 (61 42 41 40 39 43 37 36)
				 (44 59 52 51 50 49 47 46)
				 (45 32 53 56 55 54 48)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 9 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 14 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s6
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 59 52 16 15)
				 (57 35 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s7
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 28 21 20 19 59 52 16 15)
				 (57 35 45 32 26 22 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s8
  (with-basic-cvrp-solution (s1 ((11 7 48 28 21 20 19 59 52 1)
				 (18 33 13 12 10 44 8)
				 (6 5 4 3 2 16 15)
				 (57 35 45 32 26 22 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s9
  (with-basic-cvrp-solution (s1 ((11 7 48 28 21 20 19 59 52 1)
				 (18 33 13 12 10 44 8)
				 (6 5 4 3 2 16 15)
				 (57 35 45 32 56 55 54 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (26 22 27 25 24 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c5-s10
  (with-basic-cvrp-solution (s1 ((11 7 48 28 21 20 19 59 52 1)
				 (18 33 13 12 10 44 8)
				 (6 5 4 3 2 16 15)
				 (57 35 45 32 56 55 54 23)
				 (39 34 14 31 26 22 27)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (30 29 25 24 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 17 26 22 21 19 20 32 45 16 15)
				 (27 35 25 24 23 57 28)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 21 19 20 32 45 16 15 22 26 17)
				 (27 35 25 24 23 57 28)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 21 19 20 32 45 16 15 22 26 17)
				 (27 35 25 24 23 57 28)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 37 36 43)
				 (50 49 47 46 51 52 59 44)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 21 19 20 32 45 16 15 22 26 17)
				 (27 35 25 24 23 57 28)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 37 36 43)
				 (44 59 52 50 49 47 46 51)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 21 19 20 32 45 16 15 22 26 17)
				 (27 35 25 24 23 57 28)
				 (38 30 31 34 33 29)
				 (39 40 61 42 41 37 36 43)
				 (44 59 52 50 49 47 46 51)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s6
  (with-basic-cvrp-solution (s1 ((4 3 2 5 6 7 11 1)
				 (18 13 12 10 8 14 9)
				 (48 21 19 20 32 45 16 15 22 26 17)
				 (27 35 25 24 23 57 28)
				 (38 30 31 34 33 29)
				 (39 40 61 42 41 37 36 43)
				 (44 59 52 50 49 47 46 51)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s7
  (with-basic-cvrp-solution (s1 ((4 3 2 5 6 7 11 1)
				 (18 13 12 10 8 14 9)
				 (48 21 19 20 32 45 16 15 22 26 17)
				 (27 35 25 24 23 57 28)
				 (31 34 30 38 33 29)
				 (39 40 61 42 41 37 36 43)
				 (44 59 52 50 49 47 46 51)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s8
  (with-basic-cvrp-solution (s1 ((4 3 2 5 1 11 7 6)
				 (18 13 12 10 8 14 9)
				 (48 21 19 20 32 45 16 15 22 26 17)
				 (27 35 25 24 23 57 28)
				 (31 34 30 38 33 29)
				 (39 40 61 42 41 37 36 43)
				 (44 59 52 50 49 47 46 51)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s9
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 32 20 19 21 22 26 17 16 15)
				 (57 23 24 25 27 35 28)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c6-s10
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 32 20 19 21 22 26 17 16 15)
				 (27 35 25 24 23 57 28)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (53 56 55 54)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 9 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 8 10 12 13)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 8 10 12 13)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 30 31 34 33 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 8 10 12 13)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 30 31 34 33 29)
				 (61 43 42 41 40 39 37 36)
				 (50 49 47 46 51 52 59 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 8 10 12 13)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s6
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 8 10 12 13)
				 (48 45 32 20 19 21 22 26 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s7
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 9 8 10 12 13)
				 (48 45 32 20 19 21 22 26 17 16 15)
				 (57 28 27 25 24 23)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (56 55 54 53)
				 (64 63 62 35 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s8
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 32 20 19 21 22 26 17 16 15)
				 (57 28 27 25 24 23)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (56 55 54 53)
				 (64 63 62 35 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s9
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (48 45 32 20 19 21 22 26 17 16 15)
				 (57 28 27 25 24 23)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (53 56 55 54)
				 (64 63 62 35 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c7-s10
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 14 9)
				 (45 32 20 19 21 22 26 17 16 15)
				 (48 57 28 27 25 24 23)
				 (38 30 31 34 33 29)
				 (39 40 41 42 61 43 37 36)
				 (50 49 47 46 51 52 59 44)
				 (53 56 55 54)
				 (64 63 62 35 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 9 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 14 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 52 59 16 15)
				 (57 35 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s6
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 28 21 20 19 52 59 16 15)
				 (57 35 45 32 26 22 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s7
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 28 21 20 19 52 59 16 15)
				 (57 35 45 32 26 22 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s8
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 28 21 20 19 52 59 16 15)
				 (57 35 45 32 53 25 24 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 26 22 27)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s9
  (with-basic-cvrp-solution (s1 ((11 7 59 52 19 20 21 28 48 1)
				 (18 33 13 12 10 44 8)
				 (6 5 4 3 2 16 15)
				 (57 35 45 32 53 25 24 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 26 22 27)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c8-s10
  (with-basic-cvrp-solution (s1 ((11 7 59 52 19 20 21 28 48 1)
				 (18 33 13 12 10 44 8)
				 (6 5 4 3 2 16 15)
				 (57 35 45 32 53 25 24 23)
				 (39 34 47 49 50 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 14 31 46 9)
				 (56 55 54 26 22 27)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 9 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 9 14)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 9 14)
				 (48 45 32 44 46 47 49 50 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 26 22 21 20 19 17)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 19 20 21)
				 (48 45 32 44 46 47 49 50 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 26 22 9 14 17)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 19 20 21)
				 (48 45 32 44 46 47 49 50 16 15)
				 (57 35 28 27 25 24 23)
				 (39 34 33 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (59 52 51 26 22 9 14 17)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s6
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 19 20 21)
				 (48 45 32 44 46 47 49 50 16 15)
				 (57 35 28 33 25 24 23)
				 (39 34 27 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (59 52 51 26 22 9 14 17)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s7
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 19 20 21)
				 (48 54 46 47 49 50 16 15)
				 (57 35 28 33 25 24 23)
				 (39 34 27 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (59 52 51 26 22 9 14 17)
				 (56 55 45 32 44 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s8
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 13 12 10 8 19 20 21)
				 (48 54 46 47 49 50 16 15)
				 (57 35 28 33 25 24 23)
				 (39 34 27 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (59 52 51 26 22 9 14 17)
				 (56 55 45 32 44 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s9
  (with-basic-cvrp-solution (s1 ((28 33 25 24 23 1)
				 (18 13 12 10 8 19 20 21)
				 (48 54 46 47 49 50 16 15)
				 (57 35 2 3 4 5 6 7 11)
				 (39 34 27 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (59 52 51 26 22 9 14 17)
				 (56 55 45 32 44 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c9-s10
  (with-basic-cvrp-solution (s1 ((28 33 25 24 23 1)
				 (18 13 12 10 8 19 20 21)
				 (5 4 3 2 35 50 16 15)
				 (57 48 54 46 47 49 6 7 11)
				 (39 34 27 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (59 52 51 26 22 9 14 17)
				 (56 55 45 32 44 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s1
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 9 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 44)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s2
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 14 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 33 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s3
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (38 34 14 31 30 29)
				 (61 43 42 41 40 39 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s4
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 17 16 15)
				 (57 35 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (59 52 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s5
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (48 45 32 26 22 21 20 19 52 59 16 15)
				 (57 35 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s6
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (35 45 32 26 22 21 20 19 52 59 16 15)
				 (57 48 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 43 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 58))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s7
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (35 45 32 26 22 21 20 19 52 59 16 15)
				 (57 48 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s8
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (35 45 32 20 16 59 52 19 21 22 26 15)
				 (57 48 28 27 25 24 23)
				 (39 34 14 31 30 29)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s9
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (35 45 32 20 16 59 52 19 21 22 26 15)
				 (57 48 28 29 25 24 23)
				 (39 34 14 31 30 27)
				 (61 58 42 41 40 38 37 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n65-k9-problem-c10-s10
  (with-basic-cvrp-solution (s1 ((11 7 6 5 4 3 2 1)
				 (18 33 13 12 10 44 8)
				 (35 37 38 40 59 52 19 21 22 26 15)
				 (57 48 28 29 25 24 23)
				 (39 34 14 31 30 27)
				 (61 58 42 41 16 20 32 45 36)
				 (17 51 50 49 47 46 9)
				 (56 55 54 53)
				 (64 63 62 60 43))
				a-n65-k9-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s1
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s2
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 37 36 35 9 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s3
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 21 20 41 40)
				 (47 44 43 37 36 35 9 32 30 28)
				 (48 22 45 42 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s4
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 21 20 41 40)
				 (47 35 9 32 30 79 28)
				 (48 22 45 42 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (76 75 44 43 37 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s5
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 21 20 41 40)
				 (47 35 9 32 30 79 28)
				 (48 22 45 42 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (76 75 44 43 37 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s6
  (with-basic-cvrp-solution (s1 ((31 46 15 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 21 20 41 40)
				 (47 35 9 32 30 79 28)
				 (48 22 45 42 39)
				 (14 8 57 56 55 54 53 52 51 50 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (76 75 44 43 37 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s7
  (with-basic-cvrp-solution (s1 ((31 46 15 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 52 17 13 12)
				 (34 29 27 25 23 21 20 41 40)
				 (47 35 9 32 30 79 28)
				 (48 22 45 42 39)
				 (14 8 16 57 56 55 54 53 51 50 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (76 75 44 43 37 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s8
  (with-basic-cvrp-solution (s1 ((31 46 15 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 43 37 18 52 17 13)
				 (34 29 27 25 23 21 20 41 40)
				 (47 35 9 32 30 79 28)
				 (48 22 45 42 39)
				 (14 8 16 57 56 55 54 53 51 50 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (76 75 44 12 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s9
  (with-basic-cvrp-solution (s1 ((31 46 15 75 5 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 43 37 18 52 17 13)
				 (34 29 27 25 23 21 20 41 40)
				 (47 35 9 32 30 79 28)
				 (48 22 45 42 39)
				 (14 8 16 57 56 55 54 53 51 50 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (76 4 3 44 12 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c1-s10
  (with-basic-cvrp-solution (s1 ((31 46 15 75 5 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 43 37 18 52 17 13)
				 (34 29 27 25 23 21 20 41 40)
				 (47 35 9 32 30 79 28)
				 (48 76 4 22 45 42)
				 (14 8 16 57 56 55 54 53 51 50 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (3 39 44 12 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s1
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s2
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 37 36 35 9 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s3
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (36 37 34 29 27 25 23 21 20)
				 (47 44 43 35 9 22 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s4
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (36 37 34 29 27 25 23 21 20)
				 (47 44 43 35 9 22 32 30 28)
				 (48 45 42 41 40 39)
				 (51 76 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 52 53 54 55 56 57 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s5
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (36 37 34 29 27 25 23 21 20)
				 (47 44 43 35 9 22 32 30 28)
				 (48 45 42 41 40 39)
				 (51 76 50 72 73 49)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 71 46 69 68 66 63)
				 (79 52 53 54 55 56 57 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s6
  (with-basic-cvrp-solution (s1 ((31 15 4 3 2 63 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (36 37 34 29 27 25 23 21 20)
				 (47 44 43 35 9 22 32 30 28)
				 (48 45 42 41 40 39)
				 (51 76 50 72 73 49)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 5 8 14 71 46 69 68 66)
				 (79 52 53 54 55 56 57 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s7
  (with-basic-cvrp-solution (s1 ((31 15 4 3 2 63 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (36 37 34 29 27 25 23 21 20)
				 (44 78 43 35 9 22 32 30 28)
				 (48 45 42 41 40 39)
				 (51 76 50 72 73 49)
				 (70 67 65 64 62 61 60 59 58)
				 (77 74 5 8 14 71 46 69 47 68 66)
				 (79 52 53 54 55 56 57 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s8
  (with-basic-cvrp-solution (s1 ((31 15 4 3 2 63 1)
				 (38 33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (36 37 34 29 27 25 23 21 20)
				 (44 78 43 35 9 22 32 30 28)
				 (40 48 39 41 45 42)
				 (51 76 50 72 73 49)
				 (70 67 65 64 62 61 60 59 58)
				 (77 74 5 8 14 71 46 69 47 68 66)
				 (79 52 53 54 55 56 57 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s9
  (with-basic-cvrp-solution (s1 ((31 15 4 3 2 63 1)
				 (38 33 26 11 10 7 6)
				 (24 19 25 27 29 17 16 13 12)
				 (36 37 34 23 21 18 20)
				 (44 78 43 35 9 22 32 30 28)
				 (40 48 39 41 45 42)
				 (51 76 50 72 73 49)
				 (70 67 65 64 62 61 60 59 58)
				 (77 74 5 8 14 71 46 69 47 68 66)
				 (79 52 53 54 55 56 57 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c2-s10
  (with-basic-cvrp-solution (s1 ((31 15 4 3 2 63 1)
				 (38 33 26 11 10 7 6)
				 (12 24 16 19 25 27 29 17 13)
				 (36 37 34 23 21 18 20)
				 (44 78 43 35 9 22 32 30 28)
				 (40 48 39 41 45 42)
				 (51 76 50 72 73 49)
				 (70 67 65 64 62 61 60 59 58)
				 (77 74 5 8 14 71 46 69 47 68 66)
				 (79 52 53 54 55 56 57 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s1
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 21 20 29 27 25 23 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s2
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 6 11 10 7)
				 (24 19 18 17 16 13 12)
				 (34 21 20 29 27 25 23 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s3
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 6 11 10 7)
				 (24 19 16 18 17 13 12)
				 (34 21 20 29 27 25 23 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s4
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 6 11 10 7)
				 (24 19 16 18 17 13 12)
				 (34 21 20 29 27 25 23 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 62 61 65 64 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s5
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 6 11 10 7)
				 (12 24 19 16 18 17 13)
				 (34 21 20 29 27 25 23 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 62 61 65 64 60 59 58)
				 (73 72 69 68 78 77 74 66 63 71)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s6
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 6 11 10 7)
				 (12 24 19 16 18 17 13)
				 (34 21 20 29 27 25 23 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 62 61 65 64 60 59 58)
				 (73 77 74 66 72 69 68 78 63 71)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s7
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 6 11 10 7)
				 (12 24 19 16 18 17 13)
				 (34 21 20 29 27 25 23 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 62 61 65 64 60 59 58)
				 (73 77 74 66 72 69 68 78 63 71)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s8
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 6 11 10 7)
				 (12 24 19 16 18 17 13)
				 (23 34 21 20 29 27 25 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 62 61 65 64 60 59 58)
				 (73 77 74 66 72 69 68 78 63 71)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s9
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 6 11 10 7)
				 (12 24 19 16 18 17 13)
				 (23 34 21 20 29 27 25 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (53 52 51 46 57 56 55 54 50 49)
				 (70 67 62 61 65 64 60 59 58)
				 (73 77 74 66 72 69 68 78 63 71)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c3-s10
  (with-basic-cvrp-solution (s1 ((14 2 8 5 31 15 4 3 1)
				 (33 9 26 6 11 10 7)
				 (12 24 19 16 18 17 13)
				 (23 34 21 20 29 27 25 22)
				 (44 30 37 28 43 35 47 32 38 36)
				 (40 48 41 39 45 42)
				 (53 52 51 46 57 56 55 54 50 49)
				 (62 61 65 64 60 59 58 70 67)
				 (73 77 74 66 72 69 68 78 63 71)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s1
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s2
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 48 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s3
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (42 41 40 39)
				 (57 56 55 54 53 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 48 52 51 50 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s4
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 35 32 38 37 36 30 28)
				 (42 41 40 39)
				 (57 56 55 54 53 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 48 52 51 50 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s5
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 35 32 38 37 36 30 28)
				 (42 41 40 39)
				 (57 56 55 54 53 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 71 69 68 78 77 74 66 63)
				 (79 48 52 51 50 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s6
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 35 32 38 37 36 30 28)
				 (42 41 46 40 39)
				 (57 56 55 54 53 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 71 69 68 78 77 74 66 63)
				 (79 48 52 51 50 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s7
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 35 32 38 37 36 30 28)
				 (42 41 46 40 39)
				 (57 56 55 9 54 53 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 71 69 68 78 77 74 66 63)
				 (79 48 52 51 50 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s8
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 37 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 35 32 38 36 30 28)
				 (42 41 46 40 39)
				 (57 56 55 9 54 53 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 71 69 68 78 77 74 66 63)
				 (79 48 52 51 50 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s9
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 37 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 35 32 38 36 30 28 71)
				 (42 41 46 40 39)
				 (57 56 55 9 54 53 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63)
				 (79 48 52 51 50 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c4-s10
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 37 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (34 21 20 29 27 25 23 22)
				 (47 44 43 35 32 38 36 30 28 71)
				 (42 41 46 40 39)
				 (57 56 55 9 54 53 49)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 69 68 78 77 74 66 63)
				 (79 48 52 51 50 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s1
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s2
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 76 75 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 37 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s3
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (32 38 76 75 35 47 44 43 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 37 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s4
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 21 20 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 4 3)
				 (32 38 76 75 35 47 44 43 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 37 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s5
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 21 20 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 4 3)
				 (32 38 76 75 35 47 44 43 30 28)
				 (48 79 37 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (45 42 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s6
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 21 20 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 4 3)
				 (32 38 76 75 35 47 44 43 30 28)
				 (48 79 37 41 78 77 74 73)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (40 39 72 71 69 68 66 63)
				 (45 42 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s7
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 21 20 2 1)
				 (33 26 11 10 9 7 6)
				 (24 71 18 17 16 13 12)
				 (34 29 27 25 23 22 4 3)
				 (32 38 76 75 35 47 44 43 30 28)
				 (48 79 37 41 78 77 74 73)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (40 39 72 19 69 68 66 63)
				 (45 42 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s8
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 21 20 2 1)
				 (7 6 9 33 26 11 10)
				 (24 71 18 17 16 13 12)
				 (34 29 27 25 23 22 4 3)
				 (32 38 76 75 35 47 44 43 30 28)
				 (48 79 37 41 78 77 74 73)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (40 39 72 19 69 68 66 63)
				 (45 42 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s9
  (with-basic-cvrp-solution (s1 ((31 15 20 2 8 5 21 14 1)
				 (7 6 9 33 26 11 10)
				 (24 71 18 17 16 13 12)
				 (34 29 27 25 23 22 4 3)
				 (32 38 76 75 35 47 44 43 30 28)
				 (48 79 37 41 78 77 74 73)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (40 39 72 19 69 68 66 63)
				 (45 42 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c5-s10
  (with-basic-cvrp-solution (s1 ((31 15 20 2 8 5 21 14 1)
				 (7 6 9 33 26 11 10)
				 (24 71 18 17 16 13 12)
				 (34 29 27 25 23 22 4 50 49)
				 (32 38 76 75 35 47 44 43 30 28)
				 (48 79 37 41 78 77 74 73)
				 (57 56 55 54 53 52 51 3 46)
				 (70 67 65 64 62 61 60 59 58)
				 (40 39 72 19 69 68 66 63)
				 (45 42 36))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s1
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 14 2 1)
				 (33 9 26 11 10 7 6)
				 (13 16 17 18 24 19 12)
				 (34 29 27 25 20 21 23 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 58 59 60 61 65 64 62)
				 (66 73 74 77 78 68 69 72 71 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s2
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 14 2 1)
				 (33 9 26 6 11 10 7)
				 (13 16 17 18 24 19 12)
				 (34 29 27 25 20 21 23 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 58 59 60 61 65 64 62)
				 (66 73 74 77 78 68 69 72 71 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s3
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 14 2 1)
				 (33 9 26 6 11 10 7)
				 (13 17 18 24 16 19 12)
				 (34 29 27 25 20 21 23 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 58 59 60 61 65 64 62)
				 (66 73 74 77 78 68 69 72 71 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s4
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 14 2 1)
				 (33 9 26 6 11 10 7)
				 (13 17 18 24 16 19 12)
				 (34 29 27 25 20 21 23 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 58 59 60 61 65 64 62)
				 (66 73 74 77 78 68 69 72 71 63)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s5
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 14 2 1)
				 (33 9 26 6 11 10 7)
				 (13 17 18 24 16 19 12)
				 (23 21 34 29 27 25 20 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (46 57 56 55 54 53 52 51 50 49)
				 (70 67 58 59 60 61 65 64 62)
				 (66 73 74 77 78 68 69 72 71 63)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s6
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 14 2 1)
				 (33 9 26 6 11 10 7)
				 (13 17 18 24 16 19 12)
				 (23 21 34 29 27 25 20 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (51 52 53 46 57 56 55 54 50 49)
				 (70 67 58 59 60 61 65 64 62)
				 (66 73 74 77 78 68 69 72 71 63)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s7
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 14 2 1)
				 (33 9 26 6 11 10 7)
				 (13 17 18 24 16 19 12)
				 (23 21 34 29 27 25 20 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (51 52 53 46 57 56 55 54 50 49)
				 (70 67 58 64 65 61 59 60 62)
				 (66 73 74 77 78 68 69 72 71 63)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s8
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 2 14 1)
				 (33 9 26 6 11 10 7)
				 (13 17 18 24 16 19 12)
				 (23 21 34 29 27 25 20 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (51 52 53 46 57 56 55 54 50 49)
				 (70 67 58 64 65 61 59 60 62)
				 (66 73 74 77 78 68 69 72 71 63)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s9
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 2 14 1)
				 (33 9 26 6 11 10 7)
				 (13 17 18 24 16 19 12)
				 (23 21 34 29 27 25 20 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (51 52 53 46 57 56 55 54 50 49)
				 (70 67 58 64 65 61 59 60 62)
				 (66 73 74 77 72 69 68 78 71 63)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c6-s10
  (with-basic-cvrp-solution (s1 ((31 15 4 3 5 8 2 14 1)
				 (33 9 26 6 11 10 7)
				 (13 17 18 24 16 19 12)
				 (23 21 34 29 27 25 20 22)
				 (36 38 32 35 47 43 44 30 37 28)
				 (40 48 39 41 45 42)
				 (51 52 46 57 56 55 54 50 53 49)
				 (70 67 58 64 65 61 59 60 62)
				 (66 73 74 77 72 69 68 78 71 63)
				 (76 75 79))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s1
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s2
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 22 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s3
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 20)
				 (37 36 38 43 44 47 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 22 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s4
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 20)
				 (37 36 38 32 35 47 43 44 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 22 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s5
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (49 50 51 52 34 29 27 25 23 20)
				 (37 36 38 32 35 47 43 44 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 22 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s6
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (49 50 51 52 34 29 27 25 23 20)
				 (37 36 38 32 35 47 43 44 30 28)
				 (48 45 42 41 40 39)
				 (66 68 69 57 56 55 54 53 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 63)
				 (79 21 22 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s7
  (with-basic-cvrp-solution (s1 ((31 15 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (49 50 51 52 34 29 27 25 23 20)
				 (37 36 38 32 35 47 43 44 30 28)
				 (48 45 42 41 40 39)
				 (66 68 69 57 56 55 54 53 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 4 5 8 14 71 63)
				 (79 21 22 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s8
  (with-basic-cvrp-solution (s1 ((31 15 3 2 1)
				 (33 9 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (49 50 51 52 34 29 27 25 23 20)
				 (37 36 38 32 35 47 43 44 30 28)
				 (48 45 42 41 40 39)
				 (66 68 69 57 56 55 54 53 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 4 5 8 14 71 63)
				 (79 21 22 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s9
  (with-basic-cvrp-solution (s1 ((31 15 3 74 77 78 2 1)
				 (33 9 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (49 50 51 52 34 29 27 25 23 20)
				 (37 36 38 32 35 47 43 44 30 28)
				 (48 45 42 41 40 39)
				 (66 68 69 57 56 55 54 53 46)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 4 5 8 14 71 63)
				 (79 21 22 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c7-s10
  (with-basic-cvrp-solution (s1 ((31 15 3 74 77 78 2 1)
				 (33 9 26 11 10 7 6)
				 (24 19 18 17 16 13 12)
				 (49 50 51 52 34 29 27 25 23 20)
				 (36 38 32 35 47 43 44 30 28)
				 (48 45 42 41 40 39)
				 (66 68 69 57 56 55 54 53 46)
				 (70 67 65 64 62 61 60 59 58)
				 (73 72 4 5 8 37 14 71 63)
				 (79 21 22 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s1
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s2
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s3
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 76)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 20 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s4
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (42 45 22 76)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 20 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s5
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (42 45 22 76)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s6
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 71 72 73 13 12)
				 (42 45 22 76)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 17 16 69 68 66 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s7
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 37 43 44 7 6)
				 (24 19 18 71 72 73 13 12)
				 (42 45 22 76)
				 (36 38 9 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 17 16 69 68 66 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s8
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 37 43 44 7 6)
				 (24 19 18 71 72 73 13 12)
				 (42 45 22 76)
				 (36 38 9 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (66 77 74 17 16 69 68 78 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s9
  (with-basic-cvrp-solution (s1 ((13 73 72 4 3 2 1)
				 (33 26 11 10 37 43 44 7 6)
				 (24 19 18 71 31 15 14 8 5 12)
				 (42 45 22 76)
				 (36 38 9 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (66 77 74 17 16 69 68 78 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c8-s10
  (with-basic-cvrp-solution (s1 ((13 73 72 4 3 2 1)
				 (33 26 11 10 37 43 44 7 6)
				 (14 18 71 31 15 19 24 8 5 12)
				 (42 45 22 76)
				 (36 38 9 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (66 77 74 17 16 69 68 78 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s1
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s2
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s3
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 76)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 20 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s4
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (42 45 22 76)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 20 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s5
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (42 45 22 76)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s6
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 71 72 73 13 12)
				 (42 45 22 76)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 17 16 69 68 66 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s7
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 37 43 44 7 6)
				 (24 19 18 71 72 73 13 12)
				 (42 45 22 76)
				 (36 38 9 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 17 16 69 68 66 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s8
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 37 43 44 7 6)
				 (24 19 18 71 72 73 13 12)
				 (42 45 22 76)
				 (36 38 9 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (66 77 74 17 16 69 68 78 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s9
  (with-basic-cvrp-solution (s1 ((13 73 72 4 3 2 1)
				 (33 26 11 10 37 43 44 7 6)
				 (24 19 18 71 31 15 14 8 5 12)
				 (42 45 22 76)
				 (36 38 9 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (66 77 74 17 16 69 68 78 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c9-s10
  (with-basic-cvrp-solution (s1 ((13 73 72 4 3 2 1)
				 (33 26 11 10 37 43 44 7 6)
				 (14 18 71 31 15 19 24 8 5 12)
				 (42 45 22 76)
				 (36 38 9 47 35 32 30 28)
				 (48 34 29 27 25 23 41 40 39)
				 (57 56 55 54 53 75 20 46)
				 (70 67 65 64 62 61 60 59 58)
				 (66 77 74 17 16 69 68 78 63)
				 (79 21 52 51 50 49))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s1
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (47 44 43 38 37 36 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s2
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 45 42 41 40 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (79 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s3
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 22 21 20)
				 (36 38 37 43 44 47 35 32 30 28)
				 (48 79 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (40 41 42 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s4
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 9 7 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 35 47 44 43 37 21 20)
				 (36 38 22 32 30 28)
				 (48 79 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (78 77 74 73 72 71 69 68 66 63)
				 (40 41 42 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s5
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 71 72 73 74 77 78 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 35 47 44 43 37 21 20)
				 (36 38 22 32 30 28)
				 (48 79 39)
				 (57 56 55 54 53 52 51 50 49 46)
				 (70 67 65 64 62 61 60 59 58)
				 (7 9 69 68 66 63)
				 (40 41 42 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s6
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 71 72 73 74 77 78 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 35 47 44 43 37 21 20)
				 (36 38 22 32 30 28)
				 (48 79 39)
				 (49 53 52 51 50 54 55 56 57 46)
				 (70 67 65 64 62 61 60 59 58)
				 (7 9 69 68 66 63)
				 (40 41 42 45 76 75))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s7
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 11 10 71 72 73 74 77 78 6)
				 (24 19 18 17 16 13 12)
				 (34 29 27 25 23 35 47 44 43 37 21 20)
				 (36 38 22 32 30 28)
				 (48 79 39)
				 (49 53 52 75 76 45 42)
				 (70 67 65 64 62 61 60 59 58)
				 (7 9 69 68 66 63)
				 (40 41 46 57 56 55 54 50 51))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s8
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 47 35 23 25 27 29 34 74 77 78 6)
				 (24 19 18 17 16 13 12)
				 (73 72 71 10 11 44 43 37 21 20)
				 (36 38 22 32 30 28)
				 (48 79 39)
				 (49 53 52 75 76 45 42)
				 (70 67 65 64 62 61 60 59 58)
				 (7 9 69 68 66 63)
				 (40 41 46 57 56 55 54 50 51))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s9
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 47 35 23 25 27 29 34 74 77 78 6)
				 (24 19 18 79 48 12)
				 (73 72 71 10 11 44 43 37 21 20)
				 (36 38 22 32 30 28)
				 (13 16 17 39)
				 (49 53 52 75 76 45 42)
				 (70 67 65 64 62 61 60 59 58)
				 (7 9 69 68 66 63)
				 (40 41 46 57 56 55 54 50 51))
				a-n80-k10-problem)
    s1))

(defparameter a-n80-k10-problem-c10-s10
  (with-basic-cvrp-solution (s1 ((31 15 14 8 5 4 3 2 1)
				 (33 26 47 35 19 25 27 29 34 74 77 78 6)
				 (24 23 18 79 48 12)
				 (73 72 71 10 11 44 43 37 21 20)
				 (36 38 22 32 30 28)
				 (13 16 17 39)
				 (49 53 52 75 76 45 42)
				 (70 67 65 64 62 61 60 59 58)
				 (7 9 69 68 66 63)
				 (40 41 46 57 56 55 54 50 51))
				a-n80-k10-problem)
    s1))

(defparameter *data-problem-criterion-solutions*
  (list (list (list a-n32-k5-problem-c1-s1 a-n32-k5-problem-c1-s2
		    a-n32-k5-problem-c1-s3 a-n32-k5-problem-c1-s4
		    a-n32-k5-problem-c1-s5 a-n32-k5-problem-c1-s6
		    a-n32-k5-problem-c1-s7 a-n32-k5-problem-c1-s8
		    a-n32-k5-problem-c1-s9 a-n32-k5-problem-c1-s10)
	      (list a-n32-k5-problem-c2-s1 a-n32-k5-problem-c2-s2
		    a-n32-k5-problem-c2-s3 a-n32-k5-problem-c2-s4
		    a-n32-k5-problem-c2-s5 a-n32-k5-problem-c2-s6
		    a-n32-k5-problem-c2-s7 a-n32-k5-problem-c2-s8
		    a-n32-k5-problem-c2-s9 a-n32-k5-problem-c2-s10)
	      (list a-n32-k5-problem-c3-s1 a-n32-k5-problem-c3-s2
		    a-n32-k5-problem-c3-s3 a-n32-k5-problem-c3-s4
		    a-n32-k5-problem-c3-s5 a-n32-k5-problem-c3-s6
		    a-n32-k5-problem-c3-s7 a-n32-k5-problem-c3-s8
		    a-n32-k5-problem-c3-s9 a-n32-k5-problem-c3-s10)
	      (list a-n32-k5-problem-c4-s1 a-n32-k5-problem-c4-s2
		    a-n32-k5-problem-c4-s3 a-n32-k5-problem-c4-s4
		    a-n32-k5-problem-c4-s5 a-n32-k5-problem-c4-s6
		    a-n32-k5-problem-c4-s7 a-n32-k5-problem-c4-s8
		    a-n32-k5-problem-c4-s9 a-n32-k5-problem-c4-s10)
	      (list a-n32-k5-problem-c5-s1 a-n32-k5-problem-c5-s2
		    a-n32-k5-problem-c5-s3 a-n32-k5-problem-c5-s4
		    a-n32-k5-problem-c5-s5 a-n32-k5-problem-c5-s6
		    a-n32-k5-problem-c5-s7 a-n32-k5-problem-c5-s8
		    a-n32-k5-problem-c5-s9 a-n32-k5-problem-c5-s10)
	      (list a-n32-k5-problem-c6-s1 a-n32-k5-problem-c6-s2
		    a-n32-k5-problem-c6-s3 a-n32-k5-problem-c6-s4
		    a-n32-k5-problem-c6-s5 a-n32-k5-problem-c6-s6
		    a-n32-k5-problem-c6-s7 a-n32-k5-problem-c6-s8
		    a-n32-k5-problem-c6-s9 a-n32-k5-problem-c6-s10)
	      (list a-n32-k5-problem-c7-s1 a-n32-k5-problem-c7-s2
		    a-n32-k5-problem-c7-s3 a-n32-k5-problem-c7-s4
		    a-n32-k5-problem-c7-s5 a-n32-k5-problem-c7-s6
		    a-n32-k5-problem-c7-s7 a-n32-k5-problem-c7-s8
		    a-n32-k5-problem-c7-s9 a-n32-k5-problem-c7-s10)
	      (list a-n32-k5-problem-c8-s1 a-n32-k5-problem-c8-s2
		    a-n32-k5-problem-c8-s3 a-n32-k5-problem-c8-s4
		    a-n32-k5-problem-c8-s5 a-n32-k5-problem-c8-s6
		    a-n32-k5-problem-c8-s7 a-n32-k5-problem-c8-s8
		    a-n32-k5-problem-c8-s9 a-n32-k5-problem-c8-s10)
	      (list a-n32-k5-problem-c9-s1 a-n32-k5-problem-c9-s2
		    a-n32-k5-problem-c9-s3 a-n32-k5-problem-c9-s4
		    a-n32-k5-problem-c9-s5 a-n32-k5-problem-c9-s6
		    a-n32-k5-problem-c9-s7 a-n32-k5-problem-c9-s8
		    a-n32-k5-problem-c9-s9 a-n32-k5-problem-c9-s10)
	      (list a-n32-k5-problem-c10-s1 a-n32-k5-problem-c10-s2
		    a-n32-k5-problem-c10-s3 a-n32-k5-problem-c10-s4
		    a-n32-k5-problem-c10-s5 a-n32-k5-problem-c10-s6
		    a-n32-k5-problem-c10-s7 a-n32-k5-problem-c10-s8
		    a-n32-k5-problem-c10-s9 a-n32-k5-problem-c10-s10))
	(list (list a-n65-k9-problem-c1-s1 a-n65-k9-problem-c1-s2
		    a-n65-k9-problem-c1-s3 a-n65-k9-problem-c1-s4
		    a-n65-k9-problem-c1-s5 a-n65-k9-problem-c1-s6
		    a-n65-k9-problem-c1-s7 a-n65-k9-problem-c1-s8
		    a-n65-k9-problem-c1-s9 a-n65-k9-problem-c1-s10)
	      (list a-n65-k9-problem-c2-s1 a-n65-k9-problem-c2-s2
		    a-n65-k9-problem-c2-s3 a-n65-k9-problem-c2-s4
		    a-n65-k9-problem-c2-s5 a-n65-k9-problem-c2-s6
		    a-n65-k9-problem-c2-s7 a-n65-k9-problem-c2-s8
		    a-n65-k9-problem-c2-s9 a-n65-k9-problem-c2-s10)
	      (list a-n65-k9-problem-c3-s1 a-n65-k9-problem-c3-s2
		    a-n65-k9-problem-c3-s3 a-n65-k9-problem-c3-s4
		    a-n65-k9-problem-c3-s5 a-n65-k9-problem-c3-s6
		    a-n65-k9-problem-c3-s7 a-n65-k9-problem-c3-s8
		    a-n65-k9-problem-c3-s9 a-n65-k9-problem-c3-s10)
	      (list a-n65-k9-problem-c4-s1 a-n65-k9-problem-c4-s2
		    a-n65-k9-problem-c4-s3 a-n65-k9-problem-c4-s4
		    a-n65-k9-problem-c4-s5 a-n65-k9-problem-c4-s6
		    a-n65-k9-problem-c4-s7 a-n65-k9-problem-c4-s8
		    a-n65-k9-problem-c4-s9 a-n65-k9-problem-c4-s10)
	      (list a-n65-k9-problem-c5-s1 a-n65-k9-problem-c5-s2
		    a-n65-k9-problem-c5-s3 a-n65-k9-problem-c5-s4
		    a-n65-k9-problem-c5-s5 a-n65-k9-problem-c5-s6
		    a-n65-k9-problem-c5-s7 a-n65-k9-problem-c5-s8
		    a-n65-k9-problem-c5-s9 a-n65-k9-problem-c5-s10)
	      (list a-n65-k9-problem-c6-s1 a-n65-k9-problem-c6-s2
		    a-n65-k9-problem-c6-s3 a-n65-k9-problem-c6-s4
		    a-n65-k9-problem-c6-s5 a-n65-k9-problem-c6-s6
		    a-n65-k9-problem-c6-s7 a-n65-k9-problem-c6-s8
		    a-n65-k9-problem-c6-s9 a-n65-k9-problem-c6-s10)
	      (list a-n65-k9-problem-c7-s1 a-n65-k9-problem-c7-s2
		    a-n65-k9-problem-c7-s3 a-n65-k9-problem-c7-s4
		    a-n65-k9-problem-c7-s5 a-n65-k9-problem-c7-s6
		    a-n65-k9-problem-c7-s7 a-n65-k9-problem-c7-s8
		    a-n65-k9-problem-c7-s9 a-n65-k9-problem-c7-s10)
	      (list a-n65-k9-problem-c8-s1 a-n65-k9-problem-c8-s2
		    a-n65-k9-problem-c8-s3 a-n65-k9-problem-c8-s4
		    a-n65-k9-problem-c8-s5 a-n65-k9-problem-c8-s6
		    a-n65-k9-problem-c8-s7 a-n65-k9-problem-c8-s8
		    a-n65-k9-problem-c8-s9 a-n65-k9-problem-c8-s10)
	      (list a-n65-k9-problem-c9-s1 a-n65-k9-problem-c9-s2
		    a-n65-k9-problem-c9-s3 a-n65-k9-problem-c9-s4
		    a-n65-k9-problem-c9-s5 a-n65-k9-problem-c9-s6
		    a-n65-k9-problem-c9-s7 a-n65-k9-problem-c9-s8
		    a-n65-k9-problem-c9-s9 a-n65-k9-problem-c9-s10)
	      (list a-n65-k9-problem-c10-s1 a-n65-k9-problem-c10-s2
		    a-n65-k9-problem-c10-s3 a-n65-k9-problem-c10-s4
		    a-n65-k9-problem-c10-s5 a-n65-k9-problem-c10-s6
		    a-n65-k9-problem-c10-s7 a-n65-k9-problem-c10-s8
		    a-n65-k9-problem-c10-s9 a-n65-k9-problem-c10-s10))
	(list (list a-n80-k10-problem-c1-s1 a-n80-k10-problem-c1-s2
		    a-n80-k10-problem-c1-s3 a-n80-k10-problem-c1-s4
		    a-n80-k10-problem-c1-s5 a-n80-k10-problem-c1-s6
		    a-n80-k10-problem-c1-s7 a-n80-k10-problem-c1-s8
		    a-n80-k10-problem-c1-s9 a-n80-k10-problem-c1-s10)
	      (list a-n80-k10-problem-c2-s1 a-n80-k10-problem-c2-s2
		    a-n80-k10-problem-c2-s3 a-n80-k10-problem-c2-s4
		    a-n80-k10-problem-c2-s5 a-n80-k10-problem-c2-s6
		    a-n80-k10-problem-c2-s7 a-n80-k10-problem-c2-s8
		    a-n80-k10-problem-c2-s9 a-n80-k10-problem-c2-s10)
	      (list a-n80-k10-problem-c3-s1 a-n80-k10-problem-c3-s2
		    a-n80-k10-problem-c3-s3 a-n80-k10-problem-c3-s4
		    a-n80-k10-problem-c3-s5 a-n80-k10-problem-c3-s6
		    a-n80-k10-problem-c3-s7 a-n80-k10-problem-c3-s8
		    a-n80-k10-problem-c3-s9 a-n80-k10-problem-c3-s10)
	      (list a-n80-k10-problem-c4-s1 a-n80-k10-problem-c4-s2
		    a-n80-k10-problem-c4-s3 a-n80-k10-problem-c4-s4
		    a-n80-k10-problem-c4-s5 a-n80-k10-problem-c4-s6
		    a-n80-k10-problem-c4-s7 a-n80-k10-problem-c4-s8
		    a-n80-k10-problem-c4-s9 a-n80-k10-problem-c4-s10)
	      (list a-n80-k10-problem-c5-s1 a-n80-k10-problem-c5-s2
		    a-n80-k10-problem-c5-s3 a-n80-k10-problem-c5-s4
		    a-n80-k10-problem-c5-s5 a-n80-k10-problem-c5-s6
		    a-n80-k10-problem-c5-s7 a-n80-k10-problem-c5-s8
		    a-n80-k10-problem-c5-s9 a-n80-k10-problem-c5-s10)
	      (list a-n80-k10-problem-c6-s1 a-n80-k10-problem-c6-s2
		    a-n80-k10-problem-c6-s3 a-n80-k10-problem-c6-s4
		    a-n80-k10-problem-c6-s5 a-n80-k10-problem-c6-s6
		    a-n80-k10-problem-c6-s7 a-n80-k10-problem-c6-s8
		    a-n80-k10-problem-c6-s9 a-n80-k10-problem-c6-s10)
	      (list a-n80-k10-problem-c7-s1 a-n80-k10-problem-c7-s2
		    a-n80-k10-problem-c7-s3 a-n80-k10-problem-c7-s4
		    a-n80-k10-problem-c7-s5 a-n80-k10-problem-c7-s6
		    a-n80-k10-problem-c7-s7 a-n80-k10-problem-c7-s8
		    a-n80-k10-problem-c7-s9 a-n80-k10-problem-c7-s10)
	      (list a-n80-k10-problem-c8-s1 a-n80-k10-problem-c8-s2
		    a-n80-k10-problem-c8-s3 a-n80-k10-problem-c8-s4
		    a-n80-k10-problem-c8-s5 a-n80-k10-problem-c8-s6
		    a-n80-k10-problem-c8-s7 a-n80-k10-problem-c8-s8
		    a-n80-k10-problem-c8-s9 a-n80-k10-problem-c8-s10)
	      (list a-n80-k10-problem-c9-s1 a-n80-k10-problem-c9-s2
		    a-n80-k10-problem-c9-s3 a-n80-k10-problem-c9-s4
		    a-n80-k10-problem-c9-s5 a-n80-k10-problem-c9-s6
		    a-n80-k10-problem-c9-s7 a-n80-k10-problem-c9-s8
		    a-n80-k10-problem-c9-s9 a-n80-k10-problem-c9-s10)
	      (list a-n80-k10-problem-c10-s1 a-n80-k10-problem-c10-s2
		    a-n80-k10-problem-c10-s3 a-n80-k10-problem-c10-s4
		    a-n80-k10-problem-c10-s5 a-n80-k10-problem-c10-s6
		    a-n80-k10-problem-c10-s7 a-n80-k10-problem-c10-s8
		    a-n80-k10-problem-c10-s9 a-n80-k10-problem-c10-s10))))

(defparameter *data-neighborhood-improvements-optimun*
  (list `(20303 -220)
	`(4963 -192)
	`(3929 -162)
	`(2728 -135)
	`(1741 -114)
	`(883 -85)
	`(577 -39)
	`(350 -42)
	`(169 -9)
	`(131 -9)
	`(20535 -257)
	`(18604 -219)
	`(4199 -159)
	`(2863 -124)
	`(2965 -70)
	`(2668 -68)
	`(497 -28)
	`(370 -20)
	`(220 -17)
	`(14 -2)
	`(268 -128)
	`(168 -110)
	`(156 -109)
	`(118 -78)
	`(68 -66)
	`(42 -36)
	`(34 -35)
	`(20 -20)
	`(18 -49)
	`(10 -8)
	`(301 -128)
	`(188 -110)
	`(168 -109)
	`(128 -78)
	`(71 -66)
	`(45 -36)
	`(37 -35)
	`(23 -20)
	`(23 -49)
	`(13 -8)
	`(1151 -208)
	`(430 -133)
	`(365 -101)
	`(241 -86)
	`(171 -79)
	`(131 -57)
	`(128 -45)
	`(109 -25)
	`(76 -22)
	`(45 -36)
	`(308 -133)
	`(195 -134)
	`(107 -110)
	`(89 -109)
	`(55 -69)
	`(40 -59)
	`(23 -36)
	`(12 -11)
	`(8 -7)
	`(5 -2)
	`(341 -133)
	`(222 -134)
	`(119 -110)
	`(94 -109)
	`(58 -69)
	`(43 -59)
	`(26 -36)
	`(15 -19)
	`(11 -11)
	`(7 -7)
	`(1156 -193)
	`(518 -155)
	`(485 -133)
	`(386 -98)
	`(468 -89)
	`(288 -86)
	`(161 -47)
	`(151 -56)
	`(187 -45)
	`(122 -29)
	`(1202 -193)
	`(608 -155)
	`(564 -133)
	`(464 -111)
	`(146 -98)
	`(232 -86)
	`(102 -47)
	`(101 -56)
	`(126 -32)
	`(73 -23)
	`(1247 -193)
	`(654 -133)
	`(605 -103)
	`(350 -96)
	`(256 -65)
	`(298 -68)
	`(363 -46)
	`(265 -46)
	`(296 -40)
	`(253 -32)
	`(48856 -300)
	`(38084 -229)
`(24493 -239)
`(14747 -199)
`(10709 -156)
`(7874 -138)
`(6759 -173)
`(4794 -96)
`(3924 -94)
`(4275 -95)
`(49142 -300)
`(38505 -225)
`(27155 -200)
`(21475 -199)
`(17465 -171)
`(9301 -151)
`(8487 -124)
`(4369 -122)
`(3176 -92)
`(3059 -82)
`(598 -194)
`(550 -125)
`(506 -124)
`(460 -121)
`(396 -113)
`(272 -108)
`(230 -83)
`(214 -76)
`(200 -76)
`(182 -74)
`(781 -194)
`(733 -133)
`(570 -149)
`(465 -124)
`(413 -121)
`(322 -108)
`(281 -89)
`(233 -83)
`(217 -76)
`(203 -74)
`(3507 -264)
`(3025 -222)
`(2778 -183)
`(2532 -155)
`(2303 -154)
`(2157 -192)
`(1289 -167)
`(1143 -156)
`(816 -96)
`(836 -83)
`(684 -199)
`(632 -150)
`(583 -141)
`(537 -140)
`(465 -134)
`(314 -128)
`(268 -78)
`(247 -75)
`(229 -71)
`(204 -62)
`(864 -199)
`(812 -150)
`(759 -141)
`(695 -140)
`(600 -134)
`(398 -131)
`(327 -78)
`(302 -75)
`(275 -73)
`(239 -66)
`(3492 -264)
`(3022 -222)
`(2773 -183)
`(2556 -177)
`(2243 -192)
`(1527 -155)
`(1287 -147)
`(919 -104)
`(1068 -87)
`(976 -88)
`(3489 -268)
`(3185 -211)
`(2345 -186)
`(1816 -183)
`(1646 -180)
`(1470 -174)
`(1284 -155)
`(1016 -123)
`(948 -146)
`(931 -92)
`(3462 -264)
`(2964 -222)
`(2726 -183)
`(2518 -177)
`(2191 -176)
`(1954 -155)
`(1721 -128)
`(1129 -110)
`(1038 -91)
`(942 -104)
`(108223 -306)
`(90155 -279)
`(81901 -257)
`(86920 -251)
`(61512 -225)
`(45274 -184)
`(37896 -173)
`(24145 -134)
`(24025 -134)
`(22097 -159)
`(108932 -306)
`(90567 -274)
`(76202 -250)
`(72901 -267)
`(55694 -204)
`(45225 -192)
`(32391 -189)
`(25990 -164)
`(20177 -133)
`(13828 -132)
`(1156 -154)
`(1046 -148)
`(922 -144)
`(866 -138)
`(782 -130)
`(690 -105)
`(650 -104)
`(572 -103)
`(506 -97)
`(462 -94)
`(1907 -180)
`(1979 -178)
`(1818 -154)
`(1652 -148)
`(1473 -145)
`(1327 -159)
`(1356 -135)
`(1237 -117)
`(1117 -104)
`(1017 -100)
`(7244 -270)
`(7046 -214)
`(5616 -198)
`(5216 -179)
`(5164 -175)
`(4492 -167)
`(4014 -156)
`(3687 -155)
`(3085 -155)
`(2666 -136)
`(1336 -159)
`(1216 -152)
`(1116 -144)
`(1059 -138)
`(969 -132)
`(831 -121)
`(779 -110)
`(674 -107)
`(573 -105)
`(536 -86)
`(2076 -166)
`(2266 -159)
`(2084 -152)
`(1891 -152)
`(1751 -164)
`(1623 -151)
`(1380 -144)
`(1273 -140)
`(1122 -136)
`(1051 -123)
`(7411 -268)
`(6364 -260)
`(6324 -228)
`(5287 -193)
`(5756 -177)
`(4739 -170)
`(4231 -164)
`(3643 -160)
`(3490 -141)
`(2719 -135)
`(7366 -268)
`(6334 -260)
`(6260 -228)
`(5223 -193)
`(5697 -177)
`(4681 -170)
`(4173 -164)
`(3573 -160)
`(3457 -141)
`(2687 -135)
`(7223 -268)
`(6259 -236)
`(5581 -231)
`(5900 -219)
`(5229 -183)
`(4785 -160)
`(4376 -153)
`(3713 -146)
`(3980 -140)
`(3628 -131)))
