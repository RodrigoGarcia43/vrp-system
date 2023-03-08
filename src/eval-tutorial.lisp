(defparameter c1 (basic-client 1))
(defparameter c2 (basic-client 2))
(defparameter c3 (basic-client 3))
(defparameter c4 (basic-client 4))
(defparameter c5 (basic-client 5))
(defparameter c6 (basic-client 6))

(defparameter v1 (basic-vehicle 1))
(defparameter v2 (basic-vehicle 2))

(defparameter d0 (basic-depot))

;; remember to start and end all the routes with depot
;; the starting depot is placed in the previous-client slot
(defparameter r1 (route-for-simulation :id 1 :vehicle v1 :depot d0
				       :clients (list c1 c2 c3 d0) :previous-client d0))
(defparameter r2 (route-for-simulation :id 2 :vehicle v2 :depot d0
				       :clients (list c4 c5 c6 d0) :previous-client d0))

(defparameter s1 (basic-solution :id 1 :routes (list r1 r2)))

(defparameter dist-mat #2A((0 1 2 3 4 5 6)
			   (1 0 1 2 1 3 2)
			   (2 1 0 2 2 2 2)
			   (3 2 2 0 1 2 1)
			   (4 1 2 1 0 2 3)
			   (5 3 2 2 2 0 1)
			   (6 2 2 1 3 1 0)))

(defparameter graph (init-graph s1)))

(format t "~a%" graph)

(progn
    (def-var total-distance 0 graph) 
    (loop for r in (routes s1) do
      (progn
        (def-var route-distance 0 graph) 
        (loop for c in (clients r) do
           (progn 
               (increment-distance (previous-client r) c route-distance dist-mat graph) 
               (setf (previous-client r) c))) 
        (increment-value total-distance route-distance graph))) 
    (return-value total-distance graph))

(format t "~a~%" graph)

(do-operations `(,#'remove-client-from 0 1 ,graph))

(format t "~a~%" graph)

(do-operations `(,#'insert-client-to 0 1 1 ,graph))

(format t "~a~%"  graph)

(defparameter c1 (basic-cvrp-client 1 1))
(defparameter c2 (basic-cvrp-client 2 1))
(defparameter c3 (basic-cvrp-client 3 4))
(defparameter c4 (basic-cvrp-client 4 3))
(defparameter c5 (basic-cvrp-client 5 2))
(defparameter c6 (basic-cvrp-client 6 1))

(defparameter v1 (cvrp-vehicle 1 10))
(defparameter v2 (cvrp-vehicle 2 10))

(defparameter d0 (basic-depot))

;; Solution for the eval-graph
;; remember to start and end all the routes with depot
;; the starting depot is placed in the previous-client slot
(defparameter r1 (route-for-simulation :id 1 :vehicle v1 :depot d0
				       :clients (list c1 c2 c3 (clone d0)) :previous-client (clone d0)))
(defparameter r2 (route-for-simulation :id 2 :vehicle v2 :depot d0
				       :clients (list c4 c5 c6 (clone d0)) :previous-client (clone d0)))

(defparameter s1 (basic-solution :id 1 :routes (list r1 r2)))


       (defparameter dist-mat #2A((0 1 2 3 4 5 6)
				  (1 0 9 2 7 3 2)
				  (2 9 0 2 2 2 2)
				  (3 2 2 0 8 2 1)
				  (4 7 2 8 0 2 9)
				  (5 3 2 2 2 0 1)
				  (6 2 2 1 9 1 0)))

(defparameter problem (finite-fleet-cvrp-problem :id 1 :clients (list c1 c2 c3 c4 c5 c6)
						 :depot d0 :distance-matrix dist-mat :fleet (list v1 v2) ))

(defparameter graph (init-graph s1)))

(format t "~a%" graph)

(progn
  (def-var total-distance 0 graph)
  (loop for r in (routes s1) do 
    (progn
      (def-var route-distance 0 graph)
      (def-var route-demand (capacity (vehicle r)) graph) 
      (loop for c in (clients r) do 
	(progn
	  (increment-distance (previous-client r) c route-distance dist-mat graph)
	  (decrement-demand c route-demand graph) 
	  (setf (previous-client r) c)))
      (increment-value total-distance route-distance graph)
      (apply-penalty route-demand total-distance 10 graph)) 
    (return-value total-distance graph)))

(format t "~a~%" (output-value (output graph)))

(progn 
(format t "~a~%" (solution-track graph))
(format t "~a~%" (output-value (output graph))))

(remove-client-from 1 2 0 graph)

(insert-client-to 2 2 0 graph)

(format t "~a~%" (solution-track graph))

(do-suite-operations graph (list (op-select-client 1 1 0) 
				 (op-select-client 1 1 1)
				 (op-select-client 1 1 2)
				 (op-insert-client 2 4 0)
				 (op-insert-client 2 1 1)
				 (op-insert-client 2 5 2)))

(do-suite-operations graph (list (op-insert-client 2 5 2)))

(progn 
(format t "~a~%" (solution-track graph))
(format t "~a~%" (output-value (output graph))))

(format t "~a~%" graph)

(undo-suite-operations graph (list (op-select-client 1 2 0) 
				 (op-select-client 2 3 1)
				 (op-insert-client 1 1 1)
				 (op-insert-client 2 1 0)))

(format t "~a~%" (solution-track graph))

(format t "~a~%"  graph)

(let* (
       ;; here you define the neighborhood criteria
       (code `((select-route r1)
	       (select-route r2)
	       (select-client c1 from r1)
	       (select-client c2 from r2)
	       (swap-clients c1 c2)))
       ;; here we create the neighborhood
       (neighborhood (build-neighborhood-tree code solution problem))

;; here we define the two phase exploration generator
      (two-phase-gen (exploration-exploitation-algorithm neighborhood graph)))
 ;; this line has to be added to use the customize indexer
 (prepare-neighborhood-for-exploration neighborhood)
 ;; print all solutions in the neighborhood
 (loop while t
    doing
       (multiple-value-bind (cur-sol cur-cost) (funcall two-phase-gen)
     	 (when (not cur-sol)
     	   (return))
     	 (format t "~A : ~A~%" cur-sol cur-cost))))
