(with-basic-clients (1 2 3 4 5 6)
      (let* ((v1 (basic-vehicle 1))
	     (v2 (basic-vehicle 2))
	     (d0 (basic-depot))
	     (r1 (basic-route :id 1 :vehicle v1 :depot d0
			      :clients (list c1 c2 c3)))
	     (r2 (basic-route :id 2 :vehicle v2 :depot d0
			      :clients (list c4 c5 c6)))
	     (s (basic-solution :id 1 :routes (list r1 r2)))
	     (graph (init-graph s)))

	(format t "~a~%" graph)))

(with-basic-clients (1 2)
  (let* ((v1 (basic-vehicle 1))
	 (d0 (basic-depot))
	 (r1 (route-for-simulation :id 1 :vehicle v1 :depot d0
			  :clients (list c1 c2 d0) :previous-client d0))
	 (s (basic-solution :id 1 :routes (list r1)))
	 (out (new-output-node :output-value 0))
	 (graph (init-graph s))
	 (dist-mat #2A((0 1 2) (1 0 2) (2 2 0))))
    (progn
      (setf (gethash 'total-distance (slot-to-output graph)) out)
      (loop for r in (routes s) do
	(progn
	  (loop for c in (clients r) do
	    (progn 
	      (move-node-from-to (previous-client r) c 'total-distance dist-mat graph)
	      (setf (previous-client r) c)))))
      (format t "EVALUATION (should be 5): ~a~%" (output-value out))
      (format t "~a~%" graph))))
