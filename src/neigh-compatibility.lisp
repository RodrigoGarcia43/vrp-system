(in-package :vrp)

(defmethod transform-to-counting-solution (vrp-sol)
  (let ((route-list (loop for r in (routes vrp-sol)
		       collecting (length (clients r)))))
    (make-instance `counting-basic-solution :routes route-list
					    :num-clients (apply #'+ route-list))))

(defmethod compute-delta-position ((symbol (eql 'r))
				   coordinate
				   route position)
  ;; ('r route-number)
  0)

(defmethod compute-delta-position ((symbol (eql 'a))
				   coordinate
				   route position)
  ;; ('a route-number client-position id-operation)
  ;; check if this operation takes places in the same route and
  ;; afect the current position
  (if (or (not (= route (first coordinate)))
	  (>= (second coordinate) position))
      0
      -1))

(defmethod compute-delta-position ((symbol (eql 'b))
				   coordinate
				   route position)
  ;; ('b route-number insertion-position id-select-client-operation)
  ;; check if this operation takes places in the same route and
  ;; afect the current position
  (if (or (not (= route (first coordinate)))
	  (> (second coordinate) position))
      0
      1))

(defmethod compute-delta-position ((symbol (eql 'c))
				   coordinate
				   route position)
  ;; ('c id-select-client1-operation id-select-client2-operation)
  ;; check if any of this clients belong to the route and, if their
  ;; current positions affect the one that we are tracing
  (declare (special defined-ops-map
		    clients-current-position))
  (let* ((coord1 (nth (first coordinate) (gethash 'a defined-ops-map)))
	 (client1-cur-pos (getf clients-current-position (first coordinate)))
	 (coord2 (nth (second coordinate) (gethash 'a defined-ops-map)))
	 (client2-cur-pos (getf clients-current-position (second coordinate)))
	 (delta-pos 0))
    ;; here we check if the first client belong to route and, if its current
    ;; position affects position
    (when (and (= route (first coord1))
	       (<= client1-cur-pos position))
      (incf delta-pos))
    ;; here we check if the second client belong to route and, if its current
    ;; position affects position
    (when (and (= route (first coord2))
	       (<= client2-cur-pos position))
      (incf delta-pos))
    ;; here we return delta position
    delta-pos))

(defmethod compute-delta-position ((symbol (eql 'e))
				   coordinate
				   route position)
  ;; ('e route-number start-position length id-operation)
  ;; check if this operation takes places in the same route and
  ;; afect the current position
  (if (or (not (= route (first coordinate)))
	  (>= (second coordinate) position))
      0
      ;; when the subroute overlaps with the selectd position
      (if (> (+ (second coordinate) (third coordinate))
	     position)
	  (- (second coordinate) position)
	  (- (third coordinate)))))

(defmethod compute-delta-position ((symbol (eql 'f))
				   coordinate
				   route position)
  ;; ('f route-number insertion-position id-select-subroute-operation)
  (declare (special defined-ops-map))
  (let* ((subroute-len (third (nth (third coordinate)
				   (gethash 'e defined-ops-map)))))
    ;; check if this operation takes places in the same route and
    ;; afect the current position
    (if (or (not (= route (first coordinate)))
	    (> (second coordinate) position))
	0
	subroute-len)))

(defmethod compute-delta-position ((symbol (eql 'g))
				   coordinate
				   route position)
  ;; ('g id-select-subroute1-operation id-select-subroute2-operation)
  ;; check if any of this subroutes belong to the route and, if their
  ;; current positions affect the one that we are tracing
  (declare (special defined-ops-map
		    subroutes-current-position))
  (let* ((coord1 (nth (first coordinate) (gethash 'e defined-ops-map)))
	 (subroute1-cur-pos (getf subroutes-current-position (first coordinate)))
	 (coord2 (nth (second coordinate) (gethash 'e defined-ops-map)))
	 (subroute2-cur-pos (getf subroutes-current-position (second coordinate)))
	 (delta-pos 0))
    ;; here we check if the first client belong to route and, if its current
    ;; position affects position
    (when (and (= route (first coord1))
	       (<= subroute1-cur-pos position))
      ;; insert the second subroute here
      (incf delta-pos (third coord2)))
    ;; here we check if the second client belong to route and, if its current
    ;; position affects position
    (when (and (= route (first coord2))
	       (<= subroute2-cur-pos position))
      ;; insert the first subroute here
      (incf delta-pos (third coord1)))
    ;; here we return delta position
    delta-pos))

(defmethod compute-delta-position ((symbol (eql 'h))
				   coordinate
				   route position)
  ;; ('h id-select-subroute1-operation id-select-subroute2-operation)
  0)

(defun compute-current-position-for (coordinate performed-coordinates)
  (declare (special defined-ops-map))
  (let* ((symbol (if (eql (first coordinate) 'c)
		     'a 'e))
	 (cur-route (first (nth (second coordinate) (gethash symbol defined-ops-map))))
	 (clients-to-trace (make-hash-table))
	 clients-current-position
	 (subroutes-to-trace (make-hash-table))
	 subroutes-current-position)
    (declare (special clients-current-position
		      subroutes-current-position))
    ;; here we collect the clients or subroutes that have
    ;; to be traced
    (dolist (coord performed-coordinates)
      (cond
	((eql (first coord) 'c)
	 ;; then check if these clients have to be traced
	 (when (= cur-route
		  (first (nth (second coord)
			      (gethash 'a defined-ops-map))))
	   (setf (gethash (second coord) clients-to-trace) t))
	 (when (= cur-route
		  (first (nth (third coord)
			      (gethash 'a defined-ops-map))))
	   (setf (gethash (third coord) clients-to-trace) t)))
	((eql (first coord) 'g)
	 ;; then check if these subroutes have to be traced
	 (when (= cur-route
		  (first (nth (second coord)
			      (gethash 'e defined-ops-map))))
	   (setf (gethash (second coord) subroutes-to-trace) t))
	 (when (= cur-route
		  (first (nth (third coord)
			      (gethash 'e defined-ops-map))))
	   (setf (gethash (third coord) subroutes-to-trace) t)))
	))
    ;; given select operation has to be traced always
    (cond
      ((eql symbol 'a)
       (setf (gethash (second coordinate) clients-to-trace) t)
       (setf (gethash (third coordinate) clients-to-trace) t))      
      ((eql symbol 'e)
       (setf (gethash (second coordinate) subroutes-to-trace) t)
       (setf (gethash (third coordinate) subroutes-to-trace) t)))

    ;; iterate over the coordinates and update positions that should be traced
    (dolist (coord (reverse performed-coordinates))
      ;; update position for every client that has to be traced
      (do ((i 0 (+ i 2)))
	  ((>= i (/ (length clients-current-position) 2)))
	(incf (nth (1+ i) clients-current-position)
	      (compute-delta-position (first coord)
				      (rest coord)
				      cur-route
				      (nth (1+ i) clients-current-position))))
      ;; update position for every subroute that has to be traced
      (do ((i 0 (+ i 2)))
	  ((>= i (/ (length subroutes-current-position) 2)))
	(incf (nth (1+ i) subroutes-current-position)
	      (compute-delta-position (first coord)
				      (rest coord)
				      cur-route
				      (nth (1+ i) subroutes-current-position))))      
      ;; check if the current select operation has to be traced
      (cond
	((and (eql (first coord) 'a)
	      (gethash (fourth coord) clients-to-trace))
	 ;; here we start tracing this client
	 (push (third coord) clients-current-position)
	 (push (fourth coord) clients-current-position))
	((and (eql (first coord) 'e)
	      (gethash (fifth coord) subroutes-to-trace))
	 ;; here we start tracing this subroute
	 (push (third coord) subroutes-current-position)
	 (push (fifth coord) subroutes-current-position)))
      )
    ;; here we return the current position of the traced selection
    (cond
      ((eql symbol 'a)
       (values (getf clients-current-position (second coordinate))
	       (getf clients-current-position (third coordinate))))
      ((eql symbol 'e)
       (values (getf subroutes-current-position (second coordinate))
	       (getf subroutes-current-position (third coordinate)))))))

(defun select-op1-was-before-op2 (coordinate performed-coordinates)
  (let* ((symbol (if (eql (first coordinate) 'c)
		     'a 'e))
	 first-id
	 first-pos
	 second-id
	 second-pos
	 coordinate-list
	 (i 0))
    ;; get the neccessary sublist
    (loop for coord in (reverse performed-coordinates)
       doing
	 (progn
	   (when (and (eql (first coord) symbol)
		      (or (= (second coordinate) (first (last coord)))
			  (= (third coordinate) (first (last coord)))))
	     (if (null first-id)
		 (setf first-id (first (last coord)))
		 (progn
		   (setf second-id (first (last coord))
			 second-pos (third coord))		   
		   (return))))
	   (incf i)))
    ;; build the coordinate list
    (setf coordinate-list (slice (reverse performed-coordinates)
				 1 i))
    ;; lets compute current position for the first selection
    (setf first-pos 
	  (compute-current-position-for (list (first coordinate) first-id second-id)
					(reverse coordinate-list)))
    (if (<= first-pos second-pos)
	;; this means that the first selected operation was first
	(if (eql first-id (second coordinate))
	    t nil)
	;; else
	(if (eql first-id (third coordinate))
	    t nil))))

(defmethod from-coordinate-to-operation ((symbol (eql 'r))
					 coordinate)
  ;; ('r route-number)
  nil)

(defmethod from-coordinate-to-operation ((symbol (eql 'a))
					 coordinate)
  ;; ('a route-number client-position id-operation)
  (let (operations)
    ;; here we add the operation
    (push (op-select-client (first coordinate)
			    (second coordinate)
			    (third coordinate))
	  operations)))

(defmethod from-coordinate-to-operation ((symbol (eql 'b))
					 coordinate)
  ;; ('b route-number insertion-position id-select-client-operation)
  (let (operations)
    ;; here we add the operation
    (push (op-insert-client (first coordinate)
			    (second coordinate)
			    (third coordinate))
	  operations)))

(defmethod from-coordinate-to-operation ((symbol (eql 'c))
					 coordinate)
  ;; ('c id-select-client1-operation id-select-client2-operation)
  (declare (special defined-ops-map
		    executed-coordinates))
  ;; we first get the two select client operations and,
  ;; the current clients positions
  (let* ((coord-op1 (nth (first coordinate) (gethash 'a defined-ops-map)))
	 (coord-op2 (nth (second coordinate) (gethash 'a defined-ops-map))))
    (multiple-value-bind (client1-pos client2-pos)
	(compute-current-position-for (cons symbol coordinate) executed-coordinates)
      (cond
	(;; here we check if routes are different
	 ;; because in this case the order doesn't
	 ;; matter
       (not (= (first coord-op1) (first coord-op2)))
	 (list
	  ;; here we insert the first client where client2 was
	  (op-insert-client (first coord-op2)
			    client2-pos
			    (first coordinate))
	  ;; here we insert the second client where client1 was
	  (op-insert-client (first coord-op1)
			    client1-pos
			    (second coordinate))))
	(;; from this point on clients belong to the same route
	 ;; when client 1 has a lower position than client 2
	 (< client1-pos client2-pos)
	 (list
	  ;; here we insert the first client where client2 was
	  (op-insert-client (first coord-op2)
			    client2-pos
			    (first coordinate))
	  ;; here we insert the second client where client1 was
	  (op-insert-client (first coord-op1)
			    client1-pos
			    (second coordinate))))
	(;; from this point on clients belong to the same route
	 ;; when client 2 has a lower position than client 1
	 (< client2-pos client1-pos)
	 (list
	  ;; here we insert the second client where client1 was
	  (op-insert-client (first coord-op1)
			    client1-pos
			    (second coordinate))
	  ;; here we insert the first client where client2 was
	  (op-insert-client (first coord-op2)
			    client2-pos
			    (first coordinate))))
	(;; from this point on clients belong to the same route
	 ;; with the same position
	 (= client1-pos client2-pos)
	 (if (select-op1-was-before-op2 (list 'c (first coordinate) (second coordinate))
					executed-coordinates)
	     ;; when client 1 was selected before client 2
	     (list
	      ;; here we insert the first client
	      (op-insert-client (first coord-op1)
				client1-pos
				(first coordinate))
	      ;; here we insert the second client
	      (op-insert-client (first coord-op2)
				client2-pos
				(second coordinate)))
	     ;; when client 2 was selected before client 1
	     (list
	      ;; here we insert the second client
	      (op-insert-client (first coord-op2)
				client2-pos
				(second coordinate))
	      ;; here we insert the first client
	      (op-insert-client (first coord-op1)
				client1-pos
				(first coordinate))))))
      )))

(defmethod from-coordinate-to-operation ((symbol (eql 'e))
					 coordinate)
  ;; ('e route-number start-position length id-operation)
  (let (operations)	
    (declare (special defined-ops-map))
    ;; here we construct the set of select clients operations
    (let* ((len (third coordinate))
	   (op-id (length (gethash 'a defined-ops-map)))
	   (op nil))
      ;; here we store the id for these select client operations
      (setf (fourth (nth (fourth coordinate) (gethash 'e defined-ops-map))) op-id)
      (loop for i from 0 to (1- len)
	 doing
	   (progn
	     ;; here we define the current operation
	     (setf op (op-select-client (first coordinate)
					(second coordinate)
					(+ op-id i)))
	     (push op operations)
	     ;; here we add the coordinate
	     (setf (gethash 'a defined-ops-map) (reverse
						 (push op
						       (gethash 'a defined-ops-map))))))
      (reverse operations))))

(defmethod from-coordinate-to-operation ((symbol (eql 'f))
					 coordinate)
  ;; ('f route-number insertion-position id-select-subroute-operation)
  (let (operations)
    (declare (special defined-ops-map
		      reversed-subroutes))
    ;; here we construct the set of insert client operations
    (let* ((len (third (nth (third coordinate) (gethash 'e defined-ops-map))))
	   (selc-id (fourth (nth (third coordinate) (gethash 'e defined-ops-map))))
	   (factor (if (gethash (third coordinate) reversed-subroutes) 0 1))
	   (op nil))
      (loop for i from 0 to (1- len)
	 doing
	   (progn
	     ;; here we define the current operation
	     (setf op (op-insert-client (first coordinate)
					(+ (second coordinate) (* i factor))
					(+ selc-id i)))
	     (push op operations)
	     ;; here we add the coordinate
	     (setf (gethash 'b defined-ops-map) (reverse
						 (push op
						       (gethash 'b defined-ops-map))))))
      (reverse operations))))

(defmethod from-coordinate-to-operation ((symbol (eql 'g))
					 coordinate)
  ;; ('g id-select-subroute1-operation id-select-subroute2-operation)
  (declare (special defined-ops-map
		    reversed-subroutes
		    executed-coordinates))
  ;; we first get the two select subroute operations and,
  ;; the current subroutes positions
  (let* ((coord-op1 (nth (first coordinate) (gethash 'e defined-ops-map)))
	 (coord-op2 (nth (second coordinate) (gethash 'e defined-ops-map)))
	 first-subroute-insertion
	 second-subroute-insertion)
    (multiple-value-bind (subroute1-pos subroute2-pos)
	(compute-current-position-for (cons symbol coordinate) executed-coordinates)
      (cond
	(;; here we check if routes are different
	 ;; because in this case the order doesn't
	 ;; matter
	 (/= (first coord-op1) (first coord-op2))
	 (setf first-subroute-insertion (from-coordinate-to-operation 'f
								      `(,(first coord-op2)
									 ,subroute2-pos
									 ,(first coordinate)))
	       second-subroute-insertion (from-coordinate-to-operation 'f
								       `(,(first coord-op1)
									  ,subroute1-pos
									  ,(second coordinate)))))
	(;; from this point on subroutes belong to the same route
	 ;; when subroute 1 has a lower position than subroute 2
	 (< subroute1-pos subroute2-pos)
	 ;; here we insert the first subroute where the second was and,
	 (setf first-subroute-insertion (from-coordinate-to-operation 'f
								      `(,(first coord-op2)
									 ,subroute2-pos
									 ,(first coordinate)))
	       ;; here we insert the second subroute where the first was
	       second-subroute-insertion (from-coordinate-to-operation 'f
								       `(,(first coord-op1)
									  ,subroute1-pos
									  ,(second coordinate)))))
	(;; from this point on clients belong to the same route
	 ;; when subroute 2 has a lower position than subroute 1
	 (< subroute2-pos subroute1-pos)
	 ;; here we insert the second subroute where the first was and,
	 (setf first-subroute-insertion (from-coordinate-to-operation 'f
								      `(,(first coord-op1)
									 ,subroute1-pos
									 ,(second coordinate)))
	     ;; here we insert the first subroute where the second was
	       second-subroute-insertion (from-coordinate-to-operation 'f
								       `(,(first coord-op2)
									  ,subroute2-pos
									  ,(first coordinate)))))
	(;; from this point on clients belong to the same route
	 ;; with the same position
	 (= subroute1-pos subroute2-pos)	 
	 (if (select-op1-was-before-op2 (list 'g (first coordinate) (second coordinate))
					executed-coordinates)
	     ;; when subroute 1 was selected before subroute 2
	     (setf first-subroute-insertion (from-coordinate-to-operation 'f
									  `(,(first coord-op1)
									     ,subroute1-pos
									     ,(first coordinate)))
		   second-subroute-insertion (from-coordinate-to-operation 'f
									   `(,(first coord-op2)
									      ,subroute2-pos
									      ,(second coordinate))))
	     ;; when subroute 2 was selected before subroute 1
	     (setf first-subroute-insertion (from-coordinate-to-operation 'f
									  `(,(first coord-op2)
									     ,subroute2-pos
									     ,(second coordinate)))
		   second-subroute-insertion (from-coordinate-to-operation 'f
									   `(,(first coord-op1)
									      ,subroute1-pos
									      ,(first coordinate)))))))	 
      (append first-subroute-insertion
	      second-subroute-insertion))))

(defmethod from-coordinate-to-operation ((symbol (eql 'h))
					 coordinate)
  ;; ('h id-select-subroute-operation)
  (let (operations)
    (declare (special reversed-subroutes))
    ;; mark the subroute as reversed
    (setf (gethash (first coordinate) reversed-subroutes) t)
    ;; return an empty operation list
    operations))

(defun from-coordinates-to-operations (coordinate-list)
  (let ((defined-ops-map (make-hash-table))
	(reversed-subroutes (make-hash-table))
	executed-coordinates
	ops-list
	symbol-list)
    (declare (special defined-ops-map
		      reversed-subroutes
		      executed-coordinates))    
    ;; store the coordinates for future reference
    (loop for coord in coordinate-list
       doing
	 (when (null (gethash (first coord) defined-ops-map))
	   (push (first coord) symbol-list))
	 (push (clone (rest coord)) (gethash (first coord) defined-ops-map)))
    ;; reverse every list of defined ops to store it in the
    ;; rigth order
    (loop for symbol in symbol-list
       doing
	 (setf (gethash symbol defined-ops-map)
	       (reverse (gethash symbol defined-ops-map))))
    ;; collect the operations
    (loop for coord in coordinate-list
       doing
	 (setf ops-list (append ops-list
				(from-coordinate-to-operation (first coord)
							      (rest coord))))
	 (push coord executed-coordinates))
    ops-list))
