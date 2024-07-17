(in-package :vrp)

(defmethod count-neighbors ((op counting-op-init)
			    (sol counting-basic-solution)
			    other-ops default)
  (let ((tree-root-node (make-instance 'root-node))
	(child (count-neighbors (first other-ops)
				sol
				(rest other-ops) 0)))
    ;; here we check if there is any neighbor solution
    (when (> (total child) 0)
      (setf (child tree-root-node) child
	    (total tree-root-node) (total child)
	    ;; here we set parent for child node
	    (parent child) tree-root-node))
    ;; and finally return root-node
    tree-root-node))

(defmethod count-neighbors ((op counting-op-select-route)
			    (sol counting-basic-solution)
			    other-ops default)
  (let* ((result 0)
	 (child nil)
	 (route-list (routes sol))
	 (i 1)
	 ;; first let's build the tree node
	 (tree-r-node (make-instance 'r-node
				     :node-id (operation-id op))))
    (loop for item in route-list
       do (progn
	    ;; here we bound the route number
	    (setf (get (route-symbol op) 'number) i)
	    ;; here we compute the number of neighbors for this route
	    (setf child (count-neighbors (first other-ops)
					 sol
					 (rest other-ops) 0))
	    ;; here we check if this subtree produces any solutions
	    (when (> (total child) 0)
	      ;; here we set the route number that correspond with this child node
	      (setf (at-parent child) (1+ (length (children tree-r-node)))
		    ;; here we set parent for child node
		    (parent child) tree-r-node)
	      ;; here we add the child to the children list of the r-node
	      (push (cons child i) (children tree-r-node))
	      ;; update result
	      (setf result (+ result (total child))))
	    ;; update the number of route
	    (incf i)))
    ;; here we put children list into the right order
    (setf (children tree-r-node) (reverse (children tree-r-node)))
    (setf (total tree-r-node) result)
    ;; and finally return r-node
    tree-r-node))

(defmethod count-neighbors ((op counting-op-select-client)
			    (sol counting-basic-solution)
			    other-ops default)
  (let* ((result 0)
	 (child nil)
	 ;; here we get the index of the current route
	 (current-route (get (route-symbol op) 'number))
	 ;; first let's build the tree node
	 (tree-a-node (make-instance 'a-node
				     :node-id (operation-id op)
				     :route-number current-route)))
    ;; first let's get the number of clients in the route
    (setf result (nth (1- current-route) (routes sol)))
    ;; here we check if there is any possibility at this node
    (when (> result 0)
      ;; here we set the number of possibilities at this node
      (setf (possibilities tree-a-node) result)

      ;; now let's save the select operation id for this client
      ;; and the symbol of its route
      (setf (get (client-symbol op) 'select-op-id) (operation-id op)
	    (get (client-symbol op) 'route-symbol) (route-symbol op))

      ;; now let's decrement the length of that route
      (decf (nth (1- current-route) (routes sol)))
      ;; here we should compute everything else
      (setf child (count-neighbors (first other-ops)
				   sol
				   (rest other-ops) 1))
      ;; here we check if this subtree produces any solutions
      (when (> (total child) 0)
	;; here we set the child to the current tree node
	(setf (child tree-a-node) child
	      ;; here we set parent for child node
	      (parent child) tree-a-node))

      ;; update result
      (setf result (* result (total child)))
      ;; and we should fix what we broke in the solution
      (incf (nth (1- current-route) (routes sol))))

    ;; here we set the total amount of neighbors a this node
    (setf (total tree-a-node) result)
    ;; and finally return a-node
    tree-a-node))

(defmethod count-neighbors ((op counting-op-insert-client)
			    (sol counting-basic-solution)
			    other-ops default)
  (let* ((result 0)
	 (child nil)
	 (current-route (get (route-symbol op) 'number))
	 ;; first let's build the tree node
	 (tree-b-node (make-instance 'b-node
				     :node-id (operation-id op)
				     :route-number current-route
				     :select-op-id (get (client-symbol op) 'select-op-id))))
    ;; first let's get the number of available posititions for inserting the client in the current route
    (setf result (1+ (nth (1- current-route) (routes sol))))
    ;; here we set the number of possibilities at this node
    (setf (possibilities tree-b-node) result)

    ;; now let's increment the length of the current route
    (incf (nth (1- current-route) (routes sol)))
    ;; here we should compute everything else
    (setf child (count-neighbors (first other-ops)
				 sol
				 (rest other-ops) 1))

    ;; here we check if this subtree produces any solutions
    (when (> (total child) 0)
      ;; here we set the child to the current tree node
      (setf (child tree-b-node) child
	    ;; here we set parent for child node
	    (parent child) tree-b-node))

    ;; update result
    (setf result (* result (total child)))
    ;; and we should fix what we broke in the solution
    (decf (nth (1- current-route) (routes sol)))

    ;; here we set the total amount of neighbors at this node
    (setf (total tree-b-node) result)
    ;; and finally return b-node
    tree-b-node))

(defmethod count-neighbors ((op counting-op-swap-clients)
			    (sol counting-basic-solution)
			    other-ops default)
  (let* ((child nil)
	 ;; first let's get the number of each route
	 (current-route-1 (get (get (client-1-symbol op) 'route-symbol) 'number))
	 (current-route-2 (get (get (client-2-symbol op) 'route-symbol) 'number))
	 ;; then let's build the tree node
	 (tree-c-node (make-instance 'c-node
				     :node-id (operation-id op)
				     :select-op1-id (get (client-1-symbol op) 'select-op-id)
				     :select-op2-id (get (client-2-symbol op) 'select-op-id))))
    ;; now let's increment the length of each route
    (incf (nth (1- current-route-1) (routes sol)))
    (incf (nth (1- current-route-2) (routes sol)))
    ;; here we should compute everything else
    (setf child (count-neighbors (first other-ops)
				 sol
				 (rest other-ops) 1))

    ;; here we check if this subtree produces any solutions
    (when (> (total child) 0)
      ;; here we set the child to the current tree node
      (setf (child tree-c-node) child
	    ;; here we set parent for child node
	    (parent child) tree-c-node
	    ;; here we set the total amount of neighbors at this node
	    (total tree-c-node) (total child)))

    ;; and we should fix what we broke in the solution
    (decf (nth (1- current-route-1) (routes sol)))
    (decf (nth (1- current-route-2) (routes sol)))

    ;; and finally return c-node
    tree-c-node))

(defmethod count-neighbors ((op counting-op-select-subroute)
			    (sol counting-basic-solution)
			    other-ops default)
  (let* ((result 0)
	 (child nil)
	 ;; here we get the index of the current route
	 (current-route (get (route-symbol op) 'number))
	 ;; here we get the current length of the route
	 (current-length (nth (1- current-route) (routes sol)))
	 ;; here we compute the maximum length of subroute that we can choose
	 (max-len (- current-length
		     *upper-bound-for-subroute-length*))
	 ;; here we build the current tree node
	 (tree-e-node (make-instance 'e-node
				       :node-id (operation-id op)
				       :route-number current-route)))
    ;; now let's save the select operation id for this subroute
    ;; and the symbol of its route
      (setf (get (subroute-symbol op) 'select-op-id) (operation-id op)
	    (get (subroute-symbol op) 'route-symbol) (route-symbol op))

      ;; here we implicity check if there is any possibility at this node
      (loop for k from *lower-bound-for-subroute-length* to max-len
	 do (progn
	      ;; here we bound the subroute length
	      (setf (get (subroute-symbol op) 'length) k)
	      ;; let's decrement the length of the current route by k
	      (setf (nth (1- current-route) (routes sol)) (- current-length k))
	      ;; here we should compute everithing else
	      (setf child (count-neighbors (first other-ops)
					   sol
					   (rest other-ops) 0))

	      ;; here we check if this subtree produces any solutions
	      (when (> (total child) 0)
		(setf (at-parent child) (1+ (length (children tree-e-node)))
		      (parent child) tree-e-node)
		;; here we add the child to the children list of the e-node and (subroute-length . positions)
		(push (cons child (cons (cons k (1+ (- current-length k))) nil))
		      (children tree-e-node)))

	      ;; update result
	      (setf result (+ result
			      (* (total child) (1+ (- current-length k)))))
	      ;; here we should fix what we broke in the solution
	      (setf (nth (1- current-route) (routes sol))
		    current-length)))

      ;; here we put children list into the right order
      (setf (children tree-e-node) (reverse (children tree-e-node)))
      ;; here we set the total amount of neighbors at this node
      (setf (total tree-e-node) result)
      ;; and finally return e-node
      tree-e-node))

(defmethod count-neighbors ((op counting-op-insert-subroute)
			    (sol counting-basic-solution)
			    other-ops default)
  (let* ((result 0)
	 (child nil)
	 (current-route (get (route-symbol op) 'number))
	 (current-subroute-length (get (subroute-symbol op) 'length))
	 (current-route-length (nth (1- current-route) (routes sol)))
	 ;; first let's build the tree node
	 (tree-f-node (make-instance 'f-node
				     :node-id (operation-id op)
				     :route-number current-route
				     :select-op-id (get (subroute-symbol op) 'select-op-id))))
    ;; get the number of available positions for inserting the selected subroute in the current route
    (setf result (1+ current-route-length)
	  ;; here we set the number of possibilities at this node
	  (possibilities tree-f-node) (1+ current-route-length))

    ;; now increment the length of the current route
    (setf (nth (1- current-route) (routes sol)) (+ current-route-length current-subroute-length))
    ;; here we should compute everything else
    (setf child (count-neighbors (first other-ops)
				 sol
				(rest other-ops) 1))

    ;; here we check if this path in the tree produces any solutions
    (when (> (total child) 0)
      ;; here we set the child to the current tree node
     (setf (child tree-f-node) child
	   ;; here we set the parent for child node
	   (parent child) tree-f-node))

    ;; update result
    (setf result (* result (total child)))
    ;; and we should fix what we broke in the solution
    (setf (nth (1- current-route) (routes sol)) current-route-length)

    ;; here we set the total amount of neighbors at this node
    (setf (total tree-f-node) result)
    ;; and finally return f-node
    tree-f-node))

(defmethod count-neighbors ((op counting-op-swap-subroutes)
			    (sol counting-basic-solution)
			    other-ops default)
  (let* ((child nil)
	 ;; first let's get the number of each route
	 (current-route-1 (get (get (subroute-1-symbol op) 'route-symbol) 'number))
	 (current-route-2 (get (get (subroute-2-symbol op) 'route-symbol) 'number))
	 ;; the lets get the subroutes length
	 (current-subroute-1-length (get (subroute-1-symbol op) 'length))
	 (current-subroute-2-length (get (subroute-2-symbol op) 'length))
	 ;; then let's build the tree node
	 (tree-g-node (make-instance 'g-node
				     :node-id (operation-id op)
				     :select-op1-id (get (subroute-1-symbol op) 'select-op-id)
				     :select-op2-id (get (subroute-2-symbol op) 'select-op-id))))
    ;; now let's increment the length of each route
    (incf (nth (1- current-route-1) (routes sol)) current-subroute-2-length)
    (incf (nth (1- current-route-2) (routes sol)) current-subroute-1-length)

    ;; here we should compute everything else
    (setf child (count-neighbors (first other-ops)
				 sol
				 (rest other-ops) 1))

    ;; here we check if this subtree produces any solutions
    (when (> (total child) 0)
      ;; here we set the child to the current tree node
      (setf (child tree-g-node) child
	    ;; here we set parent for child node
	    (parent child) tree-g-node
	    ;; here we set the total amount of neighbors at this node
	    (total tree-g-node) (total child)))

    ;; and we should fix what we broke in the solution
    (decf (nth (1- current-route-1) (routes sol)) current-subroute-2-length)
    (decf (nth (1- current-route-2) (routes sol)) current-subroute-1-length)

    ;; and finally return c-node
    tree-g-node))

(defmethod count-neighbors ((op counting-op-reverse-subroute)
			    (sol counting-basic-solution)
			    other-ops default)
  (let* ((child nil)
	 ;; first let's build the tree node
	 (tree-h-node (make-instance 'h-node
				     :node-id (operation-id op)
				     :select-op-id (get (subroute-symbol op) 'select-op-id))))

    ;; here we should compute everything else
    (setf child (count-neighbors (first other-ops)
				 sol
				 (rest other-ops) 1))

    ;; here we check if this path in the tree produces any solutions
    (when (> (total child) 0)
      ;; here we set the child to the current tree node
     (setf (child tree-h-node) child
	   ;; here we set the parent for child node
	   (parent child) tree-h-node
	   ;; here we set the total amount of neighbors at this node
	   (total tree-h-node) (total child)))

    ;; and finally return f-node
    tree-h-node))

(defmethod count-neighbors ((op (eql nil))
			    (sol counting-basic-solution)
			    other-ops default)
  ;; in this case always return a nil-node which total-count is default
  (let* ((tree-nil-node (make-instance 'nil-node
				       :total-count default)))
    tree-nil-node))

(defmethod count-neighbors ((op counting-op-select-vehicle)
                            (sol counting-hfvrp-solution)
                            other-ops default)
  (let* ((result 0)
         (child nil)
         ;; here we get the index of the curent route
         (current-route (get (route-symbol op) 'number))
         ;; first let's build the tree node
         (tree-v-node (make-instance 'v-node
                                     :node-id (operation-id op)
                                     :route-number current-route)))


      ;; now let's save the select operation id for this 
      ;; and the symbol of its route
      (setf (get (vehicle-symbol op) 'select-op-id) (operation-id op)
            (get (vehicle-symbol op) 'route-symbol) (route-symbol op))


      ;; here we should compute everything else
      (setf child (count-neighbors (first other-ops)
                                   sol
                                   (rest other-ops) 1))
      ;; here we check if this subtree produces any solutions
      (when (> (total child) 0)
        ;; here we set the child to the current tree node
        (setf (child tree-v-node) child
              ;; here we set parent for child node
              (parent child) tree-v-node))

      ;; update result
      (setf result (total child))

    ;; here we set the total amount of neighbors a this node
    (setf (total tree-v-node) result)
    ;; and finally return v-node
    tree-v-node))

(defmethod count-neighbors ((op counting-op-select-vehicle-from-list)
			    (sol counting-hfvrp-solution)
			    other-ops default)
  (let* ((result 0)
	 (child nil)
	
	 (tree-z-node (make-instance 'z-node
				     :node-id (operation-id op)
                     :num-vehicles (num-vehicles sol))))
   
    (setf result (num-vehicles sol))
   
    (when (> result 0)
     
      (setf (possibilities tree-z-node) result)

     
      (setf (get (vehicle-symbol op) 'select-op-id) (operation-id op))

      
      (decf (num-vehicles sol))
      ;; here we should compute everything else
      (setf child (count-neighbors (first other-ops)
				   sol
				   (rest other-ops) 1))
      ;; here we check if this subtree produces any solutions
      (when (> (total child) 0)
	;; here we set the child to the current tree node
	(setf (child tree-z-node) child
	      ;; here we set parent for child node
	      (parent child) tree-z-node))

      ;; update result
      (setf result (* result (total child)))
      ;; and we should fix what we broke in the solution
      (incf (num-vehicles sol)))

    ;; here we set the total amount of neighbors a this node
   
    (setf (total tree-z-node) result)
    ;; and finally return x-node
    tree-z-node))

(defmethod count-neighbors ((op counting-op-swap-vehicles)
                            (sol counting-basic-solution)
                            other-ops default)
  (let* ((child nil)
         ;; then let's build the tree node
         (tree-t-node (make-instance 't-node
                                     :node-id (operation-id op)
                                     :select-op1-id (get (vehicle-1-symbol op) 'select-op-id)
                                     :select-op2-id (get (vehicle-2-symbol op) 'select-op-id))))

    ;; here we should compute everything else
    (setf child (count-neighbors (first other-ops)
                                 sol
                                 (rest other-ops) 1))

    ;; here we check if this subtree produces any solutions
    (when (> (total child) 0)
      ;; here we set the child to the current tree node
      (setf (child tree-t-node) child
            ;; here we set parent for child node
            (parent child) tree-t-node
            ;; here we set the total amount of neighbors at this node
            (total tree-t-node) (total child)))


    ;; and finally return t-node
    tree-t-node))

(defmethod count-neighbors ((op counting-op-select-depot)
                            (sol counting-basic-solution)
                            other-ops default)
  (let* ((result 0)
         (child nil)
         ;; here we get the index of the curent route
         (current-route (get (route-symbol op) 'number))
         ;; first let's build the tree node
         (tree-d-node (make-instance 'd-node
                                     :node-id (operation-id op)
                                     :route-number current-route)))


      ;; now let's save the select operation id for this 
      ;; and the symbol of its route
      (setf (get (depot-symbol op) 'select-op-id) (operation-id op)
            (get (depot-symbol op) 'route-symbol) (route-symbol op))


      ;; Here we extract the depot from the route
      ; Code here 

      ;; here we should compute everything else
      (setf child (count-neighbors (first other-ops)
                                   sol
                                   (rest other-ops) 1))
      ;; here we check if this subtree produces any solutions
      (when (> (total child) 0)
        ;; here we set the child to the current tree node
        (setf (child tree-d-node) child
              ;; here we set parent for child node
              (parent child) tree-d-node))

      ;; update result
      (setf result (total child))

    ;; here we set the total amount of neighbors a this node
    ;(format t "El valor de result es ~a" result)
    (setf (total tree-d-node) result)
    ;; and finally return d-node
    tree-d-node))

(defmethod count-neighbors ((op counting-op-select-non-selected-depot)
			    (sol counting-mdvrp-solution)
			    other-ops default)
  (let* ((result 0)
	 (child nil)
	
	 (tree-x-node (make-instance 'x-node
				     :node-id (operation-id op)
                     :num-depots (num-depots sol))))
   
    (setf result (num-depots sol))
   
    (when (> result 0)
     
      (setf (possibilities tree-x-node) result)

     
      (setf (get (depot-symbol op) 'select-op-id) (operation-id op))

      
      (decf (num-depots sol))
      ;; here we should compute everything else
      (setf child (count-neighbors (first other-ops)
				   sol
				   (rest other-ops) 1))
      ;; here we check if this subtree produces any solutions
      (when (> (total child) 0)
	;; here we set the child to the current tree node
	(setf (child tree-x-node) child
	      ;; here we set parent for child node
	      (parent child) tree-x-node))

      ;; update result
      (setf result (* result (total child)))
      ;; and we should fix what we broke in the solution
      (incf (num-depots sol)))

    ;; here we set the total amount of neighbors a this node
   
    (setf (total tree-x-node) result)
    ;; and finally return x-node
    tree-x-node))

(defmethod count-neighbors ((op counting-op-swap-depots)
                            (sol counting-mdvrp-solution)
                            other-ops default)
  (let* ((child nil)
         ;; then let's build the tree node
         (tree-s-node (make-instance 's-node
                                     :node-id (operation-id op)
                                     :select-op1-id (get (depot-1-symbol op) 'select-op-id)
                                     :select-op2-id (get (depot-2-symbol op) 'select-op-id))))

    ;; here we should compute everything else
    (setf child (count-neighbors (first other-ops)
                                 sol
                                 (rest other-ops) 1))

    ;; here we check if this subtree produces any solutions
    (when (> (total child) 0)
      ;; here we set the child to the current tree node
      (setf (child tree-s-node) child
            ;; here we set parent for child node
            (parent child) tree-s-node
            ;; here we set the total amount of neighbors at this node
            (total tree-s-node) (total child)))


    ;; and finally return s-node
    tree-s-node))
    
(defun build-neighborhood-tree (code sol &optional probl)
  (let* ((operations (create-operation-instances-from code))
	 (count-solution (transform-to-counting-solution sol))
	 (tree (count-neighbors (first operations)
				count-solution
				(rest operations)
				0)))
    (setf *from-index-to-coords-indexer-function* 'index-to-coords-classic-indexer
	  *from-coords-to-indexer-indexer-function* 'coords-to-index-classic-indexer)
    (make-instance 'neighborhood-tree
		   :problem probl
		   :solution sol
		   :criterion operations
		   :counting-solution count-solution
		   :tree tree
		   :cardinality (total tree))))
