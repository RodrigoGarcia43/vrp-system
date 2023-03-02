(in-package :vrp)

(defun random-from-range (start end)
  (+ start (random (1+ (- end start)))))

(defun random-sample-from-range (start end &optional mapper position)
  (let ((mapp (if (null mapper) (make-hash-table) mapper))
	(index 0)
	(pos (if (null position) 0 position))
	(total (1+ (- end start))))
    #'(lambda ()
	(if (>= pos total)
	    nil
	    (progn
	      ;; get a random-number in the interval [(start+pos);end]
	      (setf index (random-from-range (+ start pos) end))
	      ;; get the content in the index-th position of the list
	      (multiple-value-bind (content in-use) (gethash index mapp)
		;; get the content in the actual[(start+pos)-th] position of the list
		(multiple-value-bind (pos-content pos-in-use) (gethash (+ start pos) mapp)
		  ;; if the actual position is in use:
		  (if pos-in-use
		      ;; then we have to set the index-th content equal to the content of the actual position
		      (setf (gethash index mapp) pos-content)
		      ;; else we have to set the index-th conent equal to (start+pos)
		      (setf (gethash index mapp) (+ start pos))))
		;; here lets increment the position
		(incf pos)
		;; return the next number
		(if in-use
		    content
		    index)))))))

(defun uniform-generator-from-list (elements-list)
  (let ((generator (random-sample-from-range 1 (length elements-list)))
	(value nil))
    #'(lambda ()
	(setf value (funcall generator))
	(if (null value)
	    nil
	    (nth (1- value) elements-list)))))

(defmethod exhaustive-search ((obj neighborhood-tree))
  (let* ((mapper (solution-mapper (search-state obj)))
	 (first-index 1)
	 (last-index (cardinality obj))
	 (index (+ first-index (number-of-analysed-solutions (search-state obj)))))

    #'(lambda ()
	(if (> index last-index)
	    nil
	    ;; get the content in the index-th position of the list
	    (multiple-value-bind (content in-use) (gethash index mapper)
	      (incf (number-of-analysed-solutions (search-state obj)))
	      (if in-use
		  ;; then we are going to return the content-th solution
		  (prog1
		      (from-index-to-coord-list obj content)
		    (incf index))
		  ;; else we are going to return the index-th solution
		  (prog1
		      (from-index-to-coord-list obj index)
		    (incf index))))))))

(defmethod exhaustive-search ((obj neighborhood-region))
  (let* ((mapper (solution-mapper (search-state obj)))
	 (first-index (car (index-range obj)))
	 (last-index (cdr (index-range obj)))
	 (index (+ first-index (number-of-analysed-solutions (search-state obj)))))

    #'(lambda ()
	(if (> index last-index)
	    nil
	    ;; get the content in the index-th position of the list
	    (multiple-value-bind (content in-use) (gethash index mapper)
	      (incf (number-of-analysed-solutions (search-state obj)))
	      (if in-use
		  ;; then we are going to return the content-th solution
		  (prog1
		      (from-index-to-coord-list (neighborhood obj) content)
		    (incf index))
		  ;; else we are going to return the index-th solution
		  (prog1
		      (from-index-to-coord-list (neighborhood obj) index)
		    (incf index))))))))

(defmethod random-search ((obj neighborhood-tree) n)
       (let* ((mapper (solution-mapper (search-state obj)))
	      (count 1)
	      (index (+ 1 (random (cardinality obj)))))

	 #'(lambda ()
	     (if (> count n)
		 nil
		 ;; get the content in the index-th position of the list
		 (multiple-value-bind (content in-use) (gethash index mapper)
		   (incf (number-of-analysed-solutions (search-state obj)))
		   (if in-use
		       ;; then we are going to return the content-th solution
		       (prog1
			   (from-index-to-coord-list obj content)
			 (setf index (+ 1 (random (cardinality obj))))
			 (incf count))
		       ;; else we are going to return the index-th solution
		       (prog1
			   (from-index-to-coord-list obj index)
			 (setf index (+ 1 (random (cardinality obj))))
			 (incf count))))))))

;;     (defmethod exhaustive-search ((obj neighborhood-region))
;;       (let* ((mapper (solution-mapper (search-state obj)))
;;	      (first-index (car (index-range obj)))
;;	      (last-index (cdr (index-range obj)))
;;	      (index (+ first-index (number-of-analysed-solutions (search-state obj)))))
;;
;;	 #'(lambda ()
;;	     (if (> index last-index)
;;		 nil
;;		 ;; get the content in the index-th position of the list
;;		 (multiple-value-bind (content in-use) (gethash index mapper)
;;		   (incf (number-of-analysed-solutions (search-state obj)))
;;		   (if in-use
;;		       ;; then we are going to return the content-th solution
;;		       (prog1
;;			   (from-index-to-coord-list (neighborhood obj) content)
;;			 (incf index))
;;		       ;; else we are going to return the index-th solution
;;		       (prog1
;;			   (from-index-to-coord-list (neighborhood obj) index)
;;			 (incf index))))))))

(defmethod uniform-search ((obj neighborhood-tree))
  (let ((uniform-number-iterator (random-sample-from-range 1
                                                           (cardinality obj)
                                                           (solution-mapper (search-state obj))
                                                           (number-of-analysed-solutions (search-state obj)))))
    #'(lambda ()
        (let ((index (funcall uniform-number-iterator)))
          (if (null index)
              nil
              (progn
                (incf (number-of-analysed-solutions (search-state obj)))
                (from-index-to-coord-list obj index)))))))

(defmethod uniform-search ((obj neighborhood-region))
  (let ((uniform-number-iterator (random-sample-from-range (car (index-range obj))
                                                           (cdr (index-range obj))
                                                           (solution-mapper (search-state obj))
                                                           (number-of-analysed-solutions (search-state obj)))))
    #'(lambda ()
        (let ((index (funcall uniform-number-iterator)))
          (if (null index)
              nil
              (progn
                (incf (number-of-analysed-solutions (search-state obj)))
                (from-index-to-coord-list (neighborhood obj) index)))))))

(defmethod branch-cardinality ((tree root-node)
			       ops index branch)
  (if (null (child tree))
      ;; it means that there isn't any branch in the tree
      (values tree nil 1 nil)
      (if (null index)
	  ;; it means that we are going down in the tree so we continue traversing for its child
	  (branch-cardinality (child tree) (cons '* ops) nil branch)
	  ;; it means that we have processed all branches in the tree
	  (values tree nil 1 nil nil))))

(defmethod branch-cardinality ((tree r-node)
			       ops index branch)
  (if (null index)
      ;; it means that we are going down in the tree so we continue traversing for the first child
      (branch-cardinality (car (first (children tree)))
			  ops nil (cons (cons 'r 1) branch))
      ;; it means that we are going up in the tree so we continue traversing for the next child, 
      ;; when we reach the last one we return to this node's parent
      (if (< index (length (children tree)))
	  (branch-cardinality (car (nth index (children tree)))
			      ops nil (cons (cons 'r (1+ index)) (rest branch)))
	  (branch-cardinality (parent tree)
			      ops (at-parent tree) (rest branch)))))

(defmethod branch-cardinality ((tree a-node)
			       ops index branch)
  (let ((poss (possibilities tree)))
    (if (null index)
	;; it means that we are going down in the tree so we push the number of possibilities on
	;; this node to /ops/ and continue traversing down
	(branch-cardinality (child tree) (cons poss ops) nil branch)
	;; it means that we are going up in the tree so we continue traversing up
	(branch-cardinality (parent tree) (rest ops) (at-parent tree) branch))))

(defmethod branch-cardinality ((tree b-node)
			       ops index branch)
  (let ((poss (possibilities tree)))
    (if (null index)
	;; it means that we are going down in the tree so we push the number of possibilities on this node
	;; to /ops/ and continue traversing down
	(branch-cardinality (child tree) (cons poss ops) nil branch)
	;; it means that we are going up in the tree so we continue traversing up
	(branch-cardinality (parent tree) (rest ops) (at-parent tree) branch))))

(defmethod branch-cardinality ((tree c-node)
			       ops index branch)
  (if (null index)
      ;; it means that we are going down in the tree so we continue going down
      (branch-cardinality (child tree) ops nil branch)
      ;; it means that we are going up in the tree so we continue to node's parent
      (branch-cardinality (parent tree) ops (at-parent tree) branch)))

(defmethod branch-cardinality ((tree e-node)
			       ops index branch)
  (if (null index)
      ;; it means that we are going down in the tree so we continue traversing for the first child
      (let ((poss (cdr (second (first (children tree))))))
	(branch-cardinality (car (first (children tree)))
			    (cons poss ops) nil
			    (cons (cons 'e 1) branch)))
      ;; it means that we are going up in the tree so we continue traversing for the next child,
      ;; when we reach the last one we return to this node's parent
      (let ((poss (cdr (second (nth index (children tree))))))
	(if (< index (length (children tree)))
	    (branch-cardinality (car (nth index (children tree)))
				(cons poss (rest ops)) nil
				(cons (cons 'e (1+ index)) (rest branch)))
	    (branch-cardinality (parent tree)
				(rest ops) (at-parent tree)
				(rest branch))))))

(defmethod branch-cardinality ((tree f-node)
			       ops index branch)
  (let ((poss (possibilities tree)))
    (if (null index)
	;; it means that we are going down in the tree so we push the number of possibilities on this node
	;; to /ops/ and continue traversing down
	(branch-cardinality (child tree) (cons poss ops) nil branch)
	;; it means that we are going up in the tree so we continue traversing up
	(branch-cardinality (parent tree) (rest ops) (at-parent tree) branch))))

(defmethod branch-cardinality ((tree g-node)
			       ops index branch)
  (if (null index)
      ;; it means that we are going down in the tree so we continue going down
      (branch-cardinality (child tree) ops nil branch)
      ;; it means that we are going up in the tree so we continue to node's parent
      (branch-cardinality (parent tree) ops (at-parent tree) branch)))

(defmethod branch-cardinality ((tree h-node)
			       ops index branch)
  (if (null index)
      ;; it means that we are going down in the tree so we continue going down
      (branch-cardinality (child tree) ops nil branch)
      ;; it means that we are going up in the tree so we continue to node's parent
      (branch-cardinality (parent tree) ops (at-parent tree) branch)))

(defmethod branch-cardinality ((tree nil-node)
			       ops index branch)
  (if (null index)
      ;; it means that we are going down in the tree an we return the current /operaions list/
      (values tree ops (at-parent tree) branch)
      ;; it means that we are going up in the tree an we return to node's parent
      (branch-cardinality (parent tree) ops (at-parent tree) branch)))

(defun branch-definitions (tree)
  (let ((cur-node tree)
	cur-branch
	cur-ops
	going-up)
    #'(lambda ()
	(setf (values cur-node cur-ops going-up cur-branch)
	      (branch-cardinality cur-node cur-ops going-up cur-branch))
	(if (null cur-ops)
	    nil
	    (let (branch-poss 
		  (acc 1))
	      (loop for i from 0 to (- (length cur-ops) 2)
		 doing
		   (setf acc (* acc (nth i cur-ops)))
		   (push acc branch-poss))
	      (values (eval (reverse cur-ops))
		      (reverse cur-branch)
		      (append branch-poss (list 1))))))))

(defun prepare-neighborhood-for-exploration (neighborhood)
  ;; first create a list of basic region list
  ;; then group all that matches a given function
  ;; third from this last list we create the regions.

  (when (null (region-list neighborhood))

    ;; here we set the correct indexer function for a customize exploration
    (setf *from-index-to-coords-indexer-function* 'index-to-coords-customize-indexer
	  *from-coords-to-indexer-indexer-function* 'coords-to-index-customize-indexer)

    (let* (;; here we create the branch cardinalities generator
	   (region-card-gen (branch-definitions (tree neighborhood)))
	   ;; create a list to store the regions
	   region-list
	   ;; create a list to store the basic regions
	   basic-region-list
	   ;; create a hash to group the basic regions
	   (basic-region-groups (make-hash-table :test #'equal))
	   ;; save the group keys to iterate later
	   groups-keys
	   ;; some data for basic regions and regions
	   (cur-id 1)
	   (cur-index 1)
	   ;; average route length
	   (avg-route (truncate (/ (num-clients (counting-solution neighborhood))
				   (length (routes (counting-solution neighborhood))))))
	   (subr-class-len (length *subroute-length-classifications*))
	   )      
      ;; here we fit the subroute classification parameters for the current problem
      (setf *subroute-length-upper-bounds*
	    (append
	     '(1)
	     (loop for i from 1 to (1- subr-class-len)
		collect
		  (ceiling (* (/ avg-route subr-class-len) i)))))
      ;; here we create the basic regions
      (loop while t
	 doing
	   (multiple-value-bind (cur-branch-card cur-branch-info cur-branch-poss) (funcall region-card-gen)
	     (when (not cur-branch-card)
	       (return))
	     (push (make-instance 'neighborhood-basic-region
				  :info cur-branch-info
				  :subtree-sols cur-branch-poss
				  :cardinality cur-branch-card)
		   basic-region-list)))
      ;; here we group basic regions using some function
      (loop for basic-reg in basic-region-list
	 doing
	   (let (;; here we compute the regions key for this basic region
		 (key (from-branch-info-to-region-tuple (info basic-reg))))
	     ;; if not a hash key yet store it
	     (when (null (gethash key basic-region-groups))
	       (push key groups-keys))
	     ;; add basic region to the current region
	     (push basic-reg
		   (gethash key basic-region-groups))))
      ;; here we sort the keys in increasing order
      (setf groups-keys (sort groups-keys #'comp-less-lsts))
      ;; and store that order for this neighborhood
      (setf (region-keys neighborhood) groups-keys)
      ;; here we iterate over the region keys and build each region
      (loop for key in groups-keys
	 doing
	   (let (;; here we create the current region
		 (cur-region (make-instance 'neighborhood-region
					    :number-id cur-id
					    :neighborhood neighborhood))
		 )
	     (push cur-index (region-indexes neighborhood))
	     ;; here we iterate over the basic regions of this region
	     (loop for basic-reg in (gethash key basic-region-groups)
		doing
		  (setf (first-index basic-reg) cur-index)
		  ;; here we add the index and keys for this basic region
		  (push cur-index (basic-indexes cur-region))
		  (push (from-branch-info-to-branch-tuple (info basic-reg))
			(basic-keys cur-region))
		  ;; here we update some variables
		  (incf (cardinality cur-region) (cardinality basic-reg))
		  (incf cur-index (cardinality basic-reg))
		  ;; here we add the basic region to the current region
		  (push basic-reg (basic-region-list cur-region))
		  )
	     (setf
	      ;; here we reverse the order of indexes, keys and basic regions for this region
	      (basic-indexes cur-region) (reverse (basic-indexes cur-region))
	      (basic-keys cur-region) (reverse (basic-keys cur-region))	      
	      (basic-region-list cur-region) (reverse (basic-region-list cur-region))
	      ;; here we set the index range for the current region
	      (index-range cur-region) (cons (first-index (first (basic-region-list cur-region)))
					     (+ (first-index (first (basic-region-list cur-region)))
						(1- (cardinality cur-region)))))
	     ;; here we add the current region to the region list
	     (push cur-region region-list)
	     )
	   (incf cur-id)
	   )
      (setf
       ;; here we set some neighborhood properties
       (region-indexes neighborhood) (reverse (region-indexes neighborhood))
       (region-list neighborhood) (reverse region-list)
       (number-of-regions neighborhood) (length region-list)))))

(defun print-region (region)
  (format t "~%Region: ~A Range: ~A - ~A Cardinality: ~A~%"
	  (number-id region)
	  (car (index-range region))
	  (cdr (index-range region))
	  (cardinality region))
  (loop for b-reg in (basic-region-list region)
     doing
       (format t "Index: ~A Info: ~A Cardinality: ~A~%"
	       (first-index b-reg)
	       (info b-reg)
	       (cardinality b-reg))))

(defun combinatorial-search (neighborhood)

  (let ((cur-region-id 1)
	(region-generator-list nil)
	(reversed-list nil)
	(available-regions t)
	(cur-index 0))
    #'(lambda ()
	(if (> cur-region-id (number-of-regions neighborhood))
	    ;; here we iterate over the region's generators previously
	    ;; built till we find one that still has unvisited solutions
	    ;; or all were exhausted.
	    ;; this means that all regions were exhausted
	    ;; if branch
	    (if available-regions
		;; if branch
		(progn
		  ;; this list should be reversed once before start using it
		  (when (not reversed-list)
		    (setf reversed-list t
			  region-generator-list (reverse region-generator-list)))
		  (labels ((next-value (step)
			     (if (>= step (length region-generator-list))
				 ;; if branch
				 (values nil (setf available-regions nil))
				 ;; else branch
				 (let* ((index (mod (+ cur-index step)
						    (length region-generator-list)))
					(value (funcall (nth index region-generator-list))))
				   (if value
				       ;; if branch
				       (progn
					 (setf cur-index (mod (+ 1 index) (length region-generator-list)))
					 (values value (1+ index)))
				       ;; else branch
				       (next-value (1+ step)))))))
		    (next-value 0)))
		;; else branch
		(values nil nil))
	    ;; else branch
	    (let* ((cur-region (nth (1- cur-region-id) (region-list neighborhood)))
		   (cur-region-gen (funcall *exploration-strategy-for-combinatorial-search*
					    cur-region))
		   (cur-value (funcall cur-region-gen)))
	      ;; here we are building the region's generators on demand.
	      (push cur-region-gen region-generator-list)
	      (progn
		(incf cur-region-id)
		(values cur-value (1- cur-region-id))))))))

(defun sequential-search (neighborhood regions-ids-list)
  (if (null regions-ids-list)
      ;; if branch
      #'(lambda ()
	  nil)
      ;; else branch
      (let* (
	     ;; current region
	     (cur-region (nth (1- (first regions-ids-list))
			      (region-list neighborhood)))
	     ;; generator for the current region
	     (cur-region-gen (funcall *exploration-strategy-for-intensification-phase*
				      cur-region))
	     ;; current solution
	     (cur-solution (funcall cur-region-gen))
	     )

	;; remove cur-region from regions-ids-list
	(pop regions-ids-list)

	#'(lambda ()
	    (if (null cur-solution)
		;; if branch
		(if (null regions-ids-list)
		    ;; if branch
		    nil
		    ;; else branch
		    (progn
		      ;; here we move on to the next region
		      (setf cur-region (nth (1- (first regions-ids-list))
					    (region-list neighborhood))
			    cur-region-gen (funcall *exploration-strategy-for-intensification-phase*
						    cur-region))

		      ;; remove cur-region from regions-ids-list
		      (pop regions-ids-list)

		      (prog1
			  (funcall cur-region-gen)
			(setf cur-solution (funcall cur-region-gen)))))
		;; else branch
		(prog1
		    cur-solution
		  (setf cur-solution (funcall cur-region-gen))))))))

(defun get-number-of-regions  (neighborhood)
  ;; just return the number of regions
  (number-of-regions neighborhood))

(defmethod is-exhausted-p ((obj neighborhood-tree))
  (if (< (number-of-analysed-solutions (search-state obj))
	 (cardinality obj))
      nil t))

(defmethod is-exhausted-p ((obj neighborhood-region))
  (if (< (number-of-analysed-solutions (search-state obj))
	 (cardinality obj))
      nil t))

(defmacro exploration-interface (generator total)
  `(let ((N ,total))
     #'(lambda ()
	 (if (> N 0)
	     (progn
	       (decf N)
	       (funcall ,generator))
	     nil))))

(defun exhaustive-exploration (obj &optional total)
  (if (null total)
      (exhaustive-search obj)
      (let* ((gen (exhaustive-search obj)))
	(exploration-interface gen total))))

(defun random-exploration (obj n &optional total)
  (if (null total)
      (random-search obj n)
      (let* ((gen (random-search obj n)))
	(exploration-interface gen total))))

(defun uniform-exploration (obj &optional total)
  (if (null total)
      (uniform-search obj)
      (let* ((gen (uniform-search obj)))
	(exploration-interface gen total))))

(defun combinatorial-exploration (neigh &optional total)
  (if (null total)
      (combinatorial-search neigh)
      (let ((gen (combinatorial-search neigh)))
	(exploration-interface gen total))))

(defun sequential-exploration (neigh regions-ids-list &optional total)
  (if (null total)
      (sequential-search neigh regions-ids-list)
      (let ((gen (sequential-search neigh regions-ids-list)))
	(exploration-interface gen total))))
