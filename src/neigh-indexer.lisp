(in-package :vrp)

(defmethod nth-neighbor ((tree root-node)
			 index)
  (if (or (null (child tree)) (> index (total tree)) (< index 1))
      (error "In root-node nth-neighbor index is incorrect.")
      (nth-neighbor (child tree)
		    index)))

(defmethod nth-neighbor ((tree r-node)
			 index)
  (let* ((acc 0)
	 (pos 0)
	 (child nil))
    (loop for (current-child . current-route) in (children tree) do
	 (progn
	   (when (<= index acc)
	     (return))
	   (setf child current-child)
	   (setf acc (+ acc (total current-child)))
	   (setf pos current-route)))
    (if (> index acc)
	(error "In r-node nth-neighbor index is incorrect.")
	(progn
	  (setf acc (- acc (total child)))
	  (cons  (list 'r pos)
		 (nth-neighbor child (- index acc)))))))

(defmethod nth-neighbor ((tree a-node)
			 index)
  (let* ((pos (1+ (truncate (/ index (total (child tree)))))))
    (if (= index (* (total (child tree)) (1- pos)))
	(decf pos))
    (if (> pos (possibilities tree))
	(error "In a-node nth-neighbor index is incorrect.")
	(cons (list 'a (route-number tree) pos (node-id tree))
	      (nth-neighbor (child tree)
			    (- index (* (1- pos) (total (child tree)))))))))

(defmethod nth-neighbor ((tree b-node)
			 index)
  (let* ((pos (1+ (truncate (/ index (total (child tree)))))))

    (if (= index (* (total (child tree)) (1- pos)))
	(decf pos))
    (if (> pos (possibilities tree))
	(error "In b-node nth-neighbor index is incorrect.")
	(cons (list 'b (route-number tree) pos (node-id tree))
	      (nth-neighbor (child tree)
			    (- index (* (1- pos) (total (child tree)))))))))

(defmethod nth-neighbor ((tree c-node)
			 index)
  (if (> index (total tree))
      (error "In c-node nth-neighbor index is incorrect.")
      (cons (list 'c (select-op1-id tree) (select-op2-id tree))
	    (nth-neighbor (child tree) index))))

(defmethod nth-neighbor ((tree e-node)
			 index)
  (let* ((acc 0)
	 (pos 0)
	 (subr-len 0)
	 (child nil))
    (loop for (current-child (len . positions)) in (children tree) do
	 (progn
	   (when (<= index acc)
	     (return))
	   (setf child current-child
		 acc (+ acc (* positions (total current-child)))
		 subr-len len
		 pos positions)))

    (if (> index acc)
	(error "In e-node nth-neighbor index is incorrect.")
	(progn
	  (let* ((acc (- acc (* pos (total child))))
		 (index (- index acc))
		 (start-pos (1+ (truncate (/ index (total child))))))
	    (if (= index (* (total child) (1- start-pos)))
		(decf start-pos)) 
	    (cons (list 'e (route-number tree) start-pos subr-len (node-id tree))
		  (nth-neighbor child (- index (* (1- start-pos) (total child))))))))))

(defmethod nth-neighbor ((tree f-node)
			 index)
  (let* ((pos (1+ (truncate (/ index (total (child tree)))))))    
    (if (= index (* (total (child tree)) (1- pos)))
	(decf pos))
    (if (> pos (possibilities tree))
	(error "In f-node nth-neighbor index is incorrect.")
	(cons (list 'f (route-number tree) pos (select-op-id tree))
	      (nth-neighbor (child tree)
			    (- index (* (1- pos) (total (child tree)))))))))

(defmethod nth-neighbor ((tree g-node)
			 index)
  (if (> index (total tree))
      (error "In g-node nth-neighbor index is incorrect.")
      (cons (list 'g (select-op1-id tree) (select-op2-id tree))
	    (nth-neighbor (child tree) index))))

(defmethod nth-neighbor ((tree h-node)
			 index)
  (if (> index (total tree))
      (error "In h-node nth-neighbor index is incorrect.")
      (cons (list 'h (select-op-id tree))
	    (nth-neighbor (child tree) index))))

(defmethod nth-neighbor ((tree nil-node)
			 index)
  (if (= index 1)
      nil
      (error "In nil-node nth-neighbor index is incorrect.")))

(defmethod ops-neighbor ((tree root-node)
			 ops default)
  (if (null (child tree))
      (error "In root-node ops-neighbor ops list is incorrect.")
      (ops-neighbor (child tree) ops 0)))

(defmethod ops-neighbor ((tree r-node)
			 ops default)
  (let* ((acc 0)
	 (pos 0)
	 (child nil)
	 (op-route (second (car ops))))
    (loop for (current-child . current-route) in (children tree) do
	 (progn
	   (setf pos current-route
		 child current-child)
	   (when (= op-route current-route)
	     (return))
	   (setf acc (+ acc (total current-child)))))
    (if (= op-route pos)
	(+ acc
	   (ops-neighbor child (cdr ops) 0))
	(error "In r-node ops-neighbor ops list is incorrect."))))

(defmethod ops-neighbor ((tree a-node)
			 ops default)
  (let ((op-position (third (car ops))))
    (if (> op-position (possibilities tree))
	(error "In a-node ops-neighbor ops list is incorrect.")
	(+ (* (1- op-position) (total (child tree))) 
	   (ops-neighbor (child tree) (cdr ops) 1)))))

(defmethod ops-neighbor ((tree b-node)
			 ops default)
  (let ((op-position (third (car ops))))
    (if (> op-position (possibilities tree))
	(error "In b-node ops-neighbor ops list is incorrect.")
	(+ (* (1- op-position) (total (child tree))) 
	   (ops-neighbor (child tree) (cdr ops) 1)))))

(defmethod ops-neighbor ((tree c-node)
			 ops default)
  (ops-neighbor (child tree) (rest ops) 1))

(defmethod ops-neighbor ((tree e-node)
			 ops default)
  (let* ((acc 0)
	 (pos 0)
	 (subr-len 0)
	 (child nil)
	 (start-pos (third (car ops)))
	 (k-length (fourth (car ops))))
    (loop for (current-child (len . positions)) in (children tree) do
	 (progn
	   (setf child current-child
		 subr-len len
		 pos positions)
	   (when (= k-length len)
	     (return))
	   (setf acc (+ acc (* positions (total current-child))))))
    (if (and (= k-length subr-len) (<= start-pos pos))
	(+ acc
	   (* (1- start-pos) (total child))
	   (ops-neighbor child (cdr ops) 0))
	(error "In e-node ops-neighbor ops list is incorrect."))))

(defmethod ops-neighbor ((tree f-node)
			 ops default)
  (let ((op-position (third (car ops))))
    (if (> op-position (possibilities tree))
	(error "In f-node ops-neighbor ops list is incorrect.")
	(+ (* (1- op-position) (total (child tree))) 
	   (ops-neighbor (child tree) (cdr ops) 1)))))

(defmethod ops-neighbor ((tree g-node)
			 ops default)
  (ops-neighbor (child tree) (rest ops) 1))

(defmethod ops-neighbor ((tree h-node)
			 ops default)
  (ops-neighbor (child tree) (rest ops) 1))

(defmethod ops-neighbor ((tree nil-node)
			 ops default)
    (if (null ops)
	default
	(error "In nil-node ops-neighbor ops list is incorrect.")))

(defun index-to-coords-classic-indexer (neighborhood index)
  (nth-neighbor (tree neighborhood) index))

(defun coords-to-index-classic-indexer (neighborhood coords)
  (ops-neighbor (tree neighborhood) coords 0))

(defun nth-ops-classic-indexer-test (neighborhood)
  (let* ((N (cardinality neighborhood)))
    (loop for i from 1 to N
       ;; do
       ;;	 (if (= i (coords-to-index-classic-indexer neighborhood
       ;;					   (index-to-coords-classic-indexer neighborhood i)))
	 ;;(format t "Nth: ~A is ok.~%" i)
	 ;;(format t "Nth: ~A is wrong.~%" i)))))
       sum (abs (- i (coords-to-index-classic-indexer neighborhood
						      (index-to-coords-classic-indexer neighborhood i)))))))

(defmethod nth-branch-neighbor ((tree root-node)
				index
				branch-info branch-poss)
  (let ((total (first branch-poss)))
    (if (or (null (child tree)) (> index total) (< index 1))
	(error "In root-node nth-branch-neighbor some error occurred.")
	(nth-branch-neighbor (child tree) index branch-info (rest branch-poss)))))

(defmethod nth-branch-neighbor ((tree r-node)
				index
				branch-info branch-poss)
  (let* ((pos (1- (cdr (first branch-info))))
	 (child (car (nth pos (children tree))))
	 (route (cdr (nth pos (children tree)))))

    (cons (list 'r route)
	  (nth-branch-neighbor child index
			       (rest branch-info) branch-poss))))

(defmethod nth-branch-neighbor ((tree a-node)
				index
				branch-info branch-poss)

  (let* ((sol-for-poss (first branch-poss))
	 (pos (1+ (truncate (/ index sol-for-poss)))))
    (when (= index (* sol-for-poss (1- pos)))
      (decf pos))
    (cons (list 'a (route-number tree) pos (node-id tree))
	      (nth-branch-neighbor (child tree)
				   (- index (* (1- pos) sol-for-poss))
				   branch-info
				   (rest branch-poss)))))

(defmethod nth-branch-neighbor ((tree b-node)
			       index
			       branch-info branch-poss)

  (let* ((sol-for-poss (first branch-poss))
	 (pos (1+ (truncate (/ index sol-for-poss)))))
    (when (= index (* sol-for-poss (1- pos)))
      (decf pos))
    (cons (list 'b (route-number tree) pos (node-id tree))
	  (nth-branch-neighbor (child tree)
			       (- index (* (1- pos) sol-for-poss))
			       branch-info
			       (rest branch-poss)))))

(defmethod nth-branch-neighbor ((tree c-node)
				index
				branch-info branch-poss)
  (cons (list 'c (select-op1-id tree) (select-op2-id tree))
	(nth-branch-neighbor (child tree) index
			     branch-info branch-poss)))

(defmethod nth-branch-neighbor ((tree e-node)
				index
				branch-info branch-poss)

  (let* ((pos (1- (cdr (first branch-info))))
	 (sol-for-poss (first branch-poss))
	 (child (car (nth pos (children tree))))
	 (subr-len (car (car (cdr (nth pos (children tree))))))
	 (start-pos (1+ (truncate (/ index sol-for-poss)))))

    (when (= index (* sol-for-poss (1- start-pos)))
      (decf start-pos))

    (cons (list 'e (route-number tree) start-pos subr-len (node-id tree))
	  (nth-branch-neighbor child (- index (* (1- start-pos) sol-for-poss))
			       (rest branch-info) (rest branch-poss)))))

(defmethod nth-branch-neighbor ((tree f-node)
				index
				branch-info branch-poss)
  (let* ((sol-for-poss (first branch-poss))
	 (pos (1+ (truncate (/ index sol-for-poss)))))

    (when (= index (* sol-for-poss (1- pos)))
      (decf pos))

    (cons (list 'f (route-number tree) pos (select-op-id tree))
	  (nth-branch-neighbor (child tree)
			       (- index (* (1- pos) sol-for-poss))
			       branch-poss (rest branch-poss)))))

(defmethod nth-branch-neighbor ((tree g-node)
				index
				branch-info branch-poss)
  (cons (list 'g (select-op1-id tree) (select-op2-id tree))
	(nth-branch-neighbor (child tree) index
			     branch-info branch-poss)))

(defmethod nth-branch-neighbor ((tree h-node)
				index
				branch-info branch-poss)
  (cons (list 'h (select-op-id tree))
	(nth-branch-neighbor (child tree) index
			     branch-info branch-poss)))

(defmethod nth-branch-neighbor ((tree nil-node)
				index
				branch-info branch-poss)
  (if (= index 1)
      nil
      (error "In nil-node nth-neighbor index is incorrect.")))

(defmethod ops-branch-neighbor ((tree root-node)
				ops
				branch-info branch-poss
				default)
  (if (null (child tree))
      (error "In root-node ops-branch-neighbor some error occurred.")
      (ops-branch-neighbor (child tree) ops branch-info (rest branch-poss) 0)))

(defmethod ops-branch-neighbor ((tree r-node)
				ops
				branch-info branch-poss
				default)
  (let* ((op-route (second (car ops)))
	 (pos (1- (cdr (first branch-info))))
	 (child (car (nth pos (children tree))))
	 (route (cdr (nth pos (children tree)))))
    (if (= op-route route)
	(ops-branch-neighbor child (cdr ops) (rest branch-info)
			     branch-poss 0)
	(error "In r-node ops-branch-neighbor some error occurred."))))

(defmethod ops-branch-neighbor ((tree a-node)
				ops
				branch-info branch-poss
				default)

  (let ((sol-for-poss (first branch-poss))
	(op-position (third (car ops))))
    (if (> op-position (possibilities tree))
	(error "In a-node nth-branch-neighbor some error occurred.")
	(+ (* (1- op-position) sol-for-poss)
	   (ops-branch-neighbor (child tree) (cdr ops) branch-info (rest branch-poss) 1)))))

(defmethod ops-branch-neighbor ((tree b-node)
				ops
				branch-info branch-poss
				default)
  (let ((sol-for-poss (first branch-poss))
	(op-position (third (car ops))))
    (if (> op-position (possibilities tree))
	(error "In b-node ops-branch-neighbor some error occurred.")
	(+ (* (1- op-position) sol-for-poss)
	   (ops-branch-neighbor (child tree) (cdr ops) branch-info (rest branch-poss) 1)))))

(defmethod ops-branch-neighbor ((tree c-node)
				ops
				branch-info branch-poss
				default)
  (ops-branch-neighbor (child tree) (rest ops) branch-info branch-poss 1))

(defmethod ops-branch-neighbor ((tree e-node)
				ops
				branch-info branch-poss
				default)
  (let* ((pos (1- (cdr (first branch-info))))
	 (sol-for-poss (first branch-poss))
	 (child (car (nth pos (children tree))))
	 (subr-len (car (car (cdr (nth pos (children tree))))))
	 (possib (cdr (car (cdr (nth pos (children tree))))))

	 (start-pos (third (car ops)))
	 (k-length (fourth (car ops))))
    (if (and (= k-length subr-len) (<= start-pos possib))
	(+ (* (1- start-pos) sol-for-poss)
	   (ops-branch-neighbor child (cdr ops) (rest branch-info) (rest branch-poss) 0))
	(error "In e-node some error occurred."))))

(defmethod ops-branch-neighbor ((tree f-node)
				ops
				branch-info branch-poss
				default)
  (let ((sol-for-poss (first branch-poss))
	(op-position (third (car ops))))
    (if (> op-position (possibilities tree))
	(error "In f-node ops-branch-neighbor some error occurred.")
	(+ (* (1- op-position) sol-for-poss)
	   (ops-branch-neighbor (child tree) (cdr ops) branch-info (rest branch-poss) 1)))))

(defmethod ops-branch-neighbor ((tree g-node)
				ops
				branch-info branch-poss
				default)
  (ops-branch-neighbor (child tree) (rest ops) branch-info branch-poss 1))

(defmethod ops-branch-neighbor ((tree h-node)
				ops
				branch-info branch-poss
				default)
  (ops-branch-neighbor (child tree) (rest ops) branch-info branch-poss 1))

(defmethod ops-branch-neighbor ((tree nil-node)
				ops
				branch-info branch-poss
				default)
  (if (null ops)
      default
      (error "In nil-node ops-neighbor ops list is incorrect.")))

(defun nth-ops-branch-neighbor-test (neighborhood)
  (loop for region in (region-list neighborhood)
     doing
       (format t "Region: ~A~%" (number-id region))
       (loop for b-reg in (basic-region-list region)
	  doing
	    (let ((N (cardinality b-reg))
		  (tree-neigh (tree neighborhood))
		  (information (info b-reg))
		  (sub-sols (subtree-sols b-reg)))
	      (format t "~A~%" (loop for i from 1 to N
				  sum (abs (- i (ops-branch-neighbor tree-neigh
								     (nth-branch-neighbor tree-neigh
											  i
											  information
											  sub-sols)
								     information
								     sub-sols
								     0)))))
	      ))))

(defmethod from-coordinate-to-level ((symbol (eql 'r))
				     coordinate)
  ;; ('r route-number)
  (list (cons 'r (first coordinate))))

(defmethod from-coordinate-to-level ((symbol (eql 'a))
				     coordinate)
  ;; ('a route-number client-position id-operation)
  nil)

(defmethod from-coordinate-to-level ((symbol (eql 'b))
				     coordinate)
  ;; ('b route-number insertion-position id-select-client-operation)
  nil)

(defmethod from-coordinate-to-level ((symbol (eql 'c))
				     coordinate)
  ;; ('c id-select-client1-operation id-select-client2-operation)
  nil)

(defmethod from-coordinate-to-level ((symbol (eql 'e))
				     coordinate)
  ;; ('e route-number start-position length id-operation)
  (list (cons 'e (third coordinate))))

(defmethod from-coordinate-to-level ((symbol (eql 'f))
				     coordinate)
  ;; ('f route-number insertion-position id-select-subroute-operation)
  nil)

(defmethod from-coordinate-to-level ((symbol (eql 'g))
					 coordinate)
  ;; ('g id-select-subroute1-operation id-select-subroute2-operation)
  nil)

(defmethod from-coordinate-to-level ((symbol (eql 'h))
				     coordinate)
  ;; ('h id-select-subroute-operation)
  nil)

(defmethod from-coordinate-list-to-branch-info (coordinate-list)
  (let (branch-info)
    ;; collect the info
    (loop for coord in coordinate-list
       doing
	 (setf branch-info (append branch-info
				   (from-coordinate-to-level (first coord)
							     (rest coord)))))
    ;; return the corresponding branch-info for the current coord-list
    branch-info))

(defmethod from-branch-info-to-branch-tuple (branch-info)
  (loop for (sym . num) in branch-info
     collecting
       num))

(defmethod from-branch-info-to-region-tuple (branch-info)
  (loop for (sym . num) in branch-info
     collecting
       (if (eql sym 'e)
	   (binary-search num *subroute-length-upper-bounds*)
	   num)))

(defmethod from-coordinate-list-to-branch-tuple (branch-info)
  (from-branch-info-to-branch-tuple
   (from-coordinate-list-to-branch-info
    branch-info)))

(defmethod from-coordinate-list-to-region-tuple (branch-info)
  (from-branch-info-to-region-tuple
   (from-coordinate-list-to-branch-info
    branch-info)))

(defun index-to-coords-customize-indexer (neighborhood index)
  (let ((region (nth (binary-search index (region-indexes neighborhood))
		     (region-list neighborhood))))
    (let ((basic-region (nth (binary-search index (basic-indexes region))
			     (basic-region-list region))))
      (nth-branch-neighbor (tree neighborhood)
			   (1+ (- index (first-index basic-region)))
			   (info basic-region)
			   (subtree-sols basic-region)))))


(defun coords-to-index-customize-indexer (neighborhood coords)
  (let ((region (nth
		 (binary-search (from-coordinate-list-to-region-tuple coords)
				(region-keys neighborhood)
				:fn #'comp-less-lsts)
		 (region-list neighborhood))))
    (let ((basic-region (nth
			 (binary-search (from-coordinate-list-to-branch-tuple coords)
					(basic-keys region)
					:fn #'comp-less-lsts)
			 (basic-region-list region))))
      (+ (1-(first-index basic-region))
	 (ops-branch-neighbor (tree neighborhood)
			      coords
			      (info basic-region)
			      (subtree-sols basic-region)
			      0)))))

(defun nth-ops-customize-indexer-test (neighborhood)
  (let* ((N (cardinality neighborhood)))
    (loop for i from 1 to N
       ;; do
       ;;	 (if (= i (coords-to-index-customize-indexer neighborhood
       ;;					   (index-to-coords-customize-indexer neighborhood i)))
	 ;;(format t "Nth: ~A is ok.~%" i)
	 ;;(format t "Nth: ~A is wrong.~%" i)))))
       sum (abs (- i (coords-to-index-customize-indexer neighborhood
						      (index-to-coords-customize-indexer neighborhood i)))))))

(defun from-index-to-coord-list (neighborhood index)
  (funcall *from-index-to-coords-indexer-function* neighborhood index))

(defun from-coord-list-to-index (neighborhood coords)
  (funcall *from-coords-to-indexer-indexer-function* neighborhood coords))
