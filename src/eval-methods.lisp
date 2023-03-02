(in-package :vrp)

(defgeneric undo-low-level-node (ll-node)
   (:documentation "The generic action to undo the low level graph node evaluation"))

(defgeneric evaluate-low-level-node (ll-node)
   (:documentation "The generic action to do in the low level graph node"))

(defmethod evaluate-low-level-node ((ll-node low-level-node))
   ())

(defmethod evaluate-low-level-node ((ll-node increment-distance-node))
   (progn
       (incf (output-value (output-action ll-node))
         (aref (distance-matrix ll-node) 
               (id (content (from-client ll-node))) 
               (id (content (to-client ll-node)))))
       (if (updater (output-action ll-node))
           (undo-low-level-node (updater (output-action ll-node)))
            nil)))

(defmethod evaluate-low-level-node ((ll-node decrement-capacity-node))
  (progn
    (if (not (typep (content (input-with-demand ll-node)) 'basic-depot))
	(decf (output-value (output-action ll-node)) 
	      (demand (content (input-with-demand ll-node)))))
    (if (updater (output-action ll-node))
	  (undo-low-level-node (updater (output-action ll-node))))))

(defmethod evaluate-low-level-node ((ll-node increment-accumulator-node))
   (progn
       (setf (output-copy (partial-accumulator ll-node))
             (output-value (partial-accumulator ll-node)))
       (incf (output-value (output-action ll-node)) 
             (output-value (partial-accumulator ll-node)))
       nil))

(defmethod evaluate-low-level-node :after ((ll-node penalize-accumulator-node))
   (let ((penal-amount (* (max (- (output-value (partial-accumulator ll-node))) 0) (factor ll-node))))
        (progn 
             (setf (output-copy (partial-accumulator ll-node))
                   (min 0 (output-value (partial-accumulator ll-node))))
             (incf (output-value (output-action ll-node))
                    penal-amount)
             nil)))

(defmethod undo-low-level-node ((ll-node low-level-node))
   ())

(defmethod undo-low-level-node ((ll-node increment-distance-node))
   (progn
       (decf (output-value (output-action ll-node)) 
         (aref (distance-matrix ll-node) 
               (id (content (from-client ll-node))) 
               (id (content (to-client ll-node)))))
       (if (updater (output-action ll-node))
           (undo-low-level-node (updater (output-action ll-node))))))

(defmethod undo-low-level-node ((ll-node decrement-capacity-node))
  (progn
    (if (not (typep (content (input-with-demand ll-node)) 'basic-depot))
	(incf (output-value (output-action ll-node)) 
	      (demand (content (input-with-demand ll-node)))))
    nil))

(defmethod undo-low-level-node :after ((ll-node increment-accumulator-node))
    (progn 
        (incf (output-value (output-action ll-node))
              (- (output-value (partial-accumulator ll-node))
                 (output-copy (partial-accumulator ll-node))))
        (setf (output-copy (partial-accumulator ll-node)) 
              (output-value (partial-accumulator ll-node)))
        (if (updater (output-action ll-node))
            (undo-low-level-node (updater (output-action ll-node))))))

(defmethod undo-low-level-node :after ((ll-node penalize-accumulator-node))
    (let ((former-p (* (- (output-copy (partial-accumulator ll-node)))
                       (factor ll-node)))
          (current-p (* (max 0 (- (output-value (partial-accumulator ll-node))))
                        (factor ll-node))))
        (progn
            (decf (output-value (output-action ll-node))
                   former-p)
            (incf (output-value (output-action ll-node))
                   current-p)
            (setf (output-copy (partial-accumulator ll-node)) 
                  (min 0 (output-value (partial-accumulator ll-node))))
            (if (updater (output-action ll-node))
                (undo-low-level-node (updater (output-action ll-node)))))))

(defgeneric remove-node (t-node)
    (:method-combination append))

(defmethod remove-node append ((t-node input-node))
    ())

(defmethod remove-node append ((t-node input-distance-node))

  (if (not (typep t-node 'input-depot-node)) 
      (progn
	(undo-low-level-node (first-distance-calculator t-node))

	(undo-low-level-node (second-distance-calculator t-node))

	(let ((new-inc (new-increment-distance-node
			:output-action (output-action (first-distance-calculator t-node))
			:from-client (from-client (first-distance-calculator t-node))
			:to-client (to-client (second-distance-calculator t-node))
			:distance-matrix (distance-matrix (first-distance-calculator t-node)))))
	  (progn
	    (setf (second-distance-calculator (from-client new-inc))
		  new-inc)
	    (setf (first-distance-calculator (to-client new-inc))
		  new-inc)

	    (evaluate-low-level-node new-inc))))))

(defmethod remove-node append ((t-node input-demand-node))
   (if (not (typep t-node 'input-depot-node))
     (progn
      (undo-low-level-node (demand-calculator t-node))
      (undo-low-level-node (updater (output-action (demand-calculator t-node)))))))

(defmethod remove-node append ((t-node input-depot-node))
      (if (second-distance-calculator t-node)
        (undo-low-level-node (new-increment-distance-node
                                    :previous-node (previous-node (second-distance-calculator t-node))
                                    :next-node (next-node (second-distance-calculator t-node))
                                     :output-action(output-action (second-distance-calculator t-node))
                                     :from-client (from-client (second-distance-calculator t-node))
                                     :to-client (to-client (second-distance-calculator t-node))
                                     :distance-matrix (distance-matrix (second-distance-calculator t-node))))
        (undo-low-level-node (new-increment-distance-node
                                    :previous-node (previous-node (first-distance-calculator t-node))
                                    :next-node (next-node (first-distance-calculator t-node))
                                     :output-action(output-action (first-distance-calculator t-node))
                                     :from-client (from-client (first-distance-calculator t-node))
                                     :to-client (to-client (first-distance-calculator t-node))
                                     :distance-matrix (distance-matrix (first-distance-calculator t-node))))))

(defgeneric insert-node (t-node i-node)
    (:method-combination append))

(defmethod insert-node append ((t-node input-node) (i-node input-node))
    ())

(defmethod insert-node append ((t-node input-distance-node) 
			       (i-node input-distance-node))
  (if (not (typep (content i-node) 'basic-depot))  
      (let* ((target-calc (first-distance-calculator t-node))
	     (next-client t-node)
	     (new-calc (new-increment-distance-node
			:output-action (output-action target-calc)
			:from-client i-node
			:to-client next-client
			:distance-matrix (distance-matrix target-calc))))
	(progn
	  (undo-low-level-node target-calc)
	  (setf (to-client target-calc) i-node)
	  (setf (first-distance-calculator i-node) target-calc)
	  (setf (second-distance-calculator i-node) new-calc)
	  (setf (first-distance-calculator t-node) new-calc)
	  (evaluate-low-level-node target-calc)
	  (evaluate-low-level-node new-calc)))))

(defmethod insert-node append ((t-node input-demand-node) 
				 (i-node input-demand-node))
;;    (if (not (typep (content t-node) 'basic-depot))
	;; if branch
	(let* ((new-inc (new-decrement-capacity-node 
			 :output-action (output-action (demand-calculator t-node))
			 :input-with-demand i-node)))
	  (progn
	    (setf (demand-calculator i-node) new-inc)
	    (evaluate-low-level-node new-inc))))

;;	;; else branch
;;	(let* ((new-inc (new-decrement-capacity-node 
;;			 :output-action (output-action (demand-calculator (from-client (first-distance-calculator t-node)))) ;; TODO find a better way to do this. Why if for wathever reason t-nod is not distance-node
;;			 :input-with-demand i-node)))
;;	  (progn
;;	    (setf (demand-calculator i-node) new-inc)
;;	    (evaluate-low-level-node new-inc))))))

(defmethod insert-node append ((t-node input-distance-node) 
                                      (i-node input-depot-node))
   (let* ((is-origin (if (second-distance-calculator i-node) t nil))
          (calc (if is-origin 
                    (first-distance-calculator t-node)
                    (second-distance-calculator t-node))))
       (progn
          (if is-origin
              (progn 
                  (setf (second-distance-calculator i-node) calc)
                  (setf (from-client calc) i-node))
              (progn
                  (setf (first-distance-calculator i-node) calc)
                  (setf (to-client calc) i-node)))
          (evaluate-low-level-node calc))))

(defgeneric convert-to-node (target graph))

(defmethod convert-to-node (target graph) ())

(defmethod convert-to-node :after ((target basic-solution) graph)
      (loop for r in (routes target) do
          (convert-to-node r graph)))

(defmethod convert-to-node :after ((target has-clients) graph)
     (progn
         (loop for c in (clients target) do
         (convert-to-node c graph))))

(defmethod convert-to-node :after ((target has-vehicles) graph)
     (progn
         (loop for v in (vehicles target) do
         (convert-to-node v graph))))

(defmethod convert-to-node :after ((target has-multi-depots) graph)
  (loop for d in (depots target) do
      (if (gethash d (class-to-io graph))
           nil 
          (convert-to-node d graph))))

(defmethod convert-to-node :after ((target basic-route) graph)
  (progn
    (convert-to-node (vehicle target) graph)
    (let* ((new-r (new-input-node :content target)))
      (progn
	(setf (inputs graph) (append (inputs graph) `(,new-r)))
	(setf (gethash target (class-to-io graph)) new-r)))))

(defmethod convert-to-node :before ((target route-for-simulation) graph)
     (progn
         (convert-to-node (previous-client target) graph)))

(defmethod convert-to-node :after ((target basic-client) graph)
    (let ((new-c (new-input-distance-node :content target)))
	(progn
	    (setf (inputs graph) (append (inputs graph) `(,new-c)))
	    (setf (gethash target (class-to-io graph)) new-c))))

(defmethod convert-to-node :around ((target demand-client) graph)
     (let ((new-c (new-input-distance-demand-node :content target)))
	 (progn
	     (setf (inputs graph) (append (inputs graph) `(,new-c)))
	     (setf (gethash target (class-to-io graph)) new-c))))

(defmethod convert-to-node :after ((target basic-depot) graph)
     (let ((new-c (new-input-depot-node :content target)))
         (progn
             (setf (inputs graph) (append (inputs graph) `(,new-c)))
             (setf (gethash target (class-to-io graph)) new-c))))

(defmethod convert-to-node :after ((target basic-vehicle) graph)
     (let ((new-c (new-input-vehicle-node :content target)))
         (progn
             (setf (inputs graph) (append (inputs graph) `(,new-c)))
             (setf (gethash target (class-to-io graph)) new-c))))

(defgeneric convert-op (target graph))

(defmethod convert-op (target graph) ())

(defmethod convert-op ((target operation-select-client) graph)
  `(,#'remove-client-from ,(route target) ,(pos target) ,(operand target) ,graph))

(defmethod convert-op ((target operation-insert-client) graph)
  `(,#'insert-client-to ,(route target) ,(pos target) ,(operand target) ,graph))

(defgeneric revert-op (target graph))

(defmethod revert-op (target graph) ())

(defmethod revert-op ((target operation-select-client) graph)
  `(,#'insert-client-to ,(route target) ,(pos target) ,(operand target) ,graph))

(defmethod revert-op ((target operation-insert-client) graph)
  `(,#'remove-client-from ,(route target) ,(pos target) ,(operand target) ,graph))
