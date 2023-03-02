(in-package :vrp)

(defmacro do-operations (ops)
    `(let* ((visited-op (make-hash-table))
            (queue ()))
       (progn
         (loop for op in (list ,@ops)
             do (let* ((evaluations (apply (car op) (cdr op))))
                   (loop for e in evaluations
                       do (if (not (gethash e visited-op))
                              (progn 
                                  (setf queue (append queue `(,e)))
                                  (setf (gethash e visited-op) t))))))
          (loop while queue
              do (let* ((nxt (car queue))
                        (to-insert (apply (car nxt) (cdr nxt))))
                       (progn
                          (setf queue (cdr queue))
                          (if (and to-insert (not (gethash to-insert visited-op)))
                              (progn 
                                  (setf queue (append queue `(,to-insert)))
                                  (setf (gethash to-insert visited-op) t)))))))))

(defun do-suite-operations (graph ops)
  (loop for op in ops do
    (let* ((converted-op (convert-op op graph))
	   (op (car converted-op))
	   (a (car (cdr converted-op)))
	   (b (car (cdr (cdr converted-op))))
	   (c (car (cdr (cdr (cdr converted-op))))))
      (funcall op a b c graph))))

(defun undo-suite-operations (graph ops)
  (loop for op in (reverse ops) do
    (let* ((converted-op (revert-op op graph))
	   (op (car converted-op))
	   (a (car (cdr converted-op)))
	   (b (car (cdr (cdr converted-op))))
	   (c (car (cdr (cdr (cdr converted-op))))))
      (funcall op a b c graph))))

(defun neigh-tree-solution (solution)
    (let ((new-solution (clone solution)))
(loop for r in (routes new-solution) do
(setf (clients r) (reverse (cdr (reverse (clients r)))))
)
new-solution))

(defmacro def-var (name init-value graph)
    `(defallocable ',name ,init-value ,graph))

(defmacro increment-distance (from-client to-client incremented-var distance-matrix graph)
    `(move-node-from-to ,from-client ,to-client ',incremented-var ,distance-matrix ,graph))

(defmacro increment-value (target-var value graph)
    `(increment-with ',target-var ',value ,graph))

(defmacro return-value (var graph)
  `(set-output ',var ,graph))

(defmacro decrement-demand (target-client demand-var graph)
    `(deliver ,target-client ',demand-var ,graph))

(defmacro apply-penalty (demand-var result-var penalty-factor graph)
  `(penalize ',demand-var ',result-var ,penalty-factor ,graph))

(defmethod init-graph ((s basic-solution))
    (let ((graph (new-eval-graph :solution-track s)))
    (convert-to-node s graph)
    graph))

(defun move-node-from-to (c1 c2 output-slot d-matrix graph)
  (let* ((input1 (gethash c1 (class-to-io graph)))
	 (input2 (gethash c2 (class-to-io graph)))
	 (out-node (gethash output-slot (slot-to-output graph)))
	 (d-node (new-increment-distance-node 
		  :output-action out-node
		  :from-client input1
		  :to-client input2 
		  :distance-matrix d-matrix)))

    (progn
      (setf (second-distance-calculator input1) d-node)
      (setf (first-distance-calculator input2) d-node)
      (evaluate-low-level-node d-node))))

(defmethod defallocable (output-slot value-of-slot graph)
    (let* ((new-node (new-accumulator-node :output-value value-of-slot)))
        (progn
            (setf (gethash output-slot (slot-to-output graph)) new-node))))

(defmethod increment-with (output-slot partial-slot graph)
  (let* ((out-node (gethash output-slot (slot-to-output graph)))
	 (part-node (gethash partial-slot (slot-to-output graph)))
	 (updt (new-increment-accumulator-node 
		:output-action out-node
		:partial-accumulator part-node)))
    (progn
      (setf (updater part-node) updt)
      (evaluate-low-level-node updt))))

(defmethod set-output (output-slot graph)
    (setf (output graph) (gethash output-slot (slot-to-output graph))))

(defmethod deliver (c capacity-slot-accumulator graph)
  ;;(if (not (typep c 'basic-depot))
      (let* ((c-node (gethash c (class-to-io graph)))
	     (acc (gethash capacity-slot-accumulator (slot-to-output graph)))
	     (c-calc (new-decrement-capacity-node :output-action acc :input-with-demand c-node)))
	(progn

	  (setf (demand-calculator c-node) c-calc)
	  (evaluate-low-level-node c-calc))))

(defmethod penalize (input-slot-accum output-slot-accum p-factor graph)
    (let* ((i-node (gethash input-slot-accum (slot-to-output graph)))
           (o-node (gethash output-slot-accum (slot-to-output graph)))
           (penalizer (new-penalize-accumulator-node :output-action o-node :partial-accumulator i-node :factor p-factor)))
        (progn
            (setf (updater i-node) penalizer)
            (evaluate-low-level-node penalizer))))

(defmethod remove-client-from (route-index client-index buffer-index graph)
    (let* ((target-route (car (nthcdr route-index (append '(()) (routes (solution-track graph))))))
           (target-client (car (nthcdr client-index (append '(()) (clients target-route)))))
           (node-to-remove (gethash target-client (class-to-io graph)))) 
        (progn
            (setf (clients target-route) (remove target-client (clients target-route)))
            (setf (gethash buffer-index (client-buffer graph)) target-client)
            (remove-node node-to-remove))))

(defmethod insert-client-to (route-index client-index buffer-index graph)
  (let* ((target-route (car (nthcdr route-index (append '(()) (routes (solution-track graph))))))
	 (target-client (gethash buffer-index (client-buffer graph)))
	 (node-to-insert (gethash target-client (class-to-io graph)))
	 (client-before-insert-to (car (nthcdr client-index (append '(()) (clients target-route)))))
	 (node-before-insert-to (gethash client-before-insert-to (class-to-io graph))))


    ;;      (format t "~a ~%" node-before-insert-to)
    ;;      (format t "~a ~%~%" node-to-insert)

    (remhash buffer-index (client-buffer graph))
    (if (eq client-index 1)
	;; if branch
	(push target-client (clients target-route))
	;; else branch
	(push target-client (cdr (nthcdr (- client-index 1) (append '(()) (clients target-route))))))

    (insert-node node-before-insert-to node-to-insert)))

(defmethod get-value (var-symbol graph)
    (output-value (gethash var-symbol (slot-to-output graph))))

(defmethod get-output (graph)
    (output-value (output graph)))

(defmethod set-route-vehicle (route-instance route-accumulator graph)
    (let* ((v-input (gethash (vehicle route-instance) (class-to-io graph)))
           (init-val (capacity (vehicle route-instance)))
           (new-node (new-accumulator-node :output-value init-val)))
        (progn
            (setf (gethash route-accumulator (slot-to-output graph)) new-node)
            (setf (dependent-accumulator v-input) new-node))))

(defmethod remove-vehicle-from (route-index graph)
    (let* ((target-vehicle (vehicle (car (nthcdr route-index (routes (solution-track graph))))))
           (target-input (gethash target-vehicle (class-to-io graph))))
        (progn
            (remove-node target-input)
            (setf (vehicle-buffer graph) (append (vehicle-buffer graph) target-input)))))
