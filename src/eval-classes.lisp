(in-package :vrp)

(def-vrp-class eval-graph ()
  ((inputs :initarg :inputs
	   :initform nil)
   (output :initarg :output
	   :initform nil)
   (class-to-io :initarg :class-to-io
		:initform (make-hash-table))
   (slot-to-output :initarg :slot-to-output
		   :initform (make-hash-table))
   (solution-track)
   (client-buffer :initarg :client-buffer
		  :initform (make-hash-table))
   (vehicle-buffer :initarg :client-buffer
		  :initform (make-hash-table))
   )
  :documentation "A class that represents an evaluation graph."
  :constructor (new-eval-graph (&key solution-track))
  :print-object-string ("<EVAL-GRAPH: ~% output: ~a  ~% inputs: ~%~{  ~a~%~} ~% client-buffer: ~a ~%~% solution-track: ~a>" output inputs client-buffer solution-track)
  :slots-for-obj= (inputs output class-to-io slot-to-output solution-track client-buffer vehicle-buffer)
  :slots-for-clone (inputs output class-to-io slot-to-output solution-track client-buffer vehicle-buffer))

(def-vrp-class node () ()
  :documentation "Ancestor node for eval-graph")

(def-vrp-class io-node (node) ()
  :documentation "ancestor for IO nodes in eval-graph")

(def-vrp-class input-node (io-node) 
  ((content))
  :documentation "input node node eval-graph"
  :constructor (new-input-node (&key content))
  :print-object-string ("<Input node, content: ~a>" content)
  :slots-for-obj= (content)
  :slots-for-clone (content))

(def-vrp-class input-distance-node (input-node) 
  ((first-distance-calculator :initform nil)
   (second-distance-calculator :initform nil))
  :documentation "distance node in eval-graph"
  :constructor (new-input-distance-node (&key content))
  :print-object-string ("<Cliient node ~a, ~%     first-distance-calculator: ~a, ~%     second-distance-calculator: ~a>" content first-distance-calculator second-distance-calculator)
  :slots-for-obj= (content first-distance-calculator second-distance-calculator)
  :slots-for-clone (content first-distance-calculator second-distance-calculator))

(def-vrp-class input-demand-node (input-node) 
  ((demand-calculator :initform nil))
  :documentation "client with demand node in eval-graph"
  :constructor (new-demand-node (&key content))
  :print-object-string ("<Client node, demand: ~a>" demand-calculator)
  :slots-for-obj= (content demand-calculator)
  :slots-for-clone (content demand-calculator))

(def-vrp-class input-distance-demand-node (input-distance-node
					  input-demand-node) 
  ()
  :documentation "client with demand node for a problem with distance in eval-graph"
  :constructor (new-input-distance-demand-node (&key content))
  :print-object-string ("<Cliient node ~a, ~%     first-distance-calculator: ~a, ~%     second-distance-calculator: ~a, ~%     demand-calculator: ~a>" content first-distance-calculator second-distance-calculator demand-calculator)
  :slots-for-obj= (content first-distance-calculator second-distance-calculator demand-calculator)
  :slots-for-clone (content first-distance-calculator second-distance-calculator demand-calculator))

(def-vrp-class output-node (io-node) 
  ((output-value)
   (updater :initform nil))
  :documentation "output node in eval-graph"
  :constructor (new-output-node (&key output-value))   
  :print-object-string ("<Output node, value: ~a>" output-value)
  :slots-for-obj= (output-value)
  :slots-for-clone (output-value))

(def-vrp-class accumulator-node (input-node output-node) 
  ((output-copy :initform nil))
  :documentation "accumulator node for partial results in eval-graph"
  :constructor (new-accumulator-node (&key content output-value))
  :print-object-string ("<value: ~a>" output-value)
  :slots-for-obj= (content output-value)
  :slots-for-clone (content output-value))

(def-vrp-class initial-value-accumulator-node (accumulator-node) 
  ((initial-value))
  :documentation "accumulator node for partial results in eval-graph"
  :constructor (new-initial-value-accumulator-node (&key content output-value initial-value))
  :print-object-string ("Initial value accumulator node, initial value: ~a, content: ~a, output-value: ~a>" initial-value content output-value)
  :slots-for-obj= (initial-value content output-value)
  :slots-for-clone (initial-value content output-value))

(def-vrp-class input-vehicle-node (input-node) 
  ((dependent-accumulator :initform nil)) ;; An initial-value accumulator
  :documentation "vehicle node in eval-graph"
  :constructor (new-input-vehicle-node (&key content))
  :print-object-string ("<Vehicle node, content: ~a, accumulator: ~a>" content dependent-accumulator)
  :slots-for-obj= (content dependent-accumulator)
  :slots-for-clone (content dependent-accumulator))

(def-vrp-class input-depot-node (input-distance-demand-node)
  ()
  :documentation "vehicle node in eval-graph"
  :constructor (new-input-depot-node (&key content))
  :print-object-string ("<depot node ~a, ~%     first-distance-calculator: ~a, ~%     second-distance-calculator: ~a, ~%     demand-calculator: ~a>" content first-distance-calculator second-distance-calculator demand-calculator)
  :slots-for-obj= (content demand-calculator first-distance-calculator second-distance-calculator)
  :slots-for-clone (content demand-calculator first-distance-calculator second-distance-calculator))

(def-vrp-class low-level-node (node) 
  ((previous-node) (next-node) (output-action))
  :documentation "low-level node in eval-graph")

(def-vrp-class increment-distance-node (low-level-node) 
    ((from-client) (to-client) (distance-matrix))
    :documentation "increment distance node in eval-graph"
  :constructor (new-increment-distance-node (&key previous-node next-node output-action from-client to-client distance-matrix))
    :print-object-string ("<output: ~a>" output-action)
    :slots-for-obj= (previous-node next-node output-action from-client to-client distance-matrix)
    :slots-for-clone (previous-node next-node output-action from-client to-client distance-matrix))

(def-vrp-class decrement-capacity-node (low-level-node)
  ((input-with-demand))
  :documentation "decrement capacity node in eval-graph"
  :constructor (new-decrement-capacity-node (&key previous-node next-node output-action input-with-demand))
  :print-object-string ("decrement-capacity: ouput: ~a>" output-action)
  :slots-for-obj= (previous-node next-node output-action input-with-demand)
  :slots-for-clone (previous-node next-node output-action input-with-demand))

(def-vrp-class increment-accumulator-node (low-level-node)
  ((partial-accumulator))
  :documentation "increment accumulator node in eval-graph"
  :constructor (new-increment-accumulator-node (&key previous-node next-node output-action partial-accumulator))
  :print-object-string ("increment accumulator node, prev: ~a, next: ~a>" previous-node next-node)
  :slots-for-obj= (previous-node next-node output-action partial-accumulator)
  :slots-for-clone (previous-node next-node output-action partial-accumulator))

(def-vrp-class penalize-accumulator-node (low-level-node)
  ((partial-accumulator)
   (factor))
  :documentation "penalize node in eval-graph"
  :constructor (new-penalize-accumulator-node (&key previous-node next-node output-action partial-accumulator factor))
  :print-object-string ("Penalizee node, action: ~a, accumulator: ~a>" output-action partial-accumulator)
  :slots-for-obj= (previous-node next-node output-action partial-accumulator factor)
  :slots-for-clone (previous-node next-node output-action partial-accumulator factor))
