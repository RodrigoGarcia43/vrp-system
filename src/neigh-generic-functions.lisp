(in-package :vrp)

(defgeneric create-neigh-operation-from (symbol element)
  (:documentation "Returns an instance of a neighborhood operation."))

(defgeneric count-neighbors (op sol other-ops default)
  (:documentation "Returns how many neighbors this solution has according to this op."))

(defgeneric nth-neighbor (tree index)
  (:documentation "Returns the nth neighbor /operation list/ according to this neighborhood tree or nil if index is greater than total."))

(defgeneric ops-neighbor (tree ops default)
  (:documentation "Returns the index for this /operation list/ according to this neighborhood tree or nil if ops are not valid."))

(defgeneric nth-branch-neighbor (tree index branch-info branch-poss)
  (:documentation "Returns the nth branch neighbor /coordinate list/ according to this neighborhood tree or nil if index is greater than total."))

(defgeneric ops-branch-neighbor (tree ops branch-info branch-poss default)
  (:documentation "Returns the index for this /operation list/ according to this neighborhood tree or nil if ops are not valid."))

(defgeneric branch-cardinality (tree ops index branch)
  (:documentation "This function returns the cardinality in the current region or branch 
  of the neighborhood tree. When index is nil, we are going down in the tree."))

(defgeneric from-coordinate-to-operation (symbol coordinate)
  (:documentation "Returns an instance of the neighborhood operation that correspond with a given coordinate."))

(defgeneric transform-to-counting-solution (vrp-sol)
  (:documentation "Returns an instance of a counting solution."))

(defgeneric compute-delta-position (symbol coordinate route position)
  (:documentation "Returns in how many varies a select operation position after performing an operation."))

(defgeneric from-coordinate-to-level (symbol coordinate)
  (:documentation "Returns the level of the factor that corresponds with the current coordinate."))

(defgeneric from-coordinate-list-to-branch-info (coordinate-list))

(defgeneric from-branch-info-to-branch-tuple (branch-info))

(defgeneric from-branch-info-to-region-tuple (branch-info))

(defgeneric from-coordinate-list-to-branch-tuple (branch-info))

(defgeneric from-coordinate-list-to-region-tuple (branch-info))

(defgeneric exhaustive-search (obj))

(defgeneric uniform-search (obj))

(defgeneric is-exhausted-p (obj))

(defgeneric count-neighbors (op sol other-ops default))
