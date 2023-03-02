(in-package :vrp)

(defparameter *from-index-to-coords-indexer-function* 'index-to-coords-classic-indexer)

(defparameter *from-coords-to-indexer-indexer-function* 'coords-to-index-classic-indexer)

(defparameter *exploration-strategy-for-combinatorial-search* 'uniform-search)

(defparameter *lower-bound-for-subroute-length* 1)

(defparameter *upper-bound-for-subroute-length* 0)

(defparameter *subroute-length-classifications* '(short medium large))

(defparameter *subroute-length-upper-bounds* '(1 5 10))

(defparameter *exploration-strategy-for-intensification-phase* 'exhaustive-search)
(defparameter *statistical-technique* 'mean-technique)
(defparameter *exploration-proportion* 1/2)
(defparameter *intensification-proportion* 1/2)
(defparameter *max-number-of-sol-by-region* 30)
(defparameter *linear-constant-in-total-to-explore* 5)
(defparameter *clients-order-in-total-to-explore* 2)
