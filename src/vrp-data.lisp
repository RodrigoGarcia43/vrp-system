(in-package :vrp)

(defun make-basic-cvrp-problem (distance demands capacity
                                &optional (id 1))
  "Returns an instance of a basic-cvrp-problem from the distance-matrix, the demands and the capacity of the vehicles."
  (let* ((clients (loop for i from 1
                        for demand in demands
                        collecting (basic-cvrp-client i demand)))
         (depot (basic-depot)))
    ;; return an instance of the class
    (cvrp-problem :id id :clients clients :depot depot
                  :distance-matrix distance :capacity capacity)))

(defun make-finite-fleet-cvrp-problem (distance demands capacities
                                &optional (id 1))
  "Returns an instance of a finite-fleet-cvrp-problem from the distance-matrix, the demands and the capacities of the vehicles."
  (let* ((clients (loop for i from 1
                        for demand in demands
                        collecting (basic-cvrp-client i demand)))
         (vehicles (loop for i from 1
                         for capacity in capacities
                        collecting (cvrp-vehicle i capacity)))
         (depot (basic-depot)))
    ;; return an instance of the class
    (finite-fleet-cvrp-problem :id id
                               :clients clients
                               :depot depot
                               :distance-matrix distance
                               :fleet vehicles)))
