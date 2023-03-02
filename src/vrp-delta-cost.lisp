(in-package :vrp)

;;; A generic function to compute the delta-cost after an operation 
(defgeneric initialize-action-for-delta-cost-computation
    (solution problem action)
  (:documentation "Initializes the instance to make any future operation."))

;;; A generic function to compute the delta-cost after an operation 
(defmethod initialize-action-for-delta-cost-computation
    (solution problem action)
  "Do nothing, wait for the the auxiliary methods.")

;;; A generic function to compute the delta-cost after an operation 
(defgeneric compute-delta-cost-after
    (operation solution problem action)
  (:documentation "Computes the cost difference in the solution (working-copy) after the operation has been made."))

;; A generic function to compute the delta-cost after an operation 
(defmethod compute-delta-cost-after
    (operation solution problem action)
  "Do nothing, wait for the the auxiliary methods."
  nil)

;;; A generic function to compute the delta-cost after an operation 
(defgeneric undo-delta-cost-computation
    (operation solution problem action)
  (:documentation "Undoes the effect of the delta-cost-computation on the action.  It restores the action of the state it was before the delta-cost-computation."))

;; A generic function to compute the delta-cost after an operation 
(defmethod undo-delta-cost-computation
    (operation solution problem action)
  "Do nothing, wait for the the auxiliary methods."
  nil)

;; A generic function to finish the delta-cost computation
;; after all the operations have been processed 
(defgeneric finish-delta-cost-computation
    (working-copy problem action)
  (:documentation "Finishes the delta cost computation for a given solution, problem, actions, and strategy."))

;; A generic function to compute the delta-cost after an operation 
(defmethod finish-delta-cost-computation
    (working-copy problem action)
  "Do nothing, wait for the the auxiliary methods."
  nil)

;; A generic function to undo the effect of finishing
;; the delta-cost computation. It restores the
;; state of the action to before the call to finish
;; delta-cost computation.
(defgeneric undo-finish-delta-cost-computation
    (working-copy problem action)
  (:documentation "Undoes the effect of finish-delta-cost-computation on the action."))

;; A generic function to compute the delta-cost after an operation 
(defmethod undo-finish-delta-cost-computation
    (working-copy problem action)
  "Do nothing, wait for the the auxiliary methods."
  nil)

;; A generic function to finish the delta-cost computation
;; after all the operations have been processed 
(defgeneric delta-cost
    (operations working-copy problem action)
  (:documentation "Returns the delta-cost of the neighbor defined by the set of operations passed as the first argument"))

;;; A generic function to compute the delta-cost after an operation 
(defmethod initialize-action-for-delta-cost-computation :after
    (solution problem (action delta-distance-action))
  (setf (delta-distance action) 0))

;;; A generic function to compute the delta-cost after an operation 
(defmethod initialize-action-for-delta-cost-computation :after
    ((wc basic-working-copy)
     problem
     (action delta-basic-capacity-action))
  (let* ((number-of-clients (length (clients problem)))
         (routes (routes (solution wc))))
      (setf (delta-routes-feasibility action)
                 (make-array (1+ (+ number-of-clients
                                    (length routes)))
                             :initial-element 0))))

(defgeneric make-routes-feasibility-array (wc problem)
  (:documentation "Returns a routes-feasibility array. A routes-feasibility array is an array where in the position `pos' we have the basic-capacity of the route with id `pos'. The basic-capacity of a route is the difference between the capacity of the vehicle and the demand of the route.  We'll have as many possible routes as there are clients in the problem, because (potentially) we could create that number of new-routes through the operation add-route.

 Syntax:
    (make-routes-feasibility-array wc problem)
     wc      should be a working-copy.
     problem should be a cvrp problem."))

(defmethod make-routes-feasibility-array
    ((wc basic-working-copy)
     (problem capacity-problem))
  "Returns a routes-feasibility array. A routes-feasibility array is an array where in the position `pos' we have the basic-capacity of the route with id `pos'. The basic-capacity of a route is the difference between the capacity of the vehicle and the demand of the route.  We'll have as many possible routes as there are clients in the problem, because (potentially) we could create that number of new-routes through the operation add-route.

 Syntax:
    (make-routes-feasibility-array wc problem)
     wc      should be a working-copy.
     problem should be a problem with capacity."
   (let* ((solution (solution wc))
          (routes (routes solution))
          (number-of-clients (length (clients problem)))
          (capacity (capacity problem))

          ;; initialize the array with the appropriate size
          (result (make-array
                   (list (1+ (+ number-of-clients
                                (length routes))))
                   :initial-element capacity)))

     ;; let's update the values of each slot
     (loop for r in routes
           for r-id = (id r) 
           for availability = capacity
           doing (loop for c in (clients r)
                       doing (decf availability (demand c)))
           doing (setf (aref result r-id)
                       availability))
     ;; let's return result
     result))

(defmethod make-routes-feasibility-array
    ((wc basic-working-copy)
     (problem finite-fleet-problem))
  "Returns a routes-feasibility array. A routes-feasibility array is an array where in the position `pos' we have the basic-capacity of the route with id `pos'. The basic-capacity of a route is the difference between the capacity of the vehicle and the demand of the route.

In this case, we can only have as many routes as there are vehicles in the fleet.

 Syntax:
    (make-routes-feasibility-array wc problem)
     wc      should be a working-copy.
     problem should be a problem with a fleet."
   (let* ((solution (solution wc))
          (routes (routes solution))
          (number-of-vehicles (length (fleet problem)))

          ;; initialize the array with the appropriate size
          ;; we use 1+ because the routes-id are 1-based
          ;; and the array is 0-based.
          (result (make-array
                   (list (1+ number-of-vehicles)))))

     ;; let's update the values of each slot
     (loop for r in routes
           for r-id = (id r) 
           for availability = (capacity (vehicle r))
           doing (loop for c in (clients r)
                       doing (decf availability (demand c)))
           doing (setf (aref result r-id)
                       availability))
     ;; let's return result
     result))

;;; A generic function to compute the delta-cost after an operation 
(defmethod initialize-action-for-delta-cost-computation :after
    ((wc basic-working-copy)
     problem
     (action delta-basic-capacity-penalty-action))

  ;; sets the total-penalty to 0
  (setf (total-penalty action) 0)
  ;; initialize the original routes-feasibility array  
  (setf (original-routes-feasibility action)
        (make-routes-feasibility-array wc problem)))

;;; A generic function to compute the delta-cost after an operation 
(defmethod initialize-action-for-delta-cost-computation :after
    ((wc basic-working-copy)
     problem
     (action delta-basic-capacity-penalty-action*))

  ;; sets the total-penalty to 0
  (setf (total-penalty action) 0)
  ;; initialize the original routes-feasibility array  
  (setf (original-routes-feasibility action)
        (make-routes-feasibility-array wc problem)))

(defmethod compute-delta-cost-after :after
    ((operation operation-select-client)
     (working-copy basic-working-copy)
     (problem distance-problem)
     (action delta-distance-action))

  "If we have a selection we need to substract the distance from the selected-client to the next client and from the previous-client to the next one. We also need to add the distance from the previous-client to the next-client."

  (let* ((current-pos ;; the position of the current client
          (pos operation))
         (current-route ;; the route (id) of the current client
          (route operation))
         (current-coord ;; the coordinates of the current client
          (list current-route current-pos))
         (selected-client ;; the selected client
          (client-at current-coord working-copy))
         (previous-coord ;; the coordinates of the previous client
          (list current-route (1- current-pos)))
         (next-coord ;; the coordinates of the next client
          (list current-route (1+ current-pos)))
         (previous-client ;; the previous-client
          (client-at previous-coord working-copy))
         (next-client ;; the next client
          (client-at next-coord working-copy)))

    ;; remove the distance from previous to current
    (decf (delta-distance action)
          (get-distance-from-to previous-client
                                selected-client
                                problem))
    ;; substract the distance from current client to the next one
    (decf (delta-distance action)
          (get-distance-from-to selected-client
                                next-client
                                problem))
    ;; now add the distance from previous to next
    (incf (delta-distance action)
          (get-distance-from-to previous-client
                                next-client
                                problem))))

(defmethod compute-delta-cost-after :after
    ((operation operation-insert-client)
     (working-copy basic-working-copy)
     (problem distance-problem)
     (action delta-distance-action))
  "If we have an insertion we need to substract the distance from the previous client to the next client, and add the distance from the previous to the current and from the current to the next."


  (let* ((current-pos ;; the position where the insertion will take place
          (pos operation))
         (current-route ;; the route (id) of the insertion
          (route operation))
         (current-coord ;; the coordinates where the insertion
          ;; should occur.
          (list current-route current-pos))
         (current-client-op (operand operation))
         (current-client ;; the client that will be inserted
          (client-selected-at-operation current-client-op
                                        working-copy))
         (previous-coord ;; the coordinates of the previous client
          (list current-route (1- current-pos)))
         (next-coord ;; the coordinates of the next client
          current-coord)
         (previous-client ;; the previous-client
          (client-at previous-coord working-copy))
         (next-client ;; the next client
          ;; in this case it is the client at the pos
          ;; of the insertion.
          (client-at next-coord working-copy)))



    ;; remove the distance from previous to next
    (decf (delta-distance action)
          (get-distance-from-to previous-client
                                next-client
                                problem))
    ;; substract the distance from inserted client to the next one
    (incf (delta-distance action)
          (get-distance-from-to current-client
                                next-client
                                problem))
    ;; now add the distance from previous client to the inserted
    (incf (delta-distance action)
          (get-distance-from-to previous-client
                                current-client
                                problem))))

(defmethod compute-delta-cost-after :after
    ((operation operation-select-client)
     (working-copy basic-working-copy)
     problem
     (action delta-basic-capacity-action))

  "Increase the basic-feasibility of the route by the demand of the selected client."

  (let* ((pos (pos operation) ;; the position of the current client
          ;; in the route
           )
         (route ;; the route (id) of the current client
          (route operation))
         (selected-client (client-at (list route pos) working-copy)))

    ;; increment the feasibility of the route 
    (incf (aref (delta-routes-feasibility action) route)
          (demand selected-client))))

(defmethod compute-delta-cost-after :after
    ((operation operation-insert-client)
     (working-copy basic-working-copy)
     problem
     (action delta-basic-capacity-action))

  "Decrease the basic-feasibility of the route by the demand of the selected client."

  (let* ((route ;; the route (id) of the inserted client
          (route operation))
         (inserted-client
          (client-selected-at-operation (operand operation)
                                        working-copy)))


    ;; decrease the feasibility of the route 
    (decf (aref (delta-routes-feasibility action) route)
          (demand inserted-client))))

(defmacro make-before-method-for-delta-cost-after
    (class-list list-to-push stack-name &key what-to-log)
  (let* ((args (loop for arg in `(operation working-copy problem action)
                     for class in class-list
                     collect `(,arg ,class)))
         (format-str (format nil "Storing~a in ~a"
                             (aif what-to-log
                                  (format nil " ~a"it)
                                  "")
                             (car (last class-list)))))

    `(defbehavior compute-delta-cost-after :before
       ,args
       :log-str ,format-str
       (push (list ,@list-to-push)
             (,stack-name action)))))

(make-before-method-for-delta-cost-after
 (operation-select-client t t delta-distance-action*)
  ((delta-distance action))
  delta-distance-stack
  :what-to-log "(previous-distance)")

(make-before-method-for-delta-cost-after
  (operation-insert-client t t delta-distance-action*)
  ((delta-distance action))
  delta-distance-stack
  :what-to-log "(previous-distance)")

(make-before-method-for-delta-cost-after
   (operation-select-client t t delta-basic-capacity-action*)
   ((route operation)
    (aref (delta-routes-feasibility action) (route operation)))
    delta-routes-feasibility-stack
    :what-to-log "(route feasibility)")

(make-before-method-for-delta-cost-after
   (operation-insert-client t t delta-basic-capacity-action*)
   ((route operation)
    (aref (delta-routes-feasibility action) (route operation)))
    delta-routes-feasibility-stack
    :what-to-log "(route feasibility)")

(defmacro make-undo-delta-cost-computation
    (generic-function
     method-lambda-list
     lambda-list
     stack-name
     what-to-log
     &body body)

  `(defbehavior ,generic-function :after
     ,method-lambda-list
     :log-str ,what-to-log
     (destructuring-bind ,lambda-list (pop (,stack-name action))
         ,@body)))

(make-undo-delta-cost-computation
    undo-delta-cost-computation
    ((op operation-select-client)
     wc
     problem
     (action delta-distance-action*))    
    (old-distance) delta-distance-stack
    "Restoring delta-distance"
  (setf (delta-distance action) old-distance))

(make-undo-delta-cost-computation
    undo-delta-cost-computation
    ((op operation-insert-client)
     wc
     problem
     (action delta-distance-action*))    
    (old-distance) delta-distance-stack
    "Restoring delta-distance"
  (setf (delta-distance action) old-distance))

(make-undo-delta-cost-computation
    undo-delta-cost-computation
    ((op operation-select-client)
     wc
     problem
     (action delta-basic-capacity-action*))
    (route old-feasibility) delta-routes-feasibility-stack
     "Restoring basic route-feasibility"
  (setf (aref (delta-routes-feasibility action) route) old-feasibility))

(make-undo-delta-cost-computation
    undo-delta-cost-computation
    ((op operation-insert-client)
     wc
     problem
     (action delta-basic-capacity-action*))
    (route old-feasibility) delta-routes-feasibility-stack
     "Restoring basic route-feasibility"
  (setf (aref (delta-routes-feasibility action) route) old-feasibility))

(defbehavior finish-delta-cost-computation :after
    ((working-copy basic-working-copy)
     problem
     (action delta-basic-capacity-penalty-action))

    :log-str "Computing delta penalty related to basic-capacity"

  "Increase the basic-feasibility of the route by the demand of the selected client."

  (let* ((result (delta-distance action))
         (routes (routes working-copy))
         (penalty (penalty-factor action)))

    ;; here result is how much does the cost of this new solution
    ;; changes after the operations. In (delta-distance action)
    ;; we have the change in the distance travelled by the
    ;; vehicles. If there are not penalties this is just
    ;; the value we are looking for. Otherwise, we should
    ;; add or remove values according to the penalties.

    ;; now let's add the delta penalty cost
    (loop for r in routes
          for id = (id r)
          for current-feasibility =
              (aref (original-routes-feasibility action) id)
          for delta-feasibility =
              (aref (delta-routes-feasibility action) id)

          doing (cond
                  ;; they both are negative (It just got worse)
                  ((and (< current-feasibility 0)
                        (< delta-feasibility 0))
                   ;; if they both are negative just add more penalty

                   ;; (format t "Debug finish Dcost: cond 1, increasing in ~a~%"
                   ;;         (* (- penalty) delta-feasibility))

                   ;; add the penalty to total-penalty:
                   (incf (total-penalty action)
                         (* (- penalty) delta-feasibility))

                   (incf result (* (- penalty) delta-feasibility)))


                  ;; improvement but not enough :-[
                  ;; the improvement does not make the solution feasible
                  ;; then reduce the penalty in the difference
                  ((and (< current-feasibility 0)
                        (> delta-feasibility 0)
                        (> (abs current-feasibility)
                           (abs delta-feasibility)))
                   ;; in this case reduce the penalty
                   ;; from the result


                   ;; (format t
                   ;;         "Debug finish Dcost: cond 2, decreasing in ~a~%"
                   ;;         (* penalty
                   ;;            delta-feasibility))

                   ;; decf the penalty from total-penalty:
                   (decf (total-penalty action)
                         (* penalty
                            delta-feasibility))

                   (decf result (* penalty
                                   delta-feasibility)))

                  ;; ;; improvement makes the route feasible
                  ;; ;; decf the current penalty from the solution
                  ((and (< current-feasibility 0)
                        (> delta-feasibility 0)
                        (>= (abs delta-feasibility)
                            (abs current-feasibility)))
                   ;; in this case only reduce the previous penalty


                   ;; (format t
                   ;;         "Debug finish Dcost: cond 3, decreasing in ~a~%"
                   ;;         (* penalty
                   ;;            (- current-feasibility)))

                   (decf (total-penalty action)
                         (* penalty
                            (- current-feasibility)))

                   (decf result (* penalty
                                   (- current-feasibility))))

                  ;; TODO: NOW ADD THE OPPOSITE:
                  ;; WHEN THE CURRENT FEASIBILITY IS POSITIVE
                  ;; THE ONLY INTERESTING CASE IS WHEN
                  ;; THE DELTA-FEASIBILITY IS NEGATIvE AND GREATER THAN
                  ;; THE CURRENT-FEASIBILITY BECAUSE IN ANY OTHER CASE
                  ;; THERE IS NO NEED TO PENALYZE.

                  ((and (>= current-feasibility 0)
                        (< delta-feasibility 0)
                        (>= (abs delta-feasibility)
                            (abs current-feasibility)))
                   ;; in this case, increment the penalty in
                   ;; (delta-feasibility - current-feasibility)

                   (incf (total-penalty action)
                         (* (- penalty)
                            (+ current-feasibility
                               delta-feasibility)))

                   (incf result (* (- penalty)
                                   (+ current-feasibility
                                      delta-feasibility))))))
    ;; return result
    result))

(defbehavior finish-delta-cost-computation :after
    ((working-copy basic-working-copy)
     problem
     (action delta-basic-capacity-penalty-action*))

    :log-str "Computing delta penalty related to basic-capacity"

  "Increase the basic-feasibility of the route by the demand of the selected client."

  (let* ((result (delta-distance action))
         (routes (routes working-copy))
         (penalty (penalty-factor action)))

    ;; here result is how much does the cost of this new solution
    ;; changes after the operations. In (delta-distance action)
    ;; we have the change in the distance travelled by the
    ;; vehicles. If there are not penalties this is just
    ;; the value we are looking for. Otherwise, we should
    ;; add or remove values according to the penalties.

    ;; now let's add the delta penalty cost
    (loop for r in routes
          for id = (id r)
          for current-feasibility =
              (aref (original-routes-feasibility action) id)
          for delta-feasibility =
              (aref (delta-routes-feasibility action) id)

          doing (cond
                  ;; they both are negative (It just got worse)
                  ((and (< current-feasibility 0)
                        (< delta-feasibility 0))
                   ;; if they both are negative just add more penalty

                   ;; (format t "Debug finish Dcost: cond 1, increasing in ~a~%"
                   ;;         (* (- penalty) delta-feasibility))

                   ;; add the penalty to total-penalty:
                   (incf (total-penalty action)
                         (* (- penalty) delta-feasibility))

                   (incf result (* (- penalty) delta-feasibility)))


                  ;; improvement but not enough :-[
                  ;; the improvement does not make the solution feasible
                  ;; then reduce the penalty in the difference
                  ((and (< current-feasibility 0)
                        (> delta-feasibility 0)
                        (> (abs current-feasibility)
                           (abs delta-feasibility)))
                   ;; in this case reduce the penalty
                   ;; from the result


                   ;; (format t
                   ;;         "Debug finish Dcost: cond 2, decreasing in ~a~%"
                   ;;         (* penalty
                   ;;            delta-feasibility))

                   ;; decf the penalty from total-penalty:
                   (decf (total-penalty action)
                         (* penalty
                            delta-feasibility))

                   (decf result (* penalty
                                   delta-feasibility)))

                  ;; ;; improvement makes the route feasible
                  ;; ;; decf the current penalty from the solution
                  ((and (< current-feasibility 0)
                        (> delta-feasibility 0)
                        (>= (abs delta-feasibility)
                            (abs current-feasibility)))
                   ;; in this case only reduce the previous penalty


                   ;; (format t
                   ;;         "Debug finish Dcost: cond 3, decreasing in ~a~%"
                   ;;         (* penalty
                   ;;            (- current-feasibility)))

                   (decf (total-penalty action)
                         (* penalty
                            (- current-feasibility)))

                   (decf result (* penalty
                                   (- current-feasibility))))

                  ;; TODO: NOW ADD THE OPPOSITE:
                  ;; WHEN THE CURRENT FEASIBILITY IS POSITIVE
                  ;; THE ONLY INTERESTING CASE IS WHEN
                  ;; THE DELTA-FEASIBILITY IS NEGATIvE AND GREATER THAN
                  ;; THE CURRENT-FEASIBILITY BECAUSE IN ANY OTHER CASE
                  ;; THERE IS NO NEED TO PENALYZE.

                  ((and (>= current-feasibility 0)
                        (< delta-feasibility 0)
                        (>= (abs delta-feasibility)
                            (abs current-feasibility)))
                   ;; in this case, increment the penalty in
                   ;; (delta-feasibility - current-feasibility)

                   (incf (total-penalty action)
                         (* (- penalty)
                            (+ current-feasibility
                               delta-feasibility)))

                   (incf result (* (- penalty)
                                   (+ current-feasibility
                                      delta-feasibility))))))
    ;; return result
    result))

(defbehavior finish-delta-cost-computation :before
  ((working-copy t) (problem t) (action basic-penalty-action*))
  :log-str "Storing (total-penalty) in basic-penalty-action*"
  (push (list (total-penalty action))
        (total-penalty-stack action)))

(make-undo-delta-cost-computation
    undo-finish-delta-cost-computation
    (wc problem (action delta-basic-capacity-penalty-action*))
    (old-penalty) total-penalty-stack
     "Restoring total-penalty"
  (setf (total-penalty action) old-penalty))

(defmethod delta-cost ((operations list)
                       (working-copy basic-working-copy)
                       problem
                       action)
  "Returns the delta-cost of the neighbor defined by the set of operations passed as the first argument."

  ;; let's create a new working-copy with the same solution
  (let* ((wc (basic-working-copy (solution working-copy))))
    ;; let's initialize the wc
    (prepare-solution-for-neighborhood-exploration wc)
    ;; let's initialize the action
    (initialize-action-for-delta-cost-computation wc problem action)

    ;; let's compute delta-cost and simulate the ops
    (loop for op in operations
          do (compute-delta-cost-after op wc problem action)
          do (simulate-neighborhood-operation op wc))
    ;; let's finish the delta-cost computation
    (finish-delta-cost-computation wc problem action)


    ;; return the distance plus the total penalty
    (+ (delta-distance action)
       (total-penalty action))))

(defun delta-cost* (operations
                    working-copy
                    problem
                    action)
  "Returns the delta-cost of the neighbor defined by the set of operations passed as the first argument. In this case we leave the working-copy untouched after the delta-cost computation, and we can reuse it to compute the cost of another neighbor. We also assume that the action has already been initialized.
In this case action should be an action*, otherwise we won't be able to undo the operations."



  ;; let's create a new working-copy with the same solution
  (let* ((wc working-copy)
         (delta-cost 0))

    ;; let's compute delta-cost and simulate the ops
    (loop for op in operations
          do (compute-delta-cost-after op wc problem action)
          do (simulate-neighborhood-operation op wc))

    ;; let's finish the delta-cost computation
    (finish-delta-cost-computation wc problem action)

    ;; now let's store the value of the delta-cost
    (setf delta-cost (+ (delta-distance action)
                        (total-penalty action)))

    ;; now let's restore the working-copy to it's original state
    ;; first, let's undo the finish-delta-cost-computation
    (undo-finish-delta-cost-computation wc problem action)

    ;; now let's undo each of the operations
    (loop for op in (reverse operations)
          do (undo-delta-cost-computation op wc problem action)
          do (undo-neighborhood-operation op wc))
    ;; finally return the delta-cost
    delta-cost))

;;; A generic function to get the actual cost from an action.
(defgeneric get-cost-from-action (action)
  (:documentation "Computes the cost from a given action."))

(defmethod get-cost-from-action ((action basic-penalty-action))
  "Returns the cost stored in the given action."

  (+ (total-distance action)
     (total-penalty action)))

;;; A generic function to get the actual cost from an action.
(defgeneric get-delta-cost-from-action (action)
  (:documentation "Computes the delta cost from a given action."))

(defmethod get-delta-cost-from-action ((action basic-penalty-action))
  "Returns the cost stored in the given action."

  (+ (delta-distance action)
     (total-penalty action)))
