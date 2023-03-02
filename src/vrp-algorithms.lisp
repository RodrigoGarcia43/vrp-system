(in-package :vrp)

(defun scramble (list)
  ;; return the scrambled list.
  (sort list (lambda (x y)
               (declare (ignore x y))
               (> (random 2) 0))))

(defun is-better (sol1 sol2)
 ;; (format t "inside is-better with args~%   sol1: ~a~%   sol2: ~a~%"
 ;;         sol1 sol2)
 (if sol1
     (< (cost sol1) (cost sol2))))

(defun make-initial-solution-for-cvrp-deterministic (problem)

     (let* ((unassigned-clients (clone (clients problem)))
            (capacity (capacity problem))
            (current-capacity capacity)
            (route-id 1)
            (depot (basic-depot))
            (current-route (route-for-simulation
                            :id route-id
                            :depot depot
                            :vehicle (cvrp-vehicle
                                      route-id
                                      capacity
                                      0)
                            :clients nil))
            (current-solution (basic-cvrp-solution
                               :id 1 :cost 0 :routes nil))
            (stop-iterations nil)
            (selected-client nil)
            (feasible-solution-exists t))

       ;; let's check that there can be a feasible solution
       (loop for c in unassigned-clients
             while feasible-solution-exists
             doing (if (> (demand c) capacity)
                       (setf feasible-solution-exists nil)))

       (unless feasible-solution-exists
         (return-from make-initial-solution-for-cvrp-deterministic nil))

       (loop while (and feasible-solution-exists
                        unassigned-clients
                        ;; (<= route-id 5)
                        (not stop-iterations))

             do (setf selected-client
                      (loop for c in unassigned-clients
                            when (<= (demand c) current-capacity)
                            do (return c)))

             do (if selected-client
                    (then
                      (setf current-capacity
                            (- current-capacity
                               (demand selected-client)))
                      (push selected-client (clients current-route))
                      (setf unassigned-clients
                            (remove selected-client
                                    unassigned-clients
                                    :test #'obj=)))

                    (else ;; there are not more clients
                      ;; that can be insterted into this route
                      ;; so push the current route to the solution
                      (push current-route (routes current-solution))
                      ;; reset the current-capacity
                      (setf current-capacity capacity)
                      ;; increment the route id
                      (incf route-id)
                      ;; and create a new route with no clients
                      (setf current-route
                              (route-for-simulation
                               :id route-id
                               :depot depot
                               :vehicle (cvrp-vehicle
                                         route-id capacity 0)
                               :clients nil))))

             finally (push current-route
                           (routes current-solution)))
       ;; now let's reverse the routes
       (setf (routes current-solution)
                     (reverse (routes current-solution)))
       ;; finally return the solution
       current-solution))

(defun make-initial-solution-for-cvrp-random (problem)

     (let* ((unassigned-clients (clone (clients problem)))
            (capacity (capacity problem))
            (current-capacity capacity)
            (route-id 1)
            (depot (basic-depot))
            (current-route (route-for-simulation
                            :id route-id
                            :depot depot
                            :vehicle (cvrp-vehicle
                                      route-id
                                      capacity
                                      0)
                            :clients nil))
            (current-solution (basic-cvrp-solution
                               :id 1 :cost 0 :routes nil))
            (stop-iterations nil)
            (selected-client nil)
            (candidates nil)
            (feasible-solution-exists t))

       ;; let's check that there can be a feasible solution
       (loop for c in unassigned-clients
             while feasible-solution-exists
             doing (if (> (demand c) capacity)
                       (setf feasible-solution-exists nil)))

       (unless feasible-solution-exists
         (return-from make-initial-solution-for-cvrp-random nil))

       (loop while (and feasible-solution-exists
                        unassigned-clients
                        ;; (<= route-id 5)
                        (not stop-iterations))

             do (setf candidates (loop for c in unassigned-clients
                                       when (<= (demand c) current-capacity)
                                       collect c))

             do (setf selected-client
                      (if (> (length candidates) 0)
                          (nth (random (length candidates)) candidates)))

             do (if selected-client
                    (then
                      (setf current-capacity
                            (- current-capacity
                               (demand selected-client)))
                      (push selected-client (clients current-route))
                      (setf unassigned-clients
                            (remove selected-client
                                    unassigned-clients
                                    :test #'obj=)))

                    (else ;; there are not more clients
                      ;; that can be insterted into this route
                      ;; so push the current route to the solution
                      (push current-route (routes current-solution))
                      ;; reset the current-capacity
                      (setf current-capacity capacity)
                      ;; increment the route id
                      (incf route-id)
                      ;; and create a new route with no clients
                      (setf current-route
                              (route-for-simulation
                               :id route-id
                               :depot depot
                               :vehicle (cvrp-vehicle
                                         route-id capacity 0)
                               :clients nil))))

             finally (push current-route
                           (routes current-solution)))
       ;; now let's reverse the routes
       (setf (routes current-solution)
                     (reverse (routes current-solution)))
       ;; finally return the solution
       current-solution))

(defun make-initial-random-cvrp-solution (problem &optional
                                                    (probability 0.5))

     (let* ((unassigned-clients (clone (clients problem)))
            (route-id 1)
            (depot (basic-depot))
            (capacity (capacity problem))
            (current-route (route-for-simulation
                            :id route-id
                            :depot depot
                            :vehicle (cvrp-vehicle
                                      route-id
                                      capacity
                                      0)
                            :clients nil))
            (current-solution (basic-cvrp-solution
                               :id 1 :cost 0 :routes nil))
            (stop-iterations nil)
            (selected-client nil)
            (selected-index 0))


       (loop while (and unassigned-clients
                        (not stop-iterations))

             do (progn
                  (setf selected-index
                        (random (length unassigned-clients)))
                  (setf selected-client
                        (nth selected-index unassigned-clients))
                  (setf unassigned-clients
                        (remove selected-client
                                unassigned-clients
                                :test 'obj=)))

             do (if (<= (random 1.0) probability)

                    (then ;; add it to the current route
                      (push selected-client (clients current-route)))

                    (else ;; create a new route
                      (push current-route (routes current-solution))
                      ;; increment the route id
                      (incf route-id)
                      ;; and create a new route with no clients
                      (setf current-route
                              (route-for-simulation
                               :id route-id
                               :depot depot
                               :vehicle (cvrp-vehicle
                                         route-id capacity 0)
                               :clients nil))
                      ;; and push it there
                      (push selected-client (clients current-route))
                      ))

             finally (push current-route
                           (routes current-solution)))
       ;; now let's reverse the routes
       (setf (routes current-solution)
                     (reverse (routes current-solution)))
       ;; finally return the solution
       current-solution))

(defun make-initial-solution-for-finite-fleet-cvrp-deterministic
    (problem)

     (let* ((unassigned-clients (clone (clients problem)))
            (fleet  (fleet problem))
            (depot (basic-depot))
            (routes (loop for vehicle in fleet
                          for route-id from 1
                          collect (route-for-simulation
                                   :id route-id
                                   :depot depot
                                   :vehicle vehicle
                                   :clients nil)))
            (current-solution (basic-solution
                               :id 1 :cost 0 :routes routes)))

       ;; let's assign the clients
       (loop while unassigned-clients
             do (loop for r in (routes current-solution)
                      while unassigned-clients
                      for c = (pop unassigned-clients)
                      do (push c (clients r))))
       ;; return current-solution
       current-solution))

(defun make-initial-solution-for-finite-fleet-cvrp-random
    (problem)
  (let* ((unassigned-clients (clone (clients problem)))
         (fleet  (fleet problem))
         (depot (basic-depot))
         (routes (loop for vehicle in fleet
                       for route-id from 1
                       collect (route-for-simulation
                                :id route-id
                                :depot depot
                                :vehicle vehicle
                                :clients nil)))
         (number-of-routes (length routes))
         (current-solution (basic-solution
                            :id 1 :cost 0 :routes routes)))

    ;; let's assign the clients
    (loop while unassigned-clients
          for pos = (random number-of-routes)
          for c = (pop unassigned-clients)
          ;; do (format t "C: ~a, pos: ~a~%" c pos)
          do (push c (clients (nth pos (routes current-solution))))
          )
    ;; return current-solution
    current-solution))

;; Descent Neighboorhood Search
(defun descent-neighborhood-search
    (problem solution criterion
     &key max-iter action (print-mod 1))
  "Solves the VRP problem using a DNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (let* ((better-solution
          (funcall criterion solution problem action))
         (current-solution nil)
         (neighboorhoods-used 1))
    ;; if better-solution is non nil, then
    ;; we have a better neighboor.
    (loop while (and better-solution
                     (<= neighboorhoods-used max-iter))
          ;; update the current solution
          do (setf current-solution better-solution)
          ;; compute the new better-solution
          do (setf better-solution
                   (funcall criterion
                            current-solution
                            problem
                            action))

          ;; for debugging purposes
          (if (= (mod neighboorhoods-used print-mod) 0)
              (then
                (format t "Debug inside DNS.  Iteration ~a"
                        neighboorhoods-used)
                (if better-solution
                    (format t ", cost: ~a~%" (cost better-solution))
                    (format t ".  Best solution found.~%"))))

          ;; end of debug.

          ;; increment the number of neighboorhoods-used
          do (incf neighboorhoods-used))
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-fn
    (problem solution criteria
     &key max-iter action (print-mod 1))
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* ((current-index 0)
         (number-of-criteria (length criteria))
         (current-criterion nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)
         )

    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))

          ;; let's set the current criterion
          do (setf current-criterion (nth current-index criteria))          
          ;; let's explore with the current criteria

          ;; for debugging purposes
          do (format t "Debug inside VNS.  Iteration ~a, criterion ~a"
                     neighboorhoods-used current-index)

          do (setf better-solution
                   (funcall current-criterion
                            current-solution problem action))

          do (if better-solution
                 (then 
                   ;; we found a better solution
                   ;; update the current-solution
                   (setf current-solution better-solution)
                   ;; set the number-of-criteria to 0
                   (setf current-index 0))
                 (else
                   ;; use the next criterion
                   (incf current-index)))

          ;; ;; ;;{{{ for debugging purposes
          ;; do (format t "Debug inside VNS.  Iteration ~a, criterion ~a"
          ;;         neighboorhoods-used current-index)
          do (if better-solution
                 (format t ", cost: ~a~%" (cost better-solution))
                 (format t ".  Best solution found.~%"))
          ;; ;; ;; end of debug.

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)

          ;; ;; ;; for debugging purposes
          ;; (format t "At the end of the while: ~a, ~a~%"
          ;;         current-index number-of-criteria
          ;;         ;; (< current-index number-of-criteria)
          ;;         ;; (<= neighboorhoods-used max-iter)
          ;;         )
          ;; ;; ;; end of debugging purposes ;;}}}

          )
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-code
    (problem solution criteria
     &key
       (search-strategy +exhaustive-search-strategy+)
       (selection-strategy +random-improvement+)
       max-iter action (print-mod 1))
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* ((current-index 0)
         (number-of-criteria (length criteria))
         ;; ;; this is to make only one call to
         ;; make-neighborhood-criterion
         (criteria-as-functions (mapcar (lambda (x)
                                          (make-neighborhood-criterion
                                           x
                                           search-strategy
                                           selection-strategy))
                                        criteria))
         (current-criterion nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)



         )

    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))

          ;; let's set the current-criterion
          do (setf current-criterion (nth current-index
                                          criteria-as-functions))


          ;; let's explore with the current criteria

          ;; for debugging purposes
          do (format t "Debug inside VNS.  Iteration ~a, criterion ~a"
                      neighboorhoods-used current-index)

          do (setf better-solution
                   (funcall current-criterion
                            current-solution problem action))

          do (if better-solution
                 (then 
                   ;; we found a better solution
                   ;; update the current-solution
                   (setf current-solution better-solution)
                   ;; set the number-of-criteria to 0
                   (setf current-index 0))
                 (else
                   ;; use the next criterion
                   (incf current-index)))

          ;; ;; ;;{{{ for debugging purposes
          ;; do (format t "Debug inside VNS.  Iteration ~a, criterion ~a"
          ;;         neighboorhoods-used current-index)
          do (if better-solution
                 (then
                   (format t ", cost: ~a~%" (cost better-solution))
                   ;; (pp-solution better-solution t) (terpri)
                   )
                 (else
                   (format t ".  Best solution found.~%")))
          ;; ;; ;; end of debug.

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)

          ;; ;; ;; for debugging purposes
          ;; (format t "At the end of the while: ~a, ~a~%"
          ;;         current-index number-of-criteria
          ;;         ;; (< current-index number-of-criteria)
          ;;         ;; (<= neighboorhoods-used max-iter)
          ;;         )
          ;; ;; ;; end of debugging purposes ;;}}}

          )
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

(defun vns-fn-no-output
    (problem solution criteria
     &key max-iter action)
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."

  (let* ((current-index 0)
         (number-of-criteria (length criteria))
         (current-criterion nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)
         )

    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))

          ;; let's set the current criterion
          do (setf current-criterion
                   (nth current-index criteria))

          ;; let's explore with the current criteria


          do (setf better-solution
                   (funcall current-criterion
                            current-solution problem action))

          do (if better-solution
                 (then 
                   ;; we found a better solution
                   ;; update the current-solution
                   (setf current-solution better-solution)
                   ;; set the number-of-criteria to 0
                   (setf current-index 0))
                 (else
                   ;; use the next criterion
                  (incf current-index)))
          ;; in any case, increment the number of
          ;; neighboorhoods-used
          do (incf neighboorhoods-used))

    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-code-no-output
    (problem solution criteria
     &key
       (search-strategy +exhaustive-search-strategy+)
       (selection-strategy +random-improvement+)
       max-iter action (print-mod 1))
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* ((current-index 0)
         (number-of-criteria (length criteria))
         ;; ;; this is to make only one call to
         ;; make-neighborhood-criterion
         (criteria-as-functions (mapcar (lambda (x)
                                          (make-neighborhood-criterion
                                           x
                                           search-strategy
                                           selection-strategy))
                                        criteria))
         (current-criterion nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)



         )

    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))

          ;; let's set the current-criterion
          do (setf current-criterion (nth current-index
                                          criteria-as-functions))


          ;; let's explore with the current criteria

          do (setf better-solution
                   (funcall current-criterion
                            current-solution problem action))

          do (if better-solution
                 (then 
                   ;; we found a better solution
                   ;; update the current-solution
                   (setf current-solution better-solution)
                   ;; set the number-of-criteria to 0
                   (setf current-index 0))
                 (else
                   ;; use the next criterion
                   (incf current-index)))
          ;; in any case, increment the number of
          ;; neighboorhoods-used
          do (incf neighboorhoods-used)

          )
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-code-bar
    (problem solution criteria
     &key
       (search-strategy +exhaustive-search-strategy+)
       (selection-strategy +random-improvement+)
       max-iter action (print-mod 1)
       (dots-canvas 20))
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* ((current-index 0)
         (number-of-criteria (length criteria))
                  ;; ;; this is to make only one call to
         ;; make-neighborhood-criterion
         (criteria-as-functions (mapcar (lambda (x)
                                          (make-neighborhood-criterion
                                           x
                                           search-strategy
                                           selection-strategy))
                                        criteria))
         (current-criterion nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)
         ;; (progress-bar-increment (round (/ max-iter dots-canvas)))
         (progress-bar-increment (round dots-canvas))
         )

    ;; this is for the progress-bar
    (format t "     Running VNS ")

    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))

          ;; let's build the criterion
          do (setf current-criterion (nth current-index
                                          criteria-as-functions))


          ;; let's explore with the current criteria

          do (setf better-solution
                   (funcall current-criterion
                            current-solution problem action))

          do (if better-solution
                 (then 
                   ;; we found a better solution
                   ;; update the current-solution
                   (setf current-solution better-solution)
                   ;; set the number-of-criteria to 0
                   (setf current-index 0))
                 (else
                   ;; use the next criterion
                   (incf current-index)))

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)

          ;; display the dots line
          do (if (= (mod neighboorhoods-used progress-bar-increment) 0)
                 (format t "."))
          )
    ;; here we just finished the iterations

    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-fn-bar
    (problem solution criteria
     &key
       max-iter action (print-mod 1)
       (dots-canvas 20))
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* ((current-index 0)
         (number-of-criteria (length criteria))
         (current-criterion nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)
         ;; (progress-bar-increment (round (/ max-iter dots-canvas)))
         (progress-bar-increment (round dots-canvas))
         )

    ;; this is for the progress-bar
    (format t "     Running VNS ")

    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))

          ;; let's build the criterion
          do (setf current-criterion (nth current-index
                                          criteria))


          ;; let's explore with the current criteria

          do (setf better-solution
                   (funcall current-criterion
                            current-solution problem action))

          do (if better-solution
                 (then 
                   ;; we found a better solution
                   ;; update the current-solution
                   (setf current-solution better-solution)
                   ;; set the number-of-criteria to 0
                   (setf current-index 0))
                 (else
                   ;; use the next criterion
                   (incf current-index)))

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)

          ;; display the dots line
          do (if (= (mod neighboorhoods-used progress-bar-increment) 0)
                 (format t "."))
          )
    ;; here we just finished the iterations

    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-code-scrambling
    (problem solution criteria
     &key
       (search-strategy +exhaustive-search-strategy+)
       (selection-strategy +random-improvement+)
       max-iter action (print-mod 1))
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* ((current-index 0)
         (number-of-criteria (length criteria))
         ;; ;; this is to make only one call to
         ;; make-neighborhood-criterion
         (criteria-as-functions (loop for i from 1
                                      for x in criteria
                                      collecting
                                      (list i (make-neighborhood-criterion
                                               x
                                               search-strategy
                                               selection-strategy))))
         (current-criterion nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)
         )

    ;; first let's scramble the list
    (setf criteria-as-functions
          (scramble criteria-as-functions))

    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))

          ;; let's set the current-criterion
          do (setf current-criterion (second
                                      (nth current-index
                                           criteria-as-functions)))


          ;; let's explore with the current criteria

          ;; for debugging purposes
          do (format t "Debug. Iteration ~4d, criterion ~2d (~2d/~2d)"
                     neighboorhoods-used
                     (first (nth current-index
                                 criteria-as-functions))
                     (1+ current-index)
                     number-of-criteria)

          do (setf better-solution
                   (funcall current-criterion
                            current-solution problem action))

          do (if better-solution
                 (then 
                   ;; we found a better solution
                   ;; update the current-solution
                   (setf current-solution better-solution)
                   ;; set the number-of-criteria to 0
                   (setf current-index 0)
                   ;; scramble the list with the criteria
                   (setf criteria-as-functions
                         (scramble criteria-as-functions))
                   )
                 (else
                   ;; use the next criterion
                   (incf current-index)))

          ;; ;; ;;{{{ for debugging purposes
          ;; do (format t "Debug inside VNS.  Iteration ~a, criterion ~a"
          ;;         neighboorhoods-used current-index)
          do (if better-solution
                 (then
                   (format t ", cost: ~a~%" (cost better-solution))
                   ;; (pp-solution better-solution t) (terpri)
                   )
                 (else
                   (format t ". Best solution found.~%")))
          ;; ;; ;; end of debug.

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)

          ;; ;; ;; for debugging purposes
          ;; (format t "At the end of the while: ~a, ~a~%"
          ;;         current-index number-of-criteria
          ;;         ;; (< current-index number-of-criteria)
          ;;         ;; (<= neighboorhoods-used max-iter)
          ;;         )
          ;; ;; ;; end of debugging purposes ;;}}}

          )
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-shake
    (problem solution criteria
     &key
       (descent-search-strategy +exhaustive-search-strategy+)
       (descent-selection-strategy +best-improvement+)
       (shake-search (jump-around-search-strategy 5))
       (shake-selection (jump-around-return-last-neighbor))
       (max-iter 100)
       action (print-mod 1))
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* ((current-index 0)
         (number-of-criteria (length criteria))
         (current-criterion-code nil)
         (current-criterion nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)
         (x_prime nil)
         (initial-best-delta-cost 0)
         )

    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))
          for better-solution-found-in-shake = nil 

          ;; let's set the current criterion
          do (setf current-criterion-code (nth current-index criteria))
          ;; let's build the criterion for shaking
          do (setf current-criterion
                   (make-neighborhood-criterion
                    current-criterion-code
                    shake-search
                    shake-selection))

          ;; let's shake the current solution
          ;; we expect two values because we also need
          ;; to know the initial-best-delta-cost
          ;; to pass it to the second step.
          do (setf (values x_prime initial-best-delta-cost)
                   (funcall current-criterion
                            current-solution problem action))

          ;; (format t "     x_prime? ~a~%" (not (null x_prime)))


          ;; for debugging purposes
          do (format t "Debug inside VNS.  Iteration ~a, criterion ~a.
     Best:    ~a
     Shaking: ~a~%"
                     neighboorhoods-used
                     current-index
                     (cost current-solution)
                     (cost x_prime))

      do (format t "     initial-delta-cost: ~a~%"
                     initial-best-delta-cost)


          do (if (< initial-best-delta-cost 0)
                 (then ;; we found a better solution in the shake
                   ;; so, let's update.
                   (setf better-solution-found-in-shake t)
                   (setf current-solution x_prime)
                   (format t "     Found a better solution in shake with cost ~a.~%"
                           (cost current-solution))))

          ;; the following should be done
          ;; only if no better-solution-found-in-shake


          ;; let's build the criterion for shake
          do (unless better-solution-found-in-shake
               (format t "     Creating criterion to exploit shaked solution~%")
               ;; let's see what happens if we exploit with
               ;; a different criterion
               (setf current-criterion-code
                     (nth (mod (1+ current-index)
                               (length criteria))
                          criteria))

               (setf current-criterion
                     (make-neighborhood-criterion
                      current-criterion-code
                      descent-search-strategy
                      descent-selection-strategy)))


          ;; let's explore with the current criterion
          ;; starting from x_prime, but using
          ;; initial-best-delta-cost
          do (unless better-solution-found-in-shake
               (format t "     Exploiting shaked solution~%")
               (setf (values better-solution)
                    (funcall current-criterion
                             x_prime
                             problem
                             action
                             (- initial-best-delta-cost))))

          ;; for debugging purposes
          do (unless better-solution-found-in-shake
               (format t "     Descent"))


          do (unless better-solution-found-in-shake
               (if better-solution
                  (then 
                    ;; we found a better solution
                    ;; update the current-solution
                    (setf current-solution better-solution)
                    ;; set the number-of-criteria to 0
                    (setf current-index 0)
                    ;; this is for debug
                    (format t ": ~a~%" (cost better-solution)))
                  (else
                    ;; use the next criterion
                    (incf current-index)
                    ;; this is for debug
                    (format t ".  No improvement.~%"))))

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)

          ;; ;; ;; for debugging
          (format t "     At the end of loop: cost of current-sol: ~a~%"
                  (cost current-solution))

          ;; ;; ;; for debugging purposes
          ;; (format t "At the end of the while: ~a, ~a~%"
          ;;         current-index number-of-criteria
          ;;         ;; (< current-index number-of-criteria)
          ;;         ;; (<= neighboorhoods-used max-iter)
          ;;         )
          ;; ;; ;; end of debugging purposes ;;}}}

          )
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-shake
    (problem solution criteria
     &key
       (search-strategy +exhaustive-search-strategy+)
       (selection-strategy +best-improvement+)
       (shake-search (jump-around-search-strategy 5))
       (shake-selection (jump-around-return-last-neighbor))
       (list-returned-by-vns nil)
       (inner-search-max-iter 100)
       (max-iter 100)

       action
       (print-mod 1)
       )
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* (;; (current-index 0)
         (number-of-criteria (length criteria))
         (current-criterion-code nil)
         (current-criterion nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)
         ;; (best-solution (clone solution))
         (x_prime nil)
         (initial-best-delta-cost 0)
         (shake-index 0)
         (neighboorhoods-used-in-vns 0)
         (optimum-found-in-vns nil)
         )

    (loop while (and (< shake-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))
          for better-solution-found-in-shake = nil 
          ;; let's set the current criterion
          do (setf current-criterion-code (nth shake-index criteria))
          ;; let's build the criterion for shaking
          do (setf current-criterion
                   (make-neighborhood-criterion
                    current-criterion-code
                    shake-search
                    shake-selection))

          ;; let's shake the current solution
          ;; we expect two values because we also need
          ;; to know the initial-best-delta-cost
          ;; to pass it to the second step.
          do (setf (values x_prime initial-best-delta-cost)
                   (funcall current-criterion
                            current-solution problem action))

          ;; (format t "     x_prime? ~a~%" (not (null x_prime)))


          ;; for debugging purposes
          do (format t "Iteration ~a, shake criterion ~a.
     Best:    ~a (~a)~%"
                     neighboorhoods-used
                     shake-index
                     (cost current-solution)
                     (cost x_prime))

      ;; do (format t "     initial-delta-cost: ~a~%"
      ;;                initial-best-delta-cost)


          do (if (< initial-best-delta-cost 0)
                 (then ;; we found a better solution in the shake
                   ;; so, let's update.
                   (setf better-solution-found-in-shake t)
                   (setf current-solution x_prime)
                   (format t "     Found a better solution in shake with cost ~a.~%"
                           (cost current-solution))))

          ;; the following should be done
          ;; only if no better-solution-found-in-shake


          ;; let's call vns on the shaked solution
          do (unless better-solution-found-in-shake
               ;; let's call vns on the shaked solution
               (setf list-returned-by-vns
                     (vns-code-bar
                      problem x_prime criteria
                      :search-strategy search-strategy
                      :selection-strategy selection-strategy
                      :max-iter inner-search-max-iter
                      :action action))
               ;; let's get the results
               (setf better-solution (first list-returned-by-vns)
                     neighboorhoods-used-in-vns (nth 1 list-returned-by-vns)
                     optimum-found-in-vns (third list-returned-by-vns)))

             ;; let's see if the vns solution is better than x
          do (unless better-solution-found-in-shake
               (if better-solution
                   (then
                     ;; let's see if the solution is better
                     (if (< (cost better-solution)
                            (cost current-solution))
                         (then
                          ;; we found a better solution
                          ;; update the current-solution
                          (setf current-solution better-solution)
                          ;; set the number-of-criteria to 0
                          (setf shake-index 0)
                          ;; this is for debug
                          (format t ": ~a~%" (cost better-solution)))
                         (else
                           ;; there is no improvement
                           ;; use the next criterion for the shake
                           (incf shake-index)
                           ;; this is for debug
                           (format t ".  :-(~%"))))))

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)

          ;; ;; ;; for debugging
          ;; (format t "     At the end of loop: cost of current-sol: ~a~%"
          ;;         (cost current-solution))

          ;; ;; ;; for debugging purposes
          ;; (format t "At the end of the while: ~a, ~a~%"
          ;;         current-index number-of-criteria
          ;;         ;; (< current-index number-of-criteria)
          ;;         ;; (<= neighboorhoods-used max-iter)
          ;;         )
          ;; ;; ;; end of debugging purposes ;;}}}

          )
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-shake-smart
    (problem solution criteria
     &key
       (search-strategy +exhaustive-search-strategy+)
       (selection-strategy +best-improvement+)
       (shake-search (jump-around-search-strategy 5))
       (shake-selection (jump-around-return-last-neighbor))
       (list-returned-by-vns nil)
       (inner-search-max-iter 100)
       (max-iter 100)

       action
       action-for-shake
       (print-mod 1)
       )
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* (;; (current-index 0)
         (number-of-criteria (length criteria))
         ;; the criteria to be passed to the vns-fn-bar
         (exhaustive-criteria-code (mapcar (lambda (x)
                                             (make-neighborhood-criterion
                                              x
                                              search-strategy
                                              selection-strategy))
                                           criteria))
         ;; the criteria to use for the shake
         (random-criteria-code (mapcar (lambda (x)
                                             (make-neighborhood-criterion
                                              x
                                              shake-search
                                              shake-selection))
                                           criteria))
         (current-criterion-for-shake nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)
         ;; (best-solution (clone solution))
         (x_prime nil)
         (initial-best-delta-cost 0)
         (shake-index 0)
         (neighboorhoods-used-in-vns 0)
         (optimum-found-in-vns nil)
         )

    (loop while (and (< shake-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))
          for better-solution-found-in-shake = nil 
          ;; let's get the criterion for shaking
          do (setf current-criterion-for-shake
                   (nth shake-index random-criteria-code))

          ;; let's shake the current solution
          ;; we expect two values because we also need
          ;; to know the initial-best-delta-cost
          ;; to pass it to the second step.

          ;; do (initialize-action-for-delta-cost-computation
          ;;     current-solution
          ;;     problem
          ;;     action-for-shake)

          do (setf (values x_prime initial-best-delta-cost)
                   (funcall current-criterion-for-shake
                            current-solution
                            problem
                            action-for-shake))

          ;; (format t "    debug: distance-stack after shake: ~a~%"
          ;;         (length (delta-distance-stack action)))


          ;; for debugging purposes
          do (format t "Iteration ~a, shake criterion ~a.
     Best:    ~a (~a)~%"
                     neighboorhoods-used
                     shake-index
                     (cost current-solution)
                     (cost x_prime))

      ;; do (format t "     initial-delta-cost: ~a~%"
      ;;                initial-best-delta-cost)


          do (if (< initial-best-delta-cost 0)
                 (then ;; we found a better solution in the shake
                   ;; so, let's update.
                   (setf better-solution-found-in-shake t)
                   (setf current-solution x_prime)
                   (format t "     Found a better solution in shake with cost ~a.~%"
                           (cost current-solution))))

          ;; the following should be done
          ;; only if no better-solution-found-in-shake


          ;; let's call vns on the shaked solution
          do (unless better-solution-found-in-shake
               ;; let's call vns on the shaked solution
               (setf list-returned-by-vns
                     (vns-fn-bar
                      problem x_prime
                      exhaustive-criteria-code
                      :max-iter inner-search-max-iter
                      :action action
                      :dots-canvas 1
                      ))
               ;; let's get the results
               (setf better-solution
                     (first list-returned-by-vns))
               (setf neighboorhoods-used-in-vns
                     (nth 1 list-returned-by-vns))
               (setf optimum-found-in-vns
                     (third list-returned-by-vns)))

             ;; let's see if the vns solution is better than x
          do (unless better-solution-found-in-shake
               (if better-solution
                   (then
                     ;; let's see if the solution is better
                     (if (< (cost better-solution)
                            (cost current-solution))
                         (then
                          ;; we found a better solution
                          ;; update the current-solution
                          (setf current-solution better-solution)
                          ;; set the number-of-criteria to 0
                          (setf shake-index 0)
                          ;; this is for debug
                          (format t ": ~a~%" (cost better-solution)))
                         (else
                           ;; there is no improvement
                           ;; use the next criterion for the shake
                           (incf shake-index)
                           ;; this is for debug
                           (format t ".  :-(~%"))))))

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)

          ;; ;; ;; for debugging
          ;; (format t "     At the end of loop: cost of current-sol: ~a~%"
          ;;         (cost current-solution))

          ;; ;; ;; for debugging purposes
          ;; (format t "At the end of the while: ~a, ~a~%"
          ;;         current-index number-of-criteria
          ;;         ;; (< current-index number-of-criteria)
          ;;         ;; (<= neighboorhoods-used max-iter)
          ;;         )
          ;; ;; ;; end of debugging purposes ;;}}}

          )
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-shake-smart-no-output
    (problem solution criteria
     &key
       (search-strategy +exhaustive-search-strategy+)
       (selection-strategy +best-improvement+)
       (shake-search (jump-around-search-strategy 5))
       (shake-selection (jump-around-return-last-neighbor))
       (list-returned-by-vns nil)
       (inner-search-max-iter 100)
       (max-iter 100)

       action
       action-for-shake
       )
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."

  (let* (;; (current-index 0)
         (number-of-criteria (length criteria))
         ;; the criteria to be passed to the vns-fn-bar
         (exhaustive-criteria-code (mapcar (lambda (x)
                                             (make-neighborhood-criterion
                                              x
                                              search-strategy
                                              selection-strategy))
                                           criteria))
         ;; the criteria to use for the shake
         (random-criteria-code (mapcar (lambda (x)
                                             (make-neighborhood-criterion
                                              x
                                              shake-search
                                              shake-selection))
                                           criteria))
         (current-criterion-for-shake nil)
         (better-solution nil)
         (current-solution solution)
         (neighboorhoods-used 1)
         ;; (best-solution (clone solution))
         (x_prime nil)
         (initial-best-delta-cost 0)
         (shake-index 0)
         (neighboorhoods-used-in-vns 0)
         (optimum-found-in-vns nil)
         )

    (loop while (and (< shake-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))
          for better-solution-found-in-shake = nil 
          ;; let's get the criterion for shaking
          do (setf current-criterion-for-shake
                   (nth shake-index random-criteria-code))

          ;; let's shake the current solution
          ;; we expect two values because we also need
          ;; to know the initial-best-delta-cost
          ;; to pass it to the second step.

          do (setf (values x_prime initial-best-delta-cost)
                   (funcall current-criterion-for-shake
                            current-solution
                            problem
                            action-for-shake))


          do (if (< initial-best-delta-cost 0)
                 (then ;; we found a better solution in the shake
                   ;; so, let's update.
                   (setf better-solution-found-in-shake t)
                   (setf current-solution x_prime)))

          ;; the following should be done
          ;; only if no better-solution-found-in-shake

          ;; let's call vns on the shaked solution
          do (unless better-solution-found-in-shake
               ;; let's call vns on the shaked solution
               (setf list-returned-by-vns
                     (vns-fn-no-output
                      problem x_prime
                      exhaustive-criteria-code
                      :max-iter inner-search-max-iter
                      :action action))
               ;; let's get the results
               (setf better-solution
                     (first list-returned-by-vns))
               (setf neighboorhoods-used-in-vns
                     (nth 1 list-returned-by-vns))
               (setf optimum-found-in-vns
                     (third list-returned-by-vns)))

             ;; let's see if the vns solution is better than x
          do (unless better-solution-found-in-shake
               (if better-solution
                   (then
                     ;; let's see if the solution is better
                     (if (< (cost better-solution)
                            (cost current-solution))
                         (then
                          ;; we found a better solution
                          ;; update the current-solution
                          (setf current-solution better-solution)
                          ;; set the number-of-criteria to 0
                          (setf shake-index 0))
                         (else
                           ;; there is no improvement
                           ;; use the next criterion for the shake
                           (incf shake-index))))))

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)
          )
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-vrp-system
    (problem criteria graph
     &key max-iter (print-mod 1))
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* ((current-index 0)
         (number-of-criteria (length criteria))
         (current-criterion nil)
         (better-solution nil)
         (current-solution (neigh-tree-solution (solution-track graph)))
         (neighboorhoods-used 1)
         )

    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))

          ;; let's set the current criterion
          do (setf current-criterion (nth current-index criteria))          
          ;; let's explore with the current criteria

          ;; for debugging purposes
          do (format t "Debug inside VNS.  Iteration ~a, criterion ~a"
                     neighboorhoods-used current-index)

          do (setf better-solution
                   (funcall current-criterion
                            current-solution problem graph))

          do (if better-solution
                 (then 
                   ;; we found a better solution
                   ;; update the current-solution
                   (setf current-solution better-solution)
                   ;; set the number-of-criteria to 0
                   (setf current-index 0))
                 (else
                   ;; use the next criterion
                   (incf current-index)))

          ;; ;; ;;{{{ for debugging purposes
          ;; do (format t "Debug inside VNS.  Iteration ~a, criterion ~a"
          ;;         neighboorhoods-used current-index)
          do (if better-solution
                 (format t ", cost: ~a~%" (cost better-solution))
                 (format t "Best solution found.~%"))
          ;; ;; ;; end of debug.

          ;; in any case, increment the number of neighboorhoods-used
          do (incf neighboorhoods-used)

          ;; ;; ;; for debugging purposes
          ;; (format t "At the end of the while: ~a, ~a~%"
          ;;         current-index number-of-criteria
          ;;         ;; (< current-index number-of-criteria)
          ;;         ;; (<= neighboorhoods-used max-iter)
          ;;         )
          ;; ;; ;; end of debugging purposes ;;}}}

          )
    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (list current-solution
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))
