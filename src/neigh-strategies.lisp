(in-package :vrp)

(defmacro update-best-known-solution ()
  "Adds the code to update the best-known-solution."
  `(if (< current-delta-cost best-known-cost)
       ;; let's save it
       (setf solution-better-than-the-best-known
             (clone current-sol-operations)
             cost-better-than-the-best-known
             current-delta-cost)))

(defparameter *default-best-known-cost* -1e10
  "Update the best-known-solution if the current cost is smaller than this value.")

(defparameter *probability-to-accept-a-better-neighbor* 0.3
  "This is the probability to accept a better neighbor in a random-improvement search.")

(defparameter *number-of-candidates-to-keep-in-random-improvement* 10
  "This is the number of solutions to keep in a random-improvement search.")

(defun return-best-neighbor (sol-generator
                             working-copy
                             problem
                             action
                             &optional best-known-cost)
  "Returns the best neighbor of the current solution."
  (declare (ignore best-known-cost))
  (let* (;; first the standard initializations

         ;; here we create the current solution
         (current-solution (funcall sol-generator))

         ;; the current-solution as vrp-operations
         (current-sol-operations nil)

         ;; :vrp system stuff
         (current-delta-cost 0)
         ;; we need to use an action with *
         ;; because we are using the delta-cost* function 

         ;; what follows are the required elements
         ;; for the exhaustive search
         (best-delta-cost 0)
         (best-neighbor nil)
         )

    ;; here starts the exploration of the neighborhood
    (loop while current-solution do
          ;; here we have the current neighbor as a list
          ;; of vrp operations.
          (setf current-sol-operations (from-coordinates-to-operations
                                        current-solution))

          ;; let's get the cost of the current neighbor
          (setf current-delta-cost
                (delta-cost* current-sol-operations
                             working-copy
                             problem
                             action))

          ;; let's see if it is better
          (if (< current-delta-cost
                 best-delta-cost)
              (then
                ;; let's save the ops that creates the best neighbor
                (setf best-neighbor (clone current-sol-operations))
                ;; let's update the best-cost
                (setf best-delta-cost current-delta-cost)))

          ;; now let's create the next solution
          (setf current-solution (funcall sol-generator)))

    ;; here we check if we found a neighbor better than solution
    ;; otherwise, we return nil
    ;; and that means that we didn't find a better neighbor

    ;; we return the best-neighbor and the best-delta-cost
    ;; if best-neighbor is nil
    ;; that means that the current-solution
    ;; is a minimum for this neighborhood
    (values best-neighbor best-delta-cost
            ;; we return nil because
            ;; the algorithms expect
            ;; two more values: a solution
            ;; better than the best-known
            ;; and its cost.
            ;; In this case it is not
            ;; that much important because
            ;; with this selection
            ;; we always return a solution
            ;; better than the best-known
            nil nil)))

(defun return-first-improvement (sol-generator
                                 working-copy
                                 problem
                                 action
                                 )
  "Returns the first neighbor that is better than the current solution."
  (let* (;; first the standard initializations
         (stop-neighborhood-search nil)
         ;; here we create the current solution
         (current-solution (funcall sol-generator))

         ;; the current-solution as vrp-operations
         (current-sol-operations nil)

         ;; :vrp system stuff
         (current-delta-cost 0)
         ;; we need to use an action with *
         ;; because we want to use the delta-cost* function 

         ;; what follows is the required elements
         ;; for the exhaustive search
         (best-delta-cost 0)
         (best-neighbor nil)
         )


    ;; here starts the exploration of the neighborhood
    (loop while (and
                 current-solution
                 (not stop-neighborhood-search))
          do

          ;; here we have the current neighbor as a list
          ;; of vrp operations.
          (setf current-sol-operations (from-coordinates-to-operations
                                        current-solution))

          ;; let's get the cost of the current neighbor
          (setf current-delta-cost
                (delta-cost* current-sol-operations
                             working-copy
                             problem
                             action))

          ;; let's see if it is better
          (if (< current-delta-cost best-delta-cost)
              (then
                ;; let's save the ops that creates the best neighbor
                (setf best-neighbor (clone current-sol-operations))
                ;; let's update the best-cost
                (setf best-delta-cost current-delta-cost)
                ;; stop the search
                (setf stop-neighborhood-search t)))

          ;; now let's create the next solution
          (unless stop-neighborhood-search
            (setf current-solution (funcall sol-generator))))

    ;; here we return the first neighbor found better than the
    ;; current-solution.
    ;; If we return nil, it means
    ;; that we didn't find a better neighbor

    (values best-neighbor best-delta-cost)))

(defun return-random-improvement (sol-generator
                                  working-copy
                                  problem
                                  action
                                  &optional (best-known-cost
                                             *default-best-known-cost*))
  "Returns a random better solution in the neighborhood."
  (let* (;; first the standard initializations
         (stop-neighborhood-search nil)
         ;; here we create the current solution
         (current-solution (funcall sol-generator))

         (solution-better-than-the-best-known nil)
         (cost-better-than-the-best-known 1e10)

         ;; the current-solution as vrp-operations
         (current-sol-operations nil)

         ;; :vrp system stuff
         (current-delta-cost 0)
         ;; we need to use an action with *
         ;; because I want to use the delta-cost* function 

         ;; what follows is the required elements
         ;; for the exhaustive search
         (best-delta-cost 0)
         (best-neighbor nil)
         )


    ;; here starts the exploration of the neighborhood
    (loop while (and current-solution
                     (not stop-neighborhood-search))
          do

          ;; here we have the current neighbor as a list
          ;; of vrp operations.
          (setf current-sol-operations (from-coordinates-to-operations
                                        current-solution))

          ;; let's get the cost of the current neighbor
          (setf current-delta-cost
                (delta-cost* current-sol-operations
                             working-copy
                             problem
                             action))

          ;; let's see if it is better
          (if (< current-delta-cost best-delta-cost)
              (then
                ;; let's (possibly) update the best-known-solution
                (update-best-known-solution)

                ;; let's check if we should
                ;; randomly accept this neighbor
                (let* ((r (random 1.0)))

                  (if (<= r *probability-to-accept-a-better-neighbor*)
                      ;; let's set the best-neighbor
                      ;; and stop the search
                      (then
                        (setf best-neighbor (clone current-sol-operations))
                        (setf best-delta-cost current-delta-cost)
                        (setf stop-neighborhood-search t))

                      (else ;; let's check if
                        ;; best-neighbor is bound
                        ;; if it isn't bind it to
                        ;; this neighbor
                        ;; [this would be a first-improvement]
                        (unless best-neighbor
                          (setf best-neighbor
                                (clone current-sol-operations))
                          (setf best-delta-cost current-delta-cost)))))))

          ;; now let's create the next solution
          (unless *vrp-stop-neighborhood-search*
            (setf current-solution (funcall sol-generator))))

    ;; here we the randomly accepted
    ;; better neighbor and it's cost.
    ;; If the first returned value is nil, it means
    ;; that we didn't find a better neighbor
    ;; We also return the solution
    ;; better-than-best, if we found any.

    (values best-neighbor
            best-delta-cost
            solution-better-than-the-best-known
            cost-better-than-the-best-known)))
