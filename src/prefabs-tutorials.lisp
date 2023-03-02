(in-package :vrp)

(defun rab-exhaustive-best (solution problem action)
  "Returns the best neighbor of the solution in wc, using an exhaustive search and best-improvment strategy."
  (let* ( ;; first the standard initializations
         (*vrp-stop-neighborhood-search* nil)
         (wc (basic-working-copy solution))
         (ops-list nil)
         ;; now the corresponding to the
         ;; best-improvement strategy
         (current-delta-cost 0)
         (best-delta-cost 0)
         (best-neighbor nil)
         (best-neighbor-as-solution nil)
         )
    (prepare-solution-for-neighborhood-exploration wc)
    (initialize-action-for-delta-cost-computation wc problem action)

    (doselect-route (r1 wc)
      (doselect-client (c1 r1 wc)
        (doinsert-client (c1 r1 wc)
          ;; let's get the cost of the current neighbor
          (setf current-delta-cost
                (delta-cost (reverse ops-list) wc problem action))

          (format t "~%With delta-cost: ~a~%" current-delta-cost)
          (pp-solution wc t) (terpri)

          ;; let's see if it is better
          (if (< current-delta-cost best-delta-cost)
              ;; if it is
              (then
                ;; let's save the ops that make it
                (setf best-neighbor (clone ops-list))
                ;; let's update the best-cost
                (setf best-delta-cost current-delta-cost))))))

    ;; here we check if we found a neighbor better than solution
    ;; if so, we apply the operations in best-neighbor to solution
    ;; and return the best-neighbor
    ;; otherwise, we return nil
    ;; and that means that we didn't find a better neighbor

    (if best-neighbor
        (then
          (apply-set-of-operations (reverse best-neighbor) wc)
          (setf best-neighbor-as-solution
                (solution wc))
          (setf (cost best-neighbor-as-solution)
                (+ (cost solution)
                   best-delta-cost))
          ;; return best-neighbor-as-solution
          (values best-neighbor-as-solution
                  best-delta-cost)))))

(defun rab-exhaustive-first (solution problem action)
  "Returns the first neighbor of the solution in wc, that is better that wc.."
  (let* ( ;; first the standard initializations
         (*vrp-stop-neighborhood-search* nil)
         (wc (basic-working-copy solution))
         (ops-list nil)
         ;; now the corresponding to the
         ;; best-improvement strategy
         (current-delta-cost 0)
         (best-delta-cost 0)
         (best-neighbor nil)
         (best-neighbor-as-solution nil)
         )
    (prepare-solution-for-neighborhood-exploration wc)
    (initialize-action-for-delta-cost-computation wc problem action)

    (doselect-route (r1 wc)
      (doselect-client (c1 r1 wc)
        (doinsert-client (c1 r1 wc)
          ;; let's get the cost of the current neighbor
          (setf current-delta-cost
                (delta-cost (reverse ops-list) wc problem action))

          (format t "~%With delta-cost: ~a~%" current-delta-cost)
          (pp-solution wc t) (terpri)

          ;; let's see if it is better
          (if (< current-delta-cost best-delta-cost)
              ;; if it is, set the best values 
              ;; and stop the iterations
              (then
                ;; let's save the ops that make it
                (setf best-neighbor (clone ops-list))
                ;; let's update the best-cost
                (setf best-delta-cost current-delta-cost)
                ;; let's stop the iteration
                (stop-neighborhood-search))))))

    ;; here we check if we found a neighbor better than solution
    ;; if so, we apply the operations in best-neighbor to solution
    ;; and return the best-neighbor
    ;; otherwise, we return nil
    ;; and that means that we didn't find a better neighbor

    (if best-neighbor
        (then
          (apply-set-of-operations (reverse best-neighbor) wc)
          (setf best-neighbor-as-solution
                (solution wc))
          (setf (cost best-neighbor-as-solution)
                (+ (cost solution)
                   best-delta-cost))))

    ;; return best-neighbor-as-solution
    ;; and the best-delta-cost
    (values best-neighbor-as-solution
            best-delta-cost)))

(defun rab-exhaustive-best-with-delta-cost* (solution problem action)
  "Returns the best neighbor of the solution in wc, using an exhaustive search and best-improvment strategy."
  (let* ( ;; first the standard initializations
         (*vrp-stop-neighborhood-search* nil)
         (wc (basic-working-copy solution))
         (ops-list nil)
         ;; the following line is what we need
         ;; to use the function delta-cost*
         (wc-for-delta-cost (clone wc)) ;; the 2nd wc we'll need
         ;; now what corresponds to the
         ;; best-improvement strategy
         (current-delta-cost 0)
         (best-delta-cost 0)
         (best-neighbor nil)
         (best-neighbor-as-solution nil)
         )


    (prepare-solution-for-neighborhood-exploration wc)

    ;; let's clone the initialized wc to wc-for-delta-cost
    ;; (setf wc-for-delta-cost (clone wc))
    (prepare-solution-for-neighborhood-exploration wc-for-delta-cost)

    ;; this is the for the computation of the delta-cost
    ;; this action should be of the *-type
    (initialize-action-for-delta-cost-computation wc problem action)


    ;; here starts the exploration of the neighborhood
    (doselect-route (r1 wc)
      (doselect-client (c1 r1 wc)
        (doinsert-client (c1 r1 wc)

          ;; let's get the cost of the current neighbor
          (setf current-delta-cost
                (delta-cost* (reverse ops-list)
                             wc-for-delta-cost
                             problem
                             action))
          (format t "~%With delta-cost: ~a~%" current-delta-cost)
          (pp-solution wc t) (terpri)

          ;; let's see if it is better
          (if (< current-delta-cost best-delta-cost)
              ;; if it is
              (then
                ;; let's save the ops that make it
                (setf best-neighbor (clone ops-list))
                ;; let's update the best-cost
                (setf best-delta-cost current-delta-cost))))))

    ;; here we check if we found a neighbor better than solution
    ;; if so, we apply the operations in best-neighbor to solution
    ;; and return the best-neighbor
    ;; otherwise, we return nil
    ;; and that means that we didn't find a better neighbor

    (if best-neighbor
        (then
          (apply-set-of-operations (reverse best-neighbor) wc)
          (setf best-neighbor-as-solution
                (solution wc))
          (setf (cost best-neighbor-as-solution)
                (+ (cost solution)
                   best-delta-cost))
          ;; return best-neighbor-as-solution
          (values best-neighbor-as-solution
                  best-delta-cost)))))
