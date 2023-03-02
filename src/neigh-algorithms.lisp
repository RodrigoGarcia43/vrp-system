(in-package :vrp)

;; Descent Neighboorhood Search
(defun dns-with-generators-best-neighbor
    (problem solution criterion
     &key max-iter
       action
       ;; (print-mod 1)
       (exploration #'exhaustive-exploration))
  "Solves the VRP problem using a DNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood. We receive a parameter with the type of exploration we want to use."
  (let* (;; let's create the tree variable
         (neigh-tree (build-neighborhood-tree criterion solution))
         (solution-generator (funcall exploration neigh-tree))

         (better-solution nil)
         (better-cost 1e10)

         ;; (current-solution nil)
         (neighboorhoods-used 1)

         (current-solution (clone solution))
         (working-copy (make-working-copy current-solution))

         (better-solution-found nil)

         )

    ;; let's initialize the working copy and the solution
    (prepare-solution-for-neighborhood-exploration working-copy)
    (initialize-action-for-delta-cost-computation working-copy
                                                  problem
                                                  action)

    ;; as an experiment, let's print the number of
    ;; current solution's neighboors
    (format t "Neighboorhood  1: ~4d candidates... "
            (cardinality neigh-tree))

    ;; let's explore the neighboorhood of the
    ;; current-solution

    (setf (values better-solution better-cost)
          (return-best-neighbor solution-generator
                                working-copy
                                problem
                                action))

    ;; print info about the neighboorhood
    (if better-solution
        (then
          (format t "~6d~%" (+ (cost working-copy)
                               better-cost))
          (setf better-solution-found t))
        (else
          (format t "    Initial solution was optimum!!!~%")))



    ;; if better-solution is non nil, then
    ;; we have a better neighboor.

    ;; (if better-solution
    ;;     (then
    ;;       ;; update the current solution
    ;;       ;; in the working-copy
    ;;       (apply-set-of-operations better-solution working-copy)
    ;;       ;; update the cost of the new solution
    ;;       ;; (again, in the working copy)
    ;;       (incf (cost (solution working-copy)) better-cost)
    ;;       ;; let's prepare the new working-copy
    ;;       ;; and the action for the new solution
    ;;       (prepare-solution-for-neighborhood-exploration working-copy)
    ;;       (initialize-action-for-delta-cost-computation working-copy
    ;;                                                     problem
    ;;                                                     action)
    ;;       ;; now let's initialize the new generator
    ;;       (setf neigh-tree (build-neighborhood-tree criterion solution))
    ;;       (setf solution-generator (funcall exploration neigh-tree))

    ;;       (pp-solution working-copy t) (terpri)
    ;;       (format t "cost: ~a~%" (cost (solution working-copy)))))

    (format t "operations: ~a~%" better-solution)

    (loop while (and better-solution
                     (<= neighboorhoods-used max-iter))
          for i from 2
          do

          ;; update the current solution
          ;; in the working-copy
          (apply-set-of-operations better-solution working-copy)
          ;; update the cost of the new solution
          ;; (again, in the working copy)
          (incf (cost (solution working-copy)) better-cost)

          ;; let's prepare the new working-copy
          ;; and the action for the new solution
          (prepare-solution-for-neighborhood-exploration working-copy)
          (initialize-action-for-delta-cost-computation working-copy
                                                        problem
                                                        action)

          ;; now let's initialize the new generator
          (setf neigh-tree (build-neighborhood-tree
                            criterion
                            (solution working-copy)))
          (setf solution-generator (funcall exploration neigh-tree))


          ;; as an experiment, let's print the number of
          ;; current solution's neighboors
          (format t "Neighboorhood ~2d: ~4d candidates..."
                  i
                  (cardinality neigh-tree))

          ;; compute the new better-solution
          (setf (values better-solution better-cost)
                (return-best-neighbor solution-generator
                                      working-copy
                                      problem
                                      action))

          (if better-solution
              (then
                (format t "~6d~%" (+ (cost working-copy)
                                     better-cost)))
              (else
                (format t "    Optimum Found!!!~%")))


          ;; increment the number of neighboorhoods-used
          do (incf neighboorhoods-used))

    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (if better-solution-found
        (then
          (list (solution working-copy)
               better-cost
               neighboorhoods-used
               (<= neighboorhoods-used max-iter))))))

;; Descent Neighboorhood Search
(defun dns-with-generators-first-improvement
    (problem solution criterion
     &key max-iter
       action
       ;; (print-mod 1)
       (exploration #'exhaustive-exploration))
  "Solves the VRP problem using a DNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood. We receive a parameter with the type of exploration we want to use."
  (let* (;; let's create the tree variable
         (neigh-tree (build-neighborhood-tree criterion solution))
         (solution-generator (funcall exploration neigh-tree))

         (better-solution nil)
         (better-cost 1e10)

         ;; (current-solution nil)
         (neighboorhoods-used 1)

         (current-solution (clone solution))
         (working-copy (make-working-copy current-solution))

         (better-solution-found nil)

         )

    ;; let's initialize the working copy and the solution
    (prepare-solution-for-neighborhood-exploration working-copy)
    (initialize-action-for-delta-cost-computation working-copy
                                                  problem
                                                  action)

    ;; as an experiment, let's print the number of
    ;; current solution's neighboors
    (format t "Neighboorhood  1: ~4d candidates... "
            (cardinality neigh-tree))

    ;; let's explore the neighboorhood of the
    ;; current-solution

    (setf (values better-solution better-cost)
          (return-first-improvement solution-generator
                                    working-copy
                                    problem
                                    action))

    ;; print info about the neighboorhood
    (if better-solution
        (then
          (format t "~6d~%" (+ (cost working-copy)
                               better-cost))
          (setf better-solution-found t))
        (else
          (format t "    Initial solution was optimum!!!~%")))



    ;; if better-solution is non nil, then
    ;; we have a better neighboor.

    ;; (if better-solution
    ;;     (then
    ;;       ;; update the current solution
    ;;       ;; in the working-copy
    ;;       (apply-set-of-operations better-solution working-copy)
    ;;       ;; update the cost of the new solution
    ;;       ;; (again, in the working copy)
    ;;       (incf (cost (solution working-copy)) better-cost)
    ;;       ;; let's prepare the new working-copy
    ;;       ;; and the action for the new solution
    ;;       (prepare-solution-for-neighborhood-exploration working-copy)
    ;;       (initialize-action-for-delta-cost-computation working-copy
    ;;                                                     problem
    ;;                                                     action)
    ;;       ;; now let's initialize the new generator
    ;;       (setf neigh-tree (build-neighborhood-tree criterion solution))
    ;;       (setf solution-generator (funcall exploration neigh-tree))

    ;;       (pp-solution working-copy t) (terpri)
    ;;       (format t "cost: ~a~%" (cost (solution working-copy)))))

    (format t "operations: ~a~%" better-solution)

    (loop while (and better-solution
                     (<= neighboorhoods-used max-iter))
          for i from 2
          do

          ;; update the current solution
          ;; in the working-copy
          (apply-set-of-operations better-solution working-copy)
          ;; update the cost of the new solution
          ;; (again, in the working copy)
          (incf (cost (solution working-copy)) better-cost)

          ;; let's prepare the new working-copy
          ;; and the action for the new solution
          (prepare-solution-for-neighborhood-exploration working-copy)
          (initialize-action-for-delta-cost-computation working-copy
                                                        problem
                                                        action)

          ;; now let's initialize the new generator
          (setf neigh-tree (build-neighborhood-tree
                            criterion
                            (solution working-copy)))
          (setf solution-generator (funcall exploration neigh-tree))


          ;; as an experiment, let's print the number of
          ;; current solution's neighboors
          (format t "Neighboorhood ~2d: ~4d candidates..."
                  i
                  (cardinality neigh-tree))

          ;; compute the new better-solution
          (setf (values better-solution better-cost)
                (return-best-neighbor solution-generator
                                      working-copy
                                      problem
                                      action))

          (if better-solution
              (then
                (format t "~6d~%" (+ (cost working-copy)
                                     better-cost)))
              (else
                (format t "    Optimum Found!!!~%")))


          ;; increment the number of neighboorhoods-used
          do (incf neighboorhoods-used))

    ;; finally return the current solution
    ;; if it is nil, then the initial solution is an optimum
    (if better-solution-found
        (then
          (list (solution working-copy)
               better-cost
               neighboorhoods-used
               (<= neighboorhoods-used max-iter))))))

;; Descent Neighboorhood Search
(defun dns-with-generators-random-improvement
    (problem solution criterion
     &key max-iter
       action
       ;; (print-mod 1)
       (exploration #'exhaustive-exploration))
  "Solves the VRP problem using a DNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood. We receive a parameter with the type of exploration we want to use."
  (let* (;; let's create the tree variable
         (neigh-tree (build-neighborhood-tree criterion solution))
         (solution-generator (funcall exploration neigh-tree))

         (better-solution nil)
         (better-cost (cost solution))



         ;; (current-solution nil)
         (neighboorhoods-used 1)

         (current-solution (clone solution))
         (working-copy (make-working-copy current-solution))

         ;; the best solution found so far
         (best-solution-found (clone solution))
         (best-solution-cost  (cost best-solution-found))

         ;; solution-better-than-best
         (wc-for-solution-better-than-best nil)
         (solution-better-than-best nil)
         (cost-better-than-best 0)


         (better-solution-found nil)

         )

    ;; let's initialize the working copy and the solution
    (prepare-solution-for-neighborhood-exploration working-copy)
    (initialize-action-for-delta-cost-computation working-copy
                                                  problem
                                                  action)

    ;; as an experiment, let's print the number of
    ;; current solution's neighboors
    (format t "Neighboorhood  1: ~4d candidates..."
            (cardinality neigh-tree))

    ;; let's explore the neighboorhood of the
    ;; current-solution

    (setf (values better-solution
                  better-cost
                  ;; just in case we find a better best solution
                  solution-better-than-best
                  cost-better-than-best)
          (return-random-improvement solution-generator
                                     working-copy
                                     problem
                                     action
                                     (- better-cost
                                        best-solution-cost)))

    ;; print info about the neighboorhood
    (if better-solution
        (then
          (format t "~6d~%" (+ (cost working-copy)
                               better-cost))
          (setf better-solution-found t)
          ;; found a solution better than best
          ;; (format t "     better best solution with cost: ~a~%"
          ;;         (+ cost-better-than-best
          ;;            (cost working-copy)))
          )

        (else
          (format t "    Initial solution was optimum!!!~%")))

    ;; if we found a solution better than the best
    ;; we need to update it
    ;; (if solution-better-than-best
    ;;     (then
    ;;       ;; let's apply the operations to construct the
    ;;       ;; best neighboor

    ;;       ;; to do that, let's clone the current solution
    ;;       (setf wc-for-solution-better-than-best
    ;;             (basic-working-copy (clone (solution working-copy))))

    ;;       (apply-set-of-operations solution-better-than-best
    ;;                                wc-for-solution-better-than-best)

    ;;       (setf best-solution-found
    ;;             (clone
    ;;              (solution
    ;;               wc-for-solution-better-than-best)))
    ;;       (setf (cost best-solution-found)
    ;;             (+ (cost best-solution-found)
    ;;                cost-better-than-best))


    ;;       ;; (format t "Solution better than best with cost: ~a~%"
    ;;       ;;         (cost best-solution-found))
    ;;       ;; (pp-solution best-solution-found t) (terpri)
    ;;       )
    ;;     )



    ;; if better-solution is non nil, then
    ;; we have a better neighboor.

    ;; (if better-solution
    ;;     (then
    ;;       ;; update the current solution
    ;;       ;; in the working-copy
    ;;       (apply-set-of-operations better-solution working-copy)
    ;;       ;; update the cost of the new solution
    ;;       ;; (again, in the working copy)
    ;;       (incf (cost (solution working-copy)) better-cost)
    ;;       ;; let's prepare the new working-copy
    ;;       ;; and the action for the new solution
    ;;       (prepare-solution-for-neighborhood-exploration working-copy)
    ;;       (initialize-action-for-delta-cost-computation working-copy
    ;;                                                     problem
    ;;                                                     action)
    ;;       ;; now let's initialize the new generator
    ;;       (setf neigh-tree (build-neighborhood-tree criterion solution))
    ;;       (setf solution-generator (funcall exploration neigh-tree))

    ;;       (pp-solution working-copy t) (terpri)
    ;;       (format t "cost: ~a~%" (cost (solution working-copy)))))

    ;; (format t "operations: ~a~%" better-solution)

    (loop while (and better-solution
                     (<= neighboorhoods-used max-iter))
          for i from 2
          do

          ;; let's check if we found a solution
          ;; better than the best
          (if solution-better-than-best
              (then
                ;; let's apply the operations to construct the
                ;; best neighboor

                ;; to do that, let's clone the current solution
                (setf wc-for-solution-better-than-best
                      (basic-working-copy (clone (solution working-copy))))

                (apply-set-of-operations solution-better-than-best
                                         wc-for-solution-better-than-best)

                (setf best-solution-found
                      (clone
                       (solution
                        wc-for-solution-better-than-best)))

                ;; (format t "debug: cost best-solution-found: ~a~%"
                ;;         (cost best-solution-found))

                ;; (pp-solution best-solution-found t) (terpri)

                (setf (cost best-solution-found)
                      (+ (cost best-solution-found)
                         cost-better-than-best))
                ;; (format t "debug: after update. cost best-solution-found: ~a~%"
                ;;         (cost best-solution-found))

                ))

          ;; update the current solution
          ;; in the working-copy
          (apply-set-of-operations better-solution working-copy)
          ;; update the cost of the new solution
          ;; (again, in the working copy)
          (incf (cost (solution working-copy)) better-cost)

          ;; let's prepare the new working-copy
          ;; and the action for the new solution
          (prepare-solution-for-neighborhood-exploration working-copy)
          (initialize-action-for-delta-cost-computation working-copy
                                                        problem
                                                        action)

          ;; now let's initialize the new generator
          (setf neigh-tree (build-neighborhood-tree
                            criterion
                            (solution working-copy)))
          (setf solution-generator (funcall exploration neigh-tree))


          ;; as an experiment, let's print the number of
          ;; current solution's neighboors
          (format t "Neighboorhood ~2d: ~4d candidates (~a)..."
                  i
                  (cardinality neigh-tree)
                  ;; (cost working-copy)
                  ;; (cost best-solution-found)
                  (- (cost best-solution-found)
                     (cost working-copy))
                  )

          ;; compute the new better-solution
          (setf (values better-solution
                 better-cost
                 ;; just in case we find a better best solution
                 solution-better-than-best
                 cost-better-than-best)

                (return-random-improvement solution-generator
                                           working-copy
                                           problem
                                           action
                                           (- (cost working-copy)
                                              (cost best-solution-found))))

          (if better-solution
              (then

                (format t "~6d"
                        (+ (cost working-copy)
                           better-cost))

                (if solution-better-than-best
                    (then
                      (format t "  (~a)~%"
                        (+ (cost best-solution-found)
                           cost-better-than-best))
                      ;; (format t "Solution better than best with cost: ~a~%"
                      ;;         (cost best-solution-found))
                      ;; (pp-solution best-solution-found t) (terpri)
                      )
                    (else
                      (format t " NO BEST ~%")))
                )

              (else
                (format t "    Optimum Found!!!~%")))


          ;; increment the number of neighboorhoods-used
          do (incf neighboorhoods-used))

    ;; ;; finally return the current solution
    ;; ;; if it is nil, then the initial solution is an optimum
    (if better-solution-found
        (then
          (list best-solution-found
                (cost best-solution-found)
                neighboorhoods-used
                (<= neighboorhoods-used max-iter))))
    ))

;; Variable Neighboorhood Search
(defun vns-exhaustive-best-neighbor
    (problem
     solution
     criteria
     &key
       (max-iter 100)
       action
       (print-neighborhood-size nil)
       (print-mod 1)
       (exploration #'exhaustive-exploration))
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood."
  (declare (ignore print-mod))

  (let* (;; variables related to the vns
         (current-index 0)
         (number-of-criteria (length criteria))
         (current-criterion nil)

         ;; let's create the tree variable
         (neigh-tree nil)
         (solution-generator nil)

         (current-solution (clone solution))
         (working-copy (make-working-copy current-solution))

         (better-solution nil)
         (better-cost 0)

         (neighboorhoods-used 1)

         )

    ;; let's initialize the working copy and the solution
    (prepare-solution-for-neighborhood-exploration working-copy)
    (initialize-action-for-delta-cost-computation working-copy
                                                  problem
                                                  action)


    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))


          do
          ;; let's set the current criterion
          (setf current-criterion (nth current-index criteria))

          ;; let's create the generator
          ;; first the tree
          (setf neigh-tree (build-neighborhood-tree
                            current-criterion
                            current-solution))

          ;; and now, let's create the generator
          (setf solution-generator (funcall exploration neigh-tree))


          ;; for debugging purposes
          (format t "Debug VNS.  Iteration ~3d, criterion ~2d"
                  neighboorhoods-used current-index)

          (if print-neighborhood-size
              (format t " (~6d)"
                      (cardinality neigh-tree)))

          ;; let's explore with the current criteria
          (setf (values better-solution better-cost)
                (return-best-neighbor solution-generator
                                      working-copy
                                      problem
                                      action))

          (if better-solution
              (then ;; update the current solution

                ;; (format t "~%   dbg: ~a, -solution ~a"
                ;;         better-cost
                ;;         better-solution)


                ;; we found a better solution
                ;; update the current-solution
                ;; in the working-copy
                (apply-set-of-operations better-solution working-copy)
                ;; update the cost of the new solution
                ;; (again, in the working copy)
                (incf (cost (solution working-copy)) better-cost)



                ;; let's prepare the new working-copy
                ;; and the action for the new solution
                (prepare-solution-for-neighborhood-exploration
                 working-copy)
                (initialize-action-for-delta-cost-computation
                 working-copy
                 problem
                 action)

                ;; (format t "~%   dbg: cost: ~a"
                ;;         (cost working-copy))

                ;; (format t "~%   dbg: sol:~%")
                ;; (pp-solution working-copy t) (terpri)



                ;; now let's initialize the new generator
                (setf neigh-tree (build-neighborhood-tree
                                  current-criterion
                                  (solution working-copy)))
                (setf solution-generator
                      (funcall exploration neigh-tree))

                ;; now that the current solution is
                ;; updated, let's set the
                ;; number-of-criteria to 0
                (setf current-index 0))
              (else ;; incf the current-index
                ;; use the next criterion
                (incf current-index)))

          ;; update the log
          (format t ":  ~6d~%" (cost working-copy))

          ;; in any case, increment the number of neighboorhoods-used
          (incf neighboorhoods-used)

          )


    ;; finally return the current solution
    ;; the current-solution is the solution
    ;; in the working copy.
    ;; if it is nil, then the initial solution is an optimum

    (list (solution working-copy)
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-exhaustive-best-neighbor-max-neigh-size
    (problem
     solution
     criteria
     &key
       (max-iter 100)
       (max-neighborhood-size 4000)
       action
       (print-neighborhood-size nil)
       (print-mod 1)
       ;; (exploration #'exhaustive-exploration)
       )
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood. If a given neighboorhood has a cardinality greater than max-neighborhood-size, then just make a uniform exploration."
  (declare (ignore print-mod))

  (let* (;; variables related to the vns
         (current-index 0)
         (number-of-criteria (length criteria))
         (current-criterion nil)

         ;; let's create the tree variable
         (neigh-tree nil)
         (solution-generator nil)

         (current-solution (clone solution))
         (working-copy (make-working-copy current-solution))

         (better-solution nil)
         (better-cost 0)

         (neighboorhoods-used 1)

         )

    ;; let's initialize the working copy and the solution
    (prepare-solution-for-neighborhood-exploration working-copy)
    (initialize-action-for-delta-cost-computation working-copy
                                                  problem
                                                  action)


    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))


          do
          ;; let's set the current criterion
          (setf current-criterion (nth current-index criteria))

          ;; let's create the generator
          ;; first the tree
          (setf neigh-tree (build-neighborhood-tree
                            current-criterion
                            current-solution))

          ;; and now, let's create the generator
          ;; according to the size of the neighborhood
          (if (<= (cardinality neigh-tree) max-neighborhood-size)
              (then ;; use exhaustive-exploration
                (setf solution-generator
                      (exhaustive-exploration neigh-tree)))
              (else ;; use uniform-exploration
                (setf solution-generator
                      (uniform-exploration neigh-tree
                                           max-neighborhood-size))))


          ;; for debugging purposes
          (format t "Debug VNS.  Iteration ~3d, criterion ~2d"
                  neighboorhoods-used current-index)

          (if print-neighborhood-size
              (format t " (~6d)"
                      (if (<= (cardinality neigh-tree)
                              max-neighborhood-size)
                          (then (cardinality neigh-tree))
                          (else max-neighborhood-size))))

          ;; let's explore with the current criteria
          (setf (values better-solution better-cost)
                (return-best-neighbor solution-generator
                                      working-copy
                                      problem
                                      action))

          (if better-solution
              (then ;; update the current solution

                ;; (format t "~%   dbg: ~a, -solution ~a"
                ;;         better-cost
                ;;         better-solution)


                ;; we found a better solution
                ;; update the current-solution
                ;; in the working-copy
                (apply-set-of-operations better-solution working-copy)
                ;; update the cost of the new solution
                ;; (again, in the working copy)
                (incf (cost (solution working-copy)) better-cost)



                ;; let's prepare the new working-copy
                ;; and the action for the new solution
                (prepare-solution-for-neighborhood-exploration
                 working-copy)
                (initialize-action-for-delta-cost-computation
                 working-copy
                 problem
                 action)

                ;; (format t "~%   dbg: cost: ~a"
                ;;         (cost working-copy))

                ;; (format t "~%   dbg: sol:~%")
                ;; (pp-solution working-copy t) (terpri)



                ;; now let's initialize the new generator
                (setf neigh-tree (build-neighborhood-tree
                                  current-criterion
                                  (solution working-copy)))
                ;; create the generator according to the
                ;; size of the neighboorhood
                (if (<= (cardinality neigh-tree) max-neighborhood-size)
                    (then ;; use exhaustive-exploration
                      (setf solution-generator
                            (exhaustive-exploration neigh-tree)))
                    (else ;; use uniform-exploration
                      (setf solution-generator
                            (uniform-exploration neigh-tree
                                                 max-neighborhood-size))))

                ;; now that the current solution is
                ;; updated, let's set the
                ;; number-of-criteria to 0
                (setf current-index 0))
              (else ;; incf the current-index
                ;; use the next criterion
                (incf current-index)))

          ;; update the log
          (format t ":  ~6d~%" (cost working-copy))

          ;; in any case, increment the number of neighboorhoods-used
          (incf neighboorhoods-used)

          )


    ;; finally return the current solution
    ;; the current-solution is the solution
    ;; in the working copy.
    ;; if it is nil, then the initial solution is an optimum

    (list (solution working-copy)
          neighboorhoods-used
          (<= neighboorhoods-used max-iter))))

;; Variable Neighboorhood Search
(defun vns-adaptative-first-improvement-max-neigh-size
    (problem
     solution
     criteria
     &key
       (max-iter 100)
       (max-neighborhood-size 4000)
       action
       (print-neighborhood-size nil)
       (print-mod 1)
       ;; (exploration #'exhaustive-exploration)
       )
  "Solves the VRP problem using a VNS with the given criterion starting with the given solution. We assume that the initial solution has the cost slot bound. If the result is nil, then the initial solution is an optimum for that neighboorhood. If a given neighboorhood has a cardinality greater than max-neighborhood-size, then just make a uniform exploration."
  (declare (ignore print-mod))

  (let* (;; variables related to the vns
         (current-index 0)
         (number-of-criteria (length criteria))
         (current-criterion nil)

         ;; let's create the tree variable
         (neigh-tree nil)
         (solution-generator nil)

         (current-solution (clone solution))
         (working-copy (make-working-copy current-solution))

         (better-solution nil)
         (better-cost 0)

         (neighboorhoods-used 1)

         )

    ;; let's initialize the working copy and the solution
    (prepare-solution-for-neighborhood-exploration working-copy)
    (initialize-action-for-delta-cost-computation working-copy
                                                  problem
                                                  action)


    (loop while (and (< current-index number-of-criteria)
                     (<= neighboorhoods-used max-iter))


          do
          ;; let's set the current criterion
          (setf current-criterion (nth current-index criteria))

          ;; let's create the generator
          ;; first the tree
          (setf neigh-tree (build-neighborhood-tree
                            current-criterion
                            current-solution))

          ;; and now, let's create the generator
          ;; according to the size of the neighborhood
          (if (<= (cardinality neigh-tree) max-neighborhood-size)
              (then ;; use exhaustive-exploration
                (setf solution-generator
                      (exhaustive-exploration neigh-tree)))
              (else ;; use uniform-exploration
                (setf solution-generator
                      (uniform-exploration neigh-tree
                                           max-neighborhood-size))))


          ;; for debugging purposes
          (format t "Debug VNS.  Iteration ~3d, criterion ~2d"
                  neighboorhoods-used current-index)

          (if print-neighborhood-size
              (format t " (~6d)"
                      (if (<= (cardinality neigh-tree)
                              max-neighborhood-size)
                          (then (cardinality neigh-tree))
                          (else max-neighborhood-size))))

          ;; let's explore with the current criteria
          (setf (values better-solution better-cost)
                (return-first-improvement
                 solution-generator
                 working-copy
                 problem
                 action))

          (if better-solution
              (then ;; update the current solution

                ;; we found a better solution
                ;; update the current-solution
                ;; in the working-copy
                (apply-set-of-operations better-solution working-copy)
                ;; update the cost of the new solution
                ;; (again, in the working copy)
                (incf (cost (solution working-copy)) better-cost)


                ;; let's prepare the new working-copy
                ;; and the action for the new solution
                (prepare-solution-for-neighborhood-exploration
                 working-copy)
                (initialize-action-for-delta-cost-computation
                 working-copy
                 problem
                 action)


                ;; now let's initialize the new generator
                (setf neigh-tree (build-neighborhood-tree
                                  current-criterion
                                  (solution working-copy)))
                ;; create the generator according to the
                ;; size of the neighboorhood
                (if (<= (cardinality neigh-tree) max-neighborhood-size)
                    (then ;; use exhaustive-exploration
                      (setf solution-generator
                            (exhaustive-exploration neigh-tree)))
                    (else ;; use uniform-exploration
                      (setf solution-generator
                            (uniform-exploration neigh-tree
                                                 max-neighborhood-size))))

                ;; now that the current solution is
                ;; updated, let's set the
                ;; number-of-criteria to 0
                (setf current-index 0))
              (else ;; incf the current-index
                ;; to use the next criterion
                (incf current-index)))

          ;; update the log
          (format t ":  ~6d~%" (cost working-copy))

          ;; in any case, increment the number of neighboorhoods-used
          (incf neighboorhoods-used)

          )


    ;; finally return the current solution
    ;; the current-solution is the solution
    ;; in the working copy.
    ;; if it is nil, then the initial solution is an optimum

    (list (solution working-copy)
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
