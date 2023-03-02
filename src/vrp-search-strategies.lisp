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

(def-vrp-class use-a-neighborhood-generator ()
  ()
    :documentation "An instance of this class indicates that we should generate the neighbors using a neighborhood-generator."
    :constructor (use-a-neighborhood-generator ())
    :print-object-string ("<use-a-neighborhood-generator>")
    :slots-for-obj= ()
    :slots-for-clone ())

(def-vrp-class search-with-a-basic-wc ()
  ()
    :documentation "An instance of this class indicates that we should explore the neighborhood with an instance of a basic-working-copy."
    :constructor (search-with-a-basic-wc ())
    :print-object-string ("<search-with-basic-wc>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +search-with-a-basic-wc+
  (search-with-a-basic-wc))

(def-vrp-class search-with-delta-cost-computation ()
    ()
    :documentation "A class to represent a search strategy where we need to compute the delta-cost of each neighbor.")

(def-vrp-class compute-delta-cost-inefficiently
    (search-with-delta-cost-computation)
    ()
    :documentation "A class to represent a search strategy where we compute the delta-cost of each neighbor using the function delta-cost.")

(def-vrp-class compute-delta-cost-with-smart-macros
    (search-with-delta-cost-computation)
    ()
    :documentation "A class to represent a search strategy where we compute the delta-cost of each neighbor using the the * macros.")

(def-vrp-class there-is-a-best-solution ()
    ()
    :documentation "A class to represent a search strategy where we have a best-solution value that we update during the search.")

(def-vrp-class return-best-solution
    (there-is-a-best-solution)
    ()
    :documentation "A class to represent a search strategy where we always return the value of best-solution.  If there isn't a best-solution, we return nil.")

(def-vrp-class return-best-delta-cost
    (there-is-a-best-solution)
    ()
    :documentation "A class to represent a selection strategy where we always return the delta-cost of the best-neighbor found.  If there is not a best neighbor we return 0.")

(def-vrp-class has-candidates-for-best-neighbor
    ()
    () ;; no slots
    :documentation "A class to represent a search strategy where we collect a set of neighbors as candidates for the best-neighbor.")

(def-vrp-class looking-for-bug
    (search-with-delta-cost-computation
     search-with-a-basic-wc)
    () ;; no slots
    :documentation "A class used to appropriately print debug messages.")

(defparameter +looking-for-bug+
  (make-instance 'looking-for-bug))

(def-vrp-class exhaustive-neighborhood-search-strategy ()
    ()
    :documentation "A class to represent an exhaustive search of a neighborhood."
    :constructor (exhaustive-neighborhood-search-strategy ())
    :print-object-string ("<exhaustive-search>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +exhaustive-search-strategy+
  (exhaustive-neighborhood-search-strategy))

(def-vrp-class random-neighborhood-search-strategy ()
    ((neighborhood-size
      :documentation "The number of neighbors that should be explored."))
    :documentation "A class to represent an exhaustive search of a neighborhood."
    :constructor (random-neighborhood-search-strategy
                  (&optional (neighborhood-size 100)))
    :print-object-string ("<random-search: ~a>" neighborhood-size)
    :slots-for-obj= (neighborhood-size)
    :slots-for-clone (neighborhood-size))

(defparameter +random-search-strategy+
  (random-neighborhood-search-strategy 500))

(def-vrp-class jump-around-search-strategy
    (random-neighborhood-search-strategy)
    ()
    :documentation "A class to represent a `jump-around' search of a neighborhood."
    :constructor (jump-around-search-strategy
                  (&optional (neighborhood-size 100)))
    :print-object-string ("<jump-around-search: ~a>" neighborhood-size)
    :slots-for-obj= (neighborhood-size)
    :slots-for-clone (neighborhood-size))

(defparameter +jump-around-strategy+
  (jump-around-search-strategy 10))

(def-vrp-class exhaustive/efficient-neighborhood-search-strategy
    (exhaustive-neighborhood-search-strategy)
  ()
    :documentation "A class to represent an exhaustive search of a neighborhood."
    :constructor (exhaustive/efficient-neighborhood-search-strategy ())
    :print-object-string ("<exhaustive*-search>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter *exhaustive-search-strategy*
  (exhaustive/efficient-neighborhood-search-strategy))

(def-vrp-class random-bug-search-strategy
    (random-neighborhood-search-strategy
     looking-for-bug)
    ()
    :documentation "A class to look for a bug in a random search a neighborhood."
    :constructor (random-bug-search-strategy
                  (&optional (neighborhood-size 10)))
    :print-object-string ("<random-bug-search: ~a>" neighborhood-size)
    :slots-for-obj= (neighborhood-size)
    :slots-for-clone (neighborhood-size))

(defparameter +random-bug-search+
  (random-bug-search-strategy 10))

(def-vrp-class best-improvement-search-strategy
    (
     return-best-solution
     search-with-a-basic-wc)

    ()
    :documentation "A class to represent a best-improvement strategy in the search of a neighborhood."
    :constructor (best-improvement-search-strategy ())
    :print-object-string ("<best-improvement-strategy>")
    :slots-for-obj= ()
    :slots-for-clone ())

(def-vrp-class first-improvement-search-strategy
    (
     return-best-solution
     search-with-a-basic-wc)

    ()
    :documentation "A class to represent a first-improvement strategy in the search of a neighborhood."
    :constructor (first-improvement-search-strategy ())
    :print-object-string ("<first-improvement-strategy>")
    :slots-for-obj= ()
    :slots-for-clone ())

(def-vrp-class random-improvement-with-candidates-selection-strategy
    (
     return-best-solution
     has-candidates-for-best-neighbor
     search-with-a-basic-wc)

    ()
    :documentation "A class to represent a random-improvement strategy in the search of a neighborhood."
    :constructor (random-improvement-selection-strategy ())
    :print-object-string ("<random-improvement-strategy>")
    :slots-for-obj= ()
    :slots-for-clone ())

(def-vrp-class random-improvement-selection-strategy
    (
     return-best-solution
     search-with-a-basic-wc)

    ((acceptance-ratio
      :initform 0.5
      :documentation "The acceptance probability."))
    :documentation "A class to represent a random-improvement strategy in the search of a neighborhood."
    :constructor (random-improvement-selection-strategy
                  (&optional (acceptance-ratio 0.8)))
    :print-object-string ("<random-improvement-strategy>")
    :slots-for-obj= (acceptance-ratio)
    :slots-for-clone (acceptance-ratio))

(def-vrp-class jump-around-return-last-neighbor
    (search-with-a-basic-wc
     compute-delta-cost-inefficiently)

    ()
    :documentation "A class to represent the simplest jump-around selection strategy."
    :constructor (jump-around-return-last-neighbor ())
    :print-object-string ("<jump-around-last-neighbor>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +jump-around-return-last-neighbor+
  (make-instance 'jump-around-return-last-neighbor))

(def-vrp-class return-last-neighbor-selection-strategy
    (return-best-delta-cost
     return-best-solution
     search-with-a-basic-wc)
    () ;; no slots
    :documentation "A class to represent a search strategy where we return the last neighbor independently of its cost.")

(defparameter +return-last-neighbor+
  (make-instance 'return-last-neighbor-selection-strategy))

(def-vrp-class best-improvement-inefficient
    (best-improvement-search-strategy
     compute-delta-cost-inefficiently)

    ()
    :documentation "A class to represent a best-improvement (with delta-cost) strategy in the search of a neighborhood."
    :constructor (best-improvement-inefficient ())
    :print-object-string ("<best-improvement-inefficient>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +best-improvement+
  (make-instance 'best-improvement-inefficient))

(def-vrp-class first-improvement-ineffcient
    (first-improvement-search-strategy
     compute-delta-cost-inefficiently)

    ()
    :documentation "A class to represent a first-improvement strategy computing the delta-cost with the function =delta-cost=."
    :constructor (first-improvement-ineffcient ())
    :print-object-string ("<first-improvement-inefficient>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +first-improvement+
  (make-instance 'first-improvement-ineffcient))

(def-vrp-class random-improvement-with-candidates-inefficient
    (random-improvement-with-candidates-selection-strategy
     compute-delta-cost-inefficiently)

    ()
    :documentation "A class to represent a random-improvement strategy in the search of a neighborhood, that uses the delta-cost function."
    :constructor (random-improvement-with-candidates-inefficient)
    :print-object-string ("<random-improvement-inefficient>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter +random-improvement-with-candidates+
  (make-instance 'random-improvement-with-candidates-inefficient))

(def-vrp-class random-improvement-inefficient
    (random-improvement-selection-strategy
     compute-delta-cost-inefficiently)

    ()
    :documentation "A class to represent a random-improvement strategy in the search of a neighborhood, that uses the delta-cost function."
    :constructor (random-improvement-inefficient
                  (&optional (acceptance-ratio 0.8)))
    :print-object-string ("<random-improvement-inefficient>")
    :slots-for-obj= (acceptance-ratio)
    :slots-for-clone (acceptance-ratio))

(defparameter +random-improvement+
  (make-instance 'random-improvement-inefficient))

(def-vrp-class best-improvement-smart
    (best-improvement-search-strategy
     compute-delta-cost-with-smart-macros)

    ()
    :documentation "A class to represent a best-improvement strategy (with * macros) in the search of a neighborhood."
    :constructor (best-improvement-smart ())
    :print-object-string ("<best-improvement-smart>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter *best-improvement*
  (make-instance 'best-improvement-smart))

(def-vrp-class first-improvement-smart
    (first-improvement-search-strategy
     compute-delta-cost-with-smart-macros)

    ()
    :documentation "A class to represent a first-improvement strategy where we use the * macros."
    :constructor (first-improvement-smart ())
    :print-object-string ("<first-improvement-smart")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter *first-improvement*
  (make-instance 'first-improvement-smart))

(def-vrp-class random-improvement-with-candidates-smart
    (random-improvement-with-candidates-selection-strategy
     compute-delta-cost-with-smart-macros)

    ()
    :documentation "A class to represent a random-improvement strategy in the search of a neighborhood, that uses the smart macros."
    :constructor (random-improvement-with-candidates-smart ())
    :print-object-string ("<random-improvement-smart>")
    :slots-for-obj= ()
    :slots-for-clone ())

(defparameter *random-improvement-with-candidates*
  (make-instance 'random-improvement-with-candidates-smart))

(def-vrp-class random-improvement-smart
    (random-improvement-selection-strategy
     compute-delta-cost-with-smart-macros)

    ()
    :documentation "A class to represent a random-improvement strategy in the search of a neighborhood, that uses the smart macros."
    :constructor (random-improvement-smart (&optional
                                            (acceptance-ratio 0.8)))
    :print-object-string ("<random-improvement-smart>")
    :slots-for-obj= (acceptance-ratio)
    :slots-for-clone (acceptance-ratio))

(defparameter *random-improvement*
  (make-instance 'random-improvement-smart))

(def-vrp-class basic-neighborhood-strategy-blueprint ()
    ((initializations-inside-the-let
      :documentation "What we should initialize inside the let.")
     (initializations-outside-the-let
      :documentation "What we should initialize outside the let.")
     (macro-headings
      :documentation "A list with the heading of the macro calls.")
     (code-inside-the-macros
      :documentation "What we should do with the current neigborh.")
     (code-outside-the-macros
      :documentation "What we should do after the iteration through all the neigborhs is over.")
     (return-code
      :documentation "What we should return from the exploration."))
    :documentation "A class to represent a description of a neigborhood strategy.  It is used in the automatic generation of neigborhood search algorithms."
    :constructor (basic-strategy-blueprint
                  (&key (initializations-inside-the-let nil)
                        (initializations-outside-the-let nil)
                        (code-inside-the-macros nil)
                        (macro-headings nil)
                        (code-outside-the-macros nil)
                        (return-code nil)))

    :print-object-string ("<basic-strategy-blueprint>")
    :slots-for-obj= (initializations-inside-the-let
                     initializations-outside-the-let
                     macro-headings
                     code-inside-the-macros
                     code-outside-the-macros
                     return-code)
    :slots-for-clone (initializations-inside-the-let
                     initializations-outside-the-let
                     macro-headings
                     code-inside-the-macros
                     code-outside-the-macros
                     return-code))

(defgeneric generate-macro-calls (neighborhood-description
                                  search-strategy
                                  other-strategies
                                  where-to-store-it)
  (:documentation "This function receives a description of a neighborhood (in a human readable form) and returns a list where each element is the heading of the macro call derived from the corresponding neighborhood-instruction."))

(defparameter *exhaustive-search-macro-heading-mapping*
  `((select-route     1)
    (select-client    1 3)
    (insert-client    1 3)
    (swap-clients     1 2)
    (select-subroute  1 3)
    (insert-subroute  1 3)
    (reverse-subroute 1)
    (swap-subroutes   1 2)
    (add-route        1))
  "This list describe what are the relevant parameters in the pseudo-natural description of a neighborhood operation.  The car of each element is the pseudo-natural name of the neighborhood operation.  The rest of each element are (1-) of the position of the arguments we are interested in.")

(defgeneric get-macro-name-for-operation
    (operation strategy)
  (:documentation "Returns the name of the corresponding neighborhood-operation macro for an operation and a given strategy."))

(defmethod get-macro-name-for-operation
    (operation (strategy exhaustive-neighborhood-search-strategy))
  (symb 'do operation))

(defmethod get-macro-name-for-operation
    (operation
     (strategy exhaustive/efficient-neighborhood-search-strategy))
  (symb 'do operation '*))

(defgeneric map-pseudo-natural-op-to-do-neigborhood-heading
    (description strategy)
 (:documentation "Describes how to map a neighborhood-operation description in pseudo-natural language to the heading of the macro call."))

(defmethod map-pseudo-natural-op-to-do-neigborhood-heading
    (description strategy)

  (let* ((wanted-name (first description))
         (op-found nil)
         (macro-heading nil)
         (macro-args nil)
         (max-arg-position 0)
         (constraints nil))
    (loop for op in *exhaustive-search-macro-heading-mapping*
          for op-name = (first op)
          for op-args = (rest op)
          while (not op-found)
          when (eq op-name wanted-name)
               do (progn
                    ;; push the do-neighborhood operation macro
                    (push (get-macro-name-for-operation
                           op-name strategy)
                          macro-heading)
                    ;; op-was-found
                    (setf op-found t)
                    ;; store the greates arg pos
                    (setf max-arg-position
                          (reduce #'max op-args))
                    ;; now push the args
                    (loop for arg in op-args
                          do (push (nth arg description)
                                   macro-args)
                          finally (push `wc macro-args))))
    ;; After pushing the mandatory args
    ;; we need to push the constraints
    ;; By convention, everything after the
    ;; maximum positional arg should be a
    ;; constraint, so we need to check if
    ;; there is any of those
    (setf constraints (nthcdr (1+ max-arg-position) description))
    ;; if there are constraints we need
    ;; to push them into macro-heading.
    ;; we don't need to check if constraints
    ;; is non nil, because the loop will do it
    ;; for us.
    (loop for c in constraints
          doing (push c macro-args))

    ;; now, let's push the args
    (push (reverse macro-args) macro-heading)
    ;; return the macro-heading
    (reverse macro-heading)))

(defmethod generate-macro-calls
    ((description list)
     strategy
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (loop for op in description
        do (push
            (map-pseudo-natural-op-to-do-neigborhood-heading
             op strategy)
            (macro-headings where-to-store-it))))

(defmethod get-macro-name-for-operation
    (operation
     (strategy random-neighborhood-search-strategy))

  (symb "random-" operation))

(defmethod generate-macro-calls
    ((description list)
     (strategy random-neighborhood-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  ;; first we add the loop with the number of repetitions
  (push `(loop for random-search-repetition-counter
               from 1 to ,(neighborhood-size strategy)
               while (not *vrp-stop-neighborhood-search*) do)
        (macro-headings where-to-store-it))

  ;; and then we do the same is in the exhaustive-search.
  (loop for op in description
        do (push
            (map-pseudo-natural-op-to-do-neigborhood-heading
             op strategy)
            (macro-headings where-to-store-it))))

(defmethod generate-macro-calls
    ((description list)
     (strategy jump-around-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  ;; first we add the loop with the number of repetitions
  (push `(loop for jump-around-repetition-counter
               from 1 to ,(neighborhood-size strategy)
               while (not *vrp-stop-neighborhood-search*)
               ;; we need to prepare the wc for the exploration
               do (prepare-solution-for-neighborhood-exploration wc)
               ;; this is the last do to add the do-neig macros
               do)

        (macro-headings where-to-store-it))

  ;; and then we do the same as in the exhaustive-search.
  (loop for op in description
        do (push
            (map-pseudo-natural-op-to-do-neigborhood-heading
             op strategy)
            (macro-headings where-to-store-it))))

(defmethod map-pseudo-natural-op-to-do-neigborhood-heading
    (description (strategy looking-for-bug))

  (let* ((wanted-name (first description))
         (op-found nil)
         (macro-heading nil)
         (macro-args nil))
    (loop for op in *exhaustive-search-macro-heading-mapping*
          for op-name = (first op)
          for op-args = (rest op)
          while (not op-found)
          when (eq op-name wanted-name)
               do (progn
                    ;; push the do-neighborhood operation macro
                    (push (get-macro-name-for-operation
                           op-name strategy)
                          macro-heading)
                    ;; op-was-found
                    (setf op-found t)
                    ;; now push the args
                    (loop for arg in op-args
                          do (push (nth arg description)
                                   macro-args)
                          finally (push `wc macro-args))))
    ;; let's push the args
    (push (reverse macro-args) macro-heading)
    ;; now let's push the debug info:
    (push `(format t "debug: inside (~a ~a)~%"
                   ',(second macro-heading)
                   ',(first macro-heading))
          macro-heading)
    ;; return the macro-heading
    (reverse macro-heading)))

(defmethod get-macro-name-for-operation
    (operation
     (strategy looking-for-bug))

  (symb "random-" operation))

(defmethod generate-macro-calls
    ((description list)
     (strategy looking-for-bug)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  ;; first we add the loop with the number of repetitions
  (push `(loop for random-search-repetition-counter
               from 1 to 10
               while (not *vrp-stop-neighborhood-search*) do)
        (macro-headings where-to-store-it))

  ;; and then we do the same is in the exhaustive-search.
  (loop for op in description
        do (push
            (map-pseudo-natural-op-to-do-neigborhood-heading
             op strategy)
            (macro-headings where-to-store-it))))

(defgeneric generate-inside-let-initializations
    (neighborhood-description
     search-strategy
     other-strategies
     where-to-store-it)
  (:documentation "This function receives a description of a neighborhood (in a human readable form) updates the argument where-to-store-it with the initializations that should be made inside the let."))

(defmethod generate-inside-let-initializations
    (description
     strategy
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(*vrp-stop-neighborhood-search* nil)
        (initializations-inside-the-let where-to-store-it))
  (push `(ops-list nil)
        (initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :after
    (description
     strategy
     (other-strategies search-with-a-basic-wc)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(wc (make-working-copy (clone solution)))
        (initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :after
    (description
     strategy
     (other-strategies search-with-delta-cost-computation)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(current-delta-cost 0)
        (initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :after
    (description
     strategy
     (other-strategies there-is-a-best-solution)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(best-neighbor nil)
        (initializations-inside-the-let where-to-store-it))
  (push `(best-delta-cost initial-best-delta-cost)
        (initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :after
    (description
     strategy
     (other-strategies return-best-solution)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(best-neighbor-as-solution nil)
        (initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :after
    (description
     strategy
     (other-strategies has-candidates-for-best-neighbor)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(candidates-for-best-neighbor nil)
        (initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :after
    (description
     (strategy jump-around-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(cumulative-delta-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-delta-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-delta-cost-to-return 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-solution nil)
        (initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :around
    (description
     (strategy jump-around-search-strategy)
     (other-strategies jump-around-return-last-neighbor)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(*vrp-stop-neighborhood-search* nil)
        (initializations-inside-the-let where-to-store-it))
  (push `(ops-list nil)
        (initializations-inside-the-let where-to-store-it))
  (push `(wc (basic-working-copy (clone solution)))
        (initializations-inside-the-let where-to-store-it))
  (push `(current-delta-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(cumulative-delta-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-delta-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-delta-cost-to-return 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-solution nil)
        (initializations-inside-the-let where-to-store-it)))

(defgeneric generate-outside-let-initializations
    (neighborhood-description
     search-strategy
     other-strategies
     where-to-store-it)
  (:documentation "This function receives a description of a neighborhood (in a human readable form) updates the argument where-to-store-it with the initializations that should be made outside the let."))

(defmethod generate-outside-let-initializations
    (description
     strategy
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(prepare-solution-for-neighborhood-exploration wc)
        (initializations-outside-the-let where-to-store-it)))

(defmethod generate-outside-let-initializations :after
    (description
     (strategy exhaustive/efficient-neighborhood-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(initialize-action-for-delta-cost-computation wc problem action)
        (initializations-outside-the-let where-to-store-it)))

(defmethod generate-outside-let-initializations
    (description
     (strategy jump-around-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))
  "Don't do anything.  Wait for the auxiliary methods"
  nil)

(defgeneric generate-code-inside-macros
    (neighborhood-description
     search-strategy
     other-strategies
     where-to-store-it)
  (:documentation "This function receives a description of a neighborhood (in a human readable form) updates the argument where-to-store-it with the code that we should run inside the macros."))

(defmethod generate-code-inside-macros
    (description
     strategy
     other-strategies
     where-to-store-it)
  "Don't do anything, just wait for the auxiliary methods."
  nil)

(defmethod generate-code-inside-macros :after
    (description
     strategy
     (other-strategies compute-delta-cost-inefficiently)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(setf current-delta-cost
               (delta-cost (reverse ops-list) wc problem action))
        (code-inside-the-macros where-to-store-it)))

(defmethod generate-code-inside-macros :after
    (description
     strategy
     (other-strategies compute-delta-cost-with-smart-macros)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(progn

           (finish-delta-cost-computation wc problem action)
           (setf current-delta-cost
                 (get-delta-cost-from-action action))

           (undo-finish-delta-cost-computation wc problem action))

        (code-inside-the-macros where-to-store-it)))

(defmethod generate-code-inside-macros :after
    (description
     strategy
     (other-strategies best-improvement-search-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if (< current-delta-cost best-delta-cost)
             ;; if it is
             (then
               ;; let's save the ops that make it
               (setf best-neighbor (clone ops-list))
               ;; let's update the best-cost
               (setf best-delta-cost current-delta-cost)))
        (code-inside-the-macros where-to-store-it)))

(defmethod generate-code-inside-macros :after
    (description
     strategy
     (other-strategies first-improvement-search-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if (< current-delta-cost best-delta-cost)
             ;; if it is
             (then
               ;; let's save the ops that make it
               (setf best-neighbor (clone ops-list))
               ;; let's update the best-cost
               (setf best-delta-cost current-delta-cost)
               ;; stop the search
               (stop-neighborhood-search)))
        (code-inside-the-macros where-to-store-it)))

(defmethod generate-code-inside-macros :after
    (description
     strategy
     (other-strategies has-candidates-for-best-neighbor)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if (< current-delta-cost best-delta-cost)
             ;; if it is
             (then
               ;; let's push the neighbor into the
               ;; list of candidates
               (push (list (clone ops-list) current-delta-cost)
                     candidates-for-best-neighbor)))
        (code-inside-the-macros where-to-store-it)))

(defmethod generate-code-inside-macros :after
    (description
     strategy
     (other-strategies random-improvement-selection-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if (< current-delta-cost best-delta-cost)
             ;; if it is
             (then
               ;; let's make a random check:
               (let* ((r (random 1.0)))
                 (if (<= r ,(acceptance-ratio other-strategies))
                     ;; let's set the best-neighbor
                     ;; and stop the search
                     (then
                       (setf best-neighbor (clone ops-list))
                       (setf best-delta-cost current-delta-cost)
                       (stop-neighborhood-search))
                     (else ;; let's check if
                       ;; best-neighbor is bound
                       ;; if it isn't bind it to
                       ;; this neighbor
                       ;; [this would be a first-improvement]
                       (unless best-neighbor
                         (setf best-neighbor (clone ops-list))
                         (setf best-delta-cost current-delta-cost)))))))
        (code-inside-the-macros where-to-store-it)))

(defmethod generate-code-inside-macros :after
    (description
     (strategy jump-around-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(progn
           ;; as we have a cumulative-delta-cost
           ;; we need to update-it
           (incf cumulative-delta-cost current-delta-cost)

           ;; let's apply the operations
           (apply-set-of-operations
            (reverse ops-list) wc)

           ;; let's update the solution cost
           (setf (cost wc) 
                 (+ (cost wc)
                    (+ (delta-distance action)
                       (total-penalty action)))))
        (code-inside-the-macros where-to-store-it))

  (push
   ;; let's check for the best better solution
   `(if (< current-delta-cost best-jump-delta-cost)
        (then ;; we found a best-better-solution

          (setf best-jump-solution
                (clone (solution wc)))
          ;; we don't need to update the cost
          ;; of the best solution
          ;; because it was already done before.


          ;; the best-delta-cost is now 0
          ;; because the current-neighbor
          ;; is also updated.
          (setf best-jump-delta-cost 0)

          ;; here we set the best-jump-delta-cost-to-return
          ;; to the current-delta-cost
          ;; because that's the actual difference
          ;; from the initial solution
          (setf best-jump-delta-cost-to-return
                cumulative-delta-cost)
          )
        (else ;; if the current-cost is not better,
          ;; we need to update the best-delta-cost
          (incf best-jump-delta-cost (- current-delta-cost))))

        (code-inside-the-macros where-to-store-it)))

(defmethod generate-code-inside-macros :after
    (description
     strategy
     (other-strategies return-last-neighbor-selection-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(progn
           ;; let's set the best-neighbor
           (setf best-neighbor (clone ops-list))
           ;; let's update the best-cost
           (setf best-delta-cost current-delta-cost))
        (code-inside-the-macros where-to-store-it)))

(defgeneric generate-code-outside-macros
    (neighborhood-description
     search-strategy
     other-strategies
     where-to-store-it)
  (:documentation "This function receives a description of a neighborhood (in a human readable form) and updates the argument where-to-store-it with the code that we should run after the macros finished the exploration of the neighborhood."))

(defmethod generate-code-outside-macros
    (description
     strategy
     other-strategies
     where-to-store-it)
 "Don't do anything.  Wait for the auxiliary methods."
  nil)

(defmethod generate-code-outside-macros :after
    (description
     strategy
     (other-strategies return-best-solution )
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if best-neighbor
             (then
               (apply-set-of-operations (reverse best-neighbor) wc)
               (setf best-neighbor-as-solution
                     (solution wc))
               (setf (cost best-neighbor-as-solution)
                     (+ (cost solution)
                        best-delta-cost))))
        (code-outside-the-macros where-to-store-it)))

(defmethod generate-code-outside-macros
    (description
     (strategy jump-around-search-strategy)
     (other-strategies return-last-neighbor-selection-strategy )
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(setf best-neighbor-as-solution
           (clone (solution wc)))
        (code-outside-the-macros where-to-store-it)))

(defmethod generate-code-outside-macros :after
    (description
     strategy
     (other-strategies has-candidates-for-best-neighbor )
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if candidates-for-best-neighbor
             (then
               (let* ((selected-position
                       (random (length candidates-for-best-neighbor)))
                      (selected-info
                       (nth selected-position
                            candidates-for-best-neighbor)))
                 (setf best-neighbor
                       (first selected-info)
                       best-delta-cost
                       (second selected-info)))))
        (code-outside-the-macros where-to-store-it)))

(defgeneric generate-return-code
    (neighborhood-description
     search-strategy
     other-strategies
     where-to-store-it)
  (:documentation "This function receives a description of a neighborhood (in a human readable form) and updates the argument where-to-store-it with the code that we should run to return the result of the neighborhood exploration."))

(defmethod generate-return-code
    (description
     strategy
     other-strategies
     where-to-store-it)
 "Don't do anything.  Wait for the auxiliary methods."
  nil)

(defmethod generate-return-code :after
    (description
     strategy
     (other-strategies return-best-solution)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

    (push 'best-neighbor-as-solution
     (return-code where-to-store-it)))

(defmethod generate-return-code :after
    (description
     strategy
     (other-strategies return-best-delta-cost)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push 'best-delta-cost
        (return-code where-to-store-it)))

(defmethod generate-return-code :after
    (description
     (strategy jump-around-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push '(clone (solution wc))
        (return-code where-to-store-it))
  (push 'cumulative-delta-cost
        (return-code where-to-store-it))
  (push 'best-jump-solution
        (return-code where-to-store-it))
  (push 'best-jump-delta-cost-to-return
        (return-code where-to-store-it)))

(defgeneric prepare-blueprint-for-code-generation
    (neighborhood-description
     search-strategy
     other-strategies
     where-to-store-it)
  (:documentation "This function receives a list with the description of the neighborhood criterion, the wanted behavior (as instances of classes) and a blueprint and setups the blueprint with all the info required to write the actual code."))

(defmethod prepare-blueprint-for-code-generation
    ((description list)
     strategy
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  ;; first we generate the macro calls
  (generate-macro-calls
   description strategy other-strategies where-to-store-it)
  ;; then the inside-let
  (generate-inside-let-initializations
   description strategy other-strategies where-to-store-it)
  ;; next, the outside-let.
  (generate-outside-let-initializations
   description strategy other-strategies where-to-store-it)
  ;; Now what's inside the macros
  (generate-code-inside-macros
   description strategy other-strategies where-to-store-it)
  ;; and then what's outside them
  (generate-code-outside-macros
   description strategy other-strategies where-to-store-it)
  ;; finally, we set the return code
  (generate-return-code
   description strategy other-strategies where-to-store-it))

(defgeneric write-neighborhood-exploration-code
    (blueprint
     &key
       inside-let
       outside-let
       inside-macro
       outside-macro)

  (:documentation "A function to generate the actual code from a blueprint."))

(defmethod write-neighborhood-exploration-code 
    ((blueprint basic-neighborhood-strategy-blueprint)
     &key
     inside-let
     outside-let
     inside-macro
     outside-macro)

  (declare (ignorable inside-let
                      outside-let
                      inside-macro
                      outside-macro))

  (let* (;; first, let's compose the body
         ;; inside the macros
         ;; let's get the code-inside-the-macros
         (inner-code (reverse
                      (append
                      inside-macro
                      (code-inside-the-macros blueprint))))
         (headings (macro-headings blueprint))
         (macro-code nil)
         (let-code
          `(let* ,(reverse
                   (initializations-inside-the-let blueprint))))
         (result)
         )
    ;; let's create the code inside the first
    ;; of the macros
    (setf macro-code `(,@(first headings) ,@inner-code))
    ;; let's create the code inside the rest of the macros
    (loop for macro in (rest (macro-headings blueprint))
          doing (setf macro-code
                      `(,@macro ,macro-code)))

    ;; next we should write the code outside the macros
    (setf result `(,macro-code
                  ;; this is the code outside the macros
                  ,@(reverse (code-outside-the-macros blueprint))
                  ;; and this is the return code
                  (values ,@ (reverse (return-code blueprint)))))

    ;; now let's add the let and
    ;; the initializations outside the let
    (setf result (append
                  let-code
                  (reverse (initializations-outside-the-let blueprint))
                  result))

    ;; finally, the function heading:
    (setf result
          `(lambda (solution problem action
                    &optional (initial-best-delta-cost 0))
             (declare (ignorable initial-best-delta-cost
                                 solution problem action))

                    ,result))
    ;; and now, let's return result
    result))

(defgeneric make-neighborhood-exploration-function
    (blueprint
     &key
       inside-let
       outside-let
       inside-macro
       outside-macro)

  (:documentation "A function to create the functions that actually explore the neighborhood."))

(defmethod make-neighborhood-exploration-function 
    ((blueprint basic-neighborhood-strategy-blueprint)
     &key
     inside-let
     outside-let
     inside-macro
     outside-macro)


  (eval (write-neighborhood-exploration-code
         blueprint
         :inside-let inside-let
         :outside-let outside-let
         :inside-macro inside-macro
         :outside-macro outside-macro)))

(defun make-neighborhood-criterion
    (description
     search-strategy
     select-strategy
     &key
       inside-let
       outside-let
       inside-macro
       outside-macro)

  "This function receives a list with the pseudo-natural description of a neighborhood criterion, the strategies, and code to add to the criterion, and returns a function that explores the neighborhood using the given strategies."

  (let* ((blueprint (basic-strategy-blueprint)))

    (prepare-blueprint-for-code-generation
                     description
                     search-strategy
                     select-strategy
                     blueprint)

    (make-neighborhood-exploration-function
     blueprint
     :inside-let inside-let
     :outside-let outside-let
     :inside-macro inside-macro
     :outside-macro outside-macro)))

(defgeneric ops-list-to-list-of-clients (ops-list wc)
  (:documentation
    "A function to return a list of list of clients from an ops-list and a solution (or working copy)."))

(defmethod ops-list-to-list-of-clients ((ops-list list)
                                        (wc basic-working-copy))
    (let* ((original-solution (clone (solution wc)))
           (wc* (basic-working-copy original-solution))
           (solution nil))

      (prepare-solution-for-neighborhood-exploration wc*)
      (apply-set-of-operations (reverse ops-list) wc*)
      (setf solution (solution wc*))

      (loop for route in (routes solution)
            collecting (loop for c in (clients route)
                             collecting (id c)))))

(defmethod ops-list-to-list-of-clients ((ops-list (eql nil))
                                        (wc basic-working-copy))
    (let* ((original-solution (clone (solution wc))))

      (loop for route in (routes original-solution)
            collecting (loop for c in (clients route)
                             collecting (id c)))))

(defclass node ()
  ((value :accessor .value
           :initarg :value
           :initform :-1)
   (children-nodes :accessor .children-nodes
                    :initarg :children-nodes
                    :initform :nil)
   (parent :accessor .parent
            :initarg :parent
            :initform :nil)))

 (defun new-node (value &key children-nodes parent)
   (make-instance 'node
                  :value value
                  :children-nodes children-nodes
                  :parent parent))

(defun new-trie ()
  (new-node "r1"))

(defun insert-in-the-children-list (r node)
  (let ((flag 0))
    (loop for child in (.children-nodes r) do
          (if (eql (.value child) (.value node))
              (progn 
                (setf flag 1)
                (setf node child)
                (return child))))
    (if (eql flag 0)
        (push node (.children-nodes r)) )       
    node))

(defun insert-in-trie (solution root)
  (let* ((node nil)
         (leaf nil)               
         (visit 0))

    (loop for route in solution do 
          (loop for client in route do 
                (setf node (new-node client))
                (setf (.parent node) root)
                (setf root (insert-in-the-children-list root node)))
          (setf leaf root)
          (setf root
                (insert-in-the-children-list
                 root (new-node "r" :parent leaf))))


    (if (> (length (.children-nodes leaf)) 1)
        (setf visit 1))
    (if (not (> (length (.children-nodes leaf)) 1))
        (setf (.value root) -1))

    (loop while (> 1 0) do
          (if (eql (.parent root) nil)
              (progn
                ;; (format t "~a" (.value root))
                (return root)))
          ;; (format t "~a" (.value root))
          (setf root (.parent root)))
    visit))

(def-vrp-class repeated-solutions-search-strategy
 (search-with-a-basic-wc)
  ()
 :documentation "A class to represent a search where we store the repetead elements in a neighborhood."
 :constructor (repeated-solutions-search-strategy ())
 :print-object-string ("<repeated-solutions-search-strategy>")
 :slots-for-obj= ()
 :slots-for-clone ())

(defparameter +repeated-neighbors-search-strategy+
  (repeated-solutions-search-strategy))

(defmethod generate-inside-let-initializations :after
    (description
     strategy
     (other-strategies repeated-solutions-search-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(solutions-repeated nil) ;; a list with the repeated elements
        (initializations-inside-the-let where-to-store-it))
  (push `(current-trie (new-trie)) ;; the trie to store the visited clients
        (initializations-inside-the-let where-to-store-it)))

(defmethod generate-outside-let-initializations :after
    (description
     strategy
     (other-strategies repeated-solutions-search-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))


    (push `(insert-in-trie (ops-list-to-list-of-clients nil wc)
                            current-trie)
     (initializations-outside-the-let where-to-store-it)))

(defmethod generate-code-inside-macros :after
    (description
     strategy
     (other-strategies repeated-solutions-search-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

    (push `(progn
             (if (= (insert-in-trie
                     (ops-list-to-list-of-clients
                      ops-list wc)
                     current-trie)
                    1)
                 (then
                   (push (list
                          (clone (ops-list-to-list-of-clients
                                  ops-list wc))
                          (clone (reverse ops-list)))
                         solutions-repeated))))

     (code-inside-the-macros where-to-store-it)))

(defmethod generate-return-code :after
    (description
     strategy
     (other-strategies repeated-solutions-search-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push 'solutions-repeated
        (return-code where-to-store-it)))
