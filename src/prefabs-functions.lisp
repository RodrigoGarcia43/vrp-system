(in-package :vrp)

(defgeneric generate-loop-conditional (neighborhood-description
			   search-strategy
			   other-strategies
			   where-to-store-it)
  (:documentation "this funtions generates loop conditional."))

(defmethod generate-loop-conditional
    (description
     strategy
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))
  ())

(defmethod generate-loop-conditional
    (description
     strategy
     (other-strategies use-neigh-tree)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `current-solution
	(loop-conditional where-to-store-it)))

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

  (push `(code (quote ,description))
	(initializations-inside-the-let where-to-store-it))
  (push `(*vrp-stop-neighborhood-search* nil)
	(initializations-inside-the-let where-to-store-it))
  (push `(ops-list nil)
	(initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :before
    (description
     (strategy exhaustive-neighborhood-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(expl-type 'exhaustive-exploration)
	(initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :before
    (description
     (strategy random-neighborhood-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(expl-type 'random-exploration)
	(initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :after
    (description
     strategy
     (other-strategies use-eval-graph)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(current-cost (output-value (output graph)))
        (initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :after
    (description
     strategy
     (other-strategies use-neigh-tree)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(neigh-tree (build-neighborhood-tree code solution))
	(initializations-inside-the-let where-to-store-it))
  (push `(sol-generator (funcall expl-type neigh-tree))
	(initializations-inside-the-let where-to-store-it))
  (push `(current-solution (funcall sol-generator))
	(initializations-inside-the-let where-to-store-it)))

(defmethod generate-inside-let-initializations :after
    (description
     strategy
     (other-strategies there-is-a-best-solution)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(best-neighbor nil)
        (initializations-inside-the-let where-to-store-it))
  (push `(best-cost current-cost)
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

  (push `(cumulative-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-cost-to-return 0)
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
  (push `(current-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(cumulative-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-cost 0)
        (initializations-inside-the-let where-to-store-it))

  (push `(best-jump-cost-to-return 0)
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
  ())

(defmethod generate-outside-let-initializations
    (description
     (strategy jump-around-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))
  "Don't do anything.  Wait for the auxiliary methods"
  nil)

(defgeneric generate-code-inside-loop
    (neighborhood-description
     search-strategy
     other-strategies
     where-to-store-it)
  (:documentation "This function receives a description of a neighborhood (in a human readable form) updates the argument where-to-store-it with the code that we should run inside the loop."))

(defmethod generate-code-inside-loop
    (description
     strategy
     other-strategies
     where-to-store-it)
  "Don't do anything, just wait for the auxiliary methods."
  nil)

(defmethod generate-code-inside-loop :after
		    (description
		     strategy
		     (other-strategies use-eval-graph)
		     (where-to-store-it basic-neighborhood-strategy-blueprint))



		  (push `(setf ops-list (from-coordinates-to-operations current-solution))
			(code-inside-the-loop where-to-store-it))
;		  (push `(format t "~a ~%" ops-list)
;			(code-inside-the-loop where-to-store-it))
	  
		  (push `(do-suite-operations graph ops-list) 
			(code-inside-the-loop where-to-store-it))
		  (push `(setf current-cost (output-value (output graph)))
			(code-inside-the-loop where-to-store-it))
		  (push `(undo-suite-operations graph ops-list)
			(code-inside-the-loop where-to-store-it))
		  (push `(setf current-solution (funcall sol-generator))
			(code-inside-the-loop where-to-store-it)))

(defmethod generate-code-inside-loop :after
    (description
     strategy
     (other-strategies best-improvement-search-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if (< current-cost best-cost)
             ;; if it is
             (then
               ;; let's save the ops that make it
               (setf best-neighbor (clone ops-list))
               ;; let's update the best-cost
               (setf best-cost current-cost)))
        (code-inside-the-loop where-to-store-it)))

(defmethod generate-code-inside-loop :after
    (description
     strategy
     (other-strategies first-improvement-search-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if (< current-cost best-cost)
             ;; if it is
             (then
               ;; let's save the ops that make it
               (setf best-neighbor (clone ops-list))
               ;; let's update the best-cost
               (setf best-cost current-cost)
               ;; stop the search
               (stop-neighborhood-search)))
        (code-inside-the-loop where-to-store-it)))

(defmethod generate-code-inside-loop :after
    (description
     strategy
     (other-strategies has-candidates-for-best-neighbor)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if (< current-cost best-cost)
             ;; if it is
             (then
               ;; let's push the neighbor into the
               ;; list of candidates
               (push (list (clone ops-list) current-cost)
                     candidates-for-best-neighbor)))
        (code-inside-the-loop where-to-store-it)))

(defmethod generate-code-inside-loop :after
    (description
     strategy
     (other-strategies random-improvement-selection-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if (< current-cost best-cost)
             ;; if it is
             (then
               ;; let's make a random check:
               (let* ((r (random 1.0)))
                 (if (<= r ,(acceptance-ratio other-strategies))
                     ;; let's set the best-neighbor
                     ;; and stop the search
                     (then
                       (setf best-neighbor (clone ops-list))
                       (setf best-cost current-cost)
                       (stop-neighborhood-search))
                     (else ;; let's check if
                       ;; best-neighbor is bound
                       ;; if it isn't bind it to
                       ;; this neighbor
                       ;; [this would be a first-improvement]
                       (unless best-neighbor
                         (setf best-neighbor (clone ops-list))
                         (setf best-cost current-cost)))))))
        (code-inside-the-loop where-to-store-it)))

(defmethod generate-code-inside-loop :after
    (description
     (strategy jump-around-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(progn
           ;; as we have a cumulative-delta-cost
           ;; we need to update-it
           (incf cumulative-cost current-cost)

           ;; let's apply the operations
           (apply-set-of-operations
            (reverse ops-list) wc)

           ;; let's update the solution cost
           (setf (cost wc) 
                 (+ (cost wc)
                    (+ (delta-distance action)
                       (total-penalty action)))))
        (code-inside-the-loop where-to-store-it))

  (push
   ;; let's check for the best better solution
   `(if (< current-cost best-jump-cost)
        (then ;; we found a best-better-solution

          (setf best-jump-solution
                (clone (solution wc)))
          ;; we don't need to update the cost
          ;; of the best solution
          ;; because it was already done before.


          ;; the best-cost is now 0
          ;; because the current-neighbor
          ;; is also updated.
          (setf best-jump-cost 0)

          ;; here we set the best-jump-cost-to-return
          ;; to the current-cost
          ;; because that's the actual difference
          ;; from the initial solution
          (setf best-jump-cost-to-return
                cumulative-cost)
          )
        (else ;; if the current-cost is not better,
          ;; we need to update the best-cost
          (incf best-jump-cost (- current-cost))))

        (code-inside-the-loop where-to-store-it)))

(defmethod generate-code-inside-loop :after
    (description
     strategy
     (other-strategies return-last-neighbor-selection-strategy)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(progn
           ;; let's set the best-neighbor
           (setf best-neighbor (clone ops-list))
           ;; let's update the best-cost
           (setf best-cost current-cost))
        (code-inside-the-loop where-to-store-it)))

(defgeneric generate-code-outside-loop
    (neighborhood-description
     search-strategy
     other-strategies
     where-to-store-it)
  (:documentation "This function receives a description of a neighborhood (in a human readable form) and updates the argument where-to-store-it with the code that we should run after the loop finished the exploration of the neighborhood."))

(defmethod generate-code-outside-loop
    (description
     strategy
     other-strategies
     where-to-store-it)
 "Don't do anything.  Wait for the auxiliary methods."
  nil)

(defmethod generate-code-outside-loop :after
    (description
     strategy
     (other-strategies return-best-solution )
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(if best-neighbor
             (then
               (do-suite-operations graph best-neighbor)
               (setf best-neighbor-as-solution
                     (neigh-tree-solution (solution-track graph)))
               (setf (cost best-neighbor-as-solution)
                     best-cost)))
        (code-outside-the-loop where-to-store-it)))

(defmethod generate-code-outside-loop
    (description
     (strategy jump-around-search-strategy)
     (other-strategies return-last-neighbor-selection-strategy )
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push `(setf best-neighbor-as-solution
           (clone (solution-track graph)))
        (code-outside-the-loop where-to-store-it)))

(defmethod generate-code-outside-loop :after
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
                       best-cost
                       (second selected-info)))))
        (code-outside-the-loop where-to-store-it)))

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
     (other-strategies return-best-cost)
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push 'best-cost
        (return-code where-to-store-it)))

(defmethod generate-return-code :after
    (description
     (strategy jump-around-search-strategy)
     other-strategies
     (where-to-store-it basic-neighborhood-strategy-blueprint))

  (push '(clone (solution-track graph))
        (return-code where-to-store-it))
  (push 'cumulative-cost
        (return-code where-to-store-it))
  (push 'best-jump-solution
        (return-code where-to-store-it))
  (push 'best-jump-cost-to-return
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

  ;; first we generate the loop
  (generate-loop-conditional
   description strategy other-strategies where-to-store-it)
  ;; then the inside-let
  (generate-inside-let-initializations
   description strategy other-strategies where-to-store-it)
  ;; next, the outside-let.
  (generate-outside-let-initializations
   description strategy other-strategies where-to-store-it)
  ;; Now what's inside the loop
  (generate-code-inside-loop
   description strategy other-strategies where-to-store-it)
  ;; and then what's outside them
  (generate-code-outside-loop
   description strategy other-strategies where-to-store-it)
  ;; finally, we set the return code
  (generate-return-code
   description strategy other-strategies where-to-store-it))

(defgeneric write-neighborhood-exploration-code
    (blueprint
     &key
       inside-let
       outside-let
       inside-loop
       outside-loop)

  (:documentation "A function to generate the actual code from a blueprint."))

(defmethod write-neighborhood-exploration-code 
    ((blueprint basic-neighborhood-strategy-blueprint)
     &key
       inside-let
       outside-let
       inside-loop
       outside-loop)

  (declare (ignorable inside-let
		      outside-let
		      inside-loop
		      outside-loop))

  (let* (;; first, let's compose the body
	 ;; inside the loop
	 ;; let's get the code-inside-the-loop
	 (inner-code (reverse
		      (append
		       inside-loop
		       (code-inside-the-loop blueprint))))
	 ;;(headings (macro-headings blueprint))
	 (conditional (loop-conditional blueprint))
	 (macro-code nil)
	 (let-code
	   `(let* ,(reverse
		    (initializations-inside-the-let blueprint))))
	 (result)
	 )

    ;; let's create the code inside the first
    ;; of the macros
    ;;		  (setf macro-code `(,@(first headings) ,@inner-code))
    ;; let's create the code inside the rest of the macros
    ;;		  (loop for macro in (rest (macro-headings blueprint))
    ;;			doing (setf macro-code
    ;;				    `(,@macro ,macro-code)))

    ;; Let's create the main loop with conditional and inner-code
    (setf macro-code `(loop while ,@conditional do ,@inner-code))

    ;; next we should write the code outside the macros
    (setf result `(,macro-code
		   ;; this is the code outside the macros
		   ,@(reverse (code-outside-the-loop blueprint))
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
	  `(lambda (solution problem graph
		    &optional (initial-best-cost 0))
	     (declare (ignorable initial-best-cost
				 solution problem graph))

	     ,result))
    ;; and now, let's return result
    result))

(defgeneric make-neighborhood-exploration-function
    (blueprint
     &key
       inside-let
       outside-let
       inside-loop
       outside-loop)

  (:documentation "A function to create the functions that actually explore the neighborhood."))

(defmethod make-neighborhood-exploration-function 
    ((blueprint basic-neighborhood-strategy-blueprint)
     &key
     inside-let
     outside-let
     inside-loop
     outside-loop)


  (eval (write-neighborhood-exploration-code
         blueprint
         :inside-let inside-let
         :outside-let outside-let
         :inside-loop inside-loop
         :outside-loop outside-loop)))

(defun make-neighborhood-criterion
    (description
     search-strategy
     select-strategy
     &key
       inside-let
       outside-let
       inside-loop
       outside-loop)

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
     :inside-loop inside-loop
     :outside-loop outside-loop)))
