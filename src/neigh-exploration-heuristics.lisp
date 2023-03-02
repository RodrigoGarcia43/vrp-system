(in-package :vrp)

(defun exploration-exploitation-algorithm (neighborhood graph &optional (file-name "data") technique)
       ;; here we set the statistical technique that we are going to use
       (when technique
	 (setf *statistical-technique* technique))

       (let* (
	      ;; data for general purpose
	      (problem (problem neighborhood))
	      ;;(solution (solution neighborhood))
	      ;; (code (criterion neighborhood))
	      (num-clients (length (clients problem)))
	      ;; (num-routes (length (routes solution)))

	      ;; data for exploration status
	      (on-exploration-phase t)
	      ;; total number of solutions that can be analysed in the exploration phase
	      (total-to-expl (* *linear-constant-in-total-to-explore*
				(expt num-clients 2)))
	      ;; combinatorial generator for the exploration phase
	      (exploration-gen (combinatorial-exploration neighborhood (* total-to-expl *exploration-proportion*)))
	      ;; table to store the processed neighbor solutions
	      (result-table (make-hash-table :test 'equal))


	      ;; data for intensification status
	      on-intensification-phase
	      region-ordered-intensification-list

	      result
	      a
	      b
	      ;; data for computing delta cost
	      ;; (work-cop (basic-working-copy solution))
	      ;; (action (delta-cvrp-action))
	      )

	 ;; some initializations that have to be done for delta cost computation
	 ;; (prepare-solution-for-neighborhood-exploration work-cop)
	 ;; (initialize-action-for-delta-cost-computation work-cop problem action)

	 #'(lambda ()
	     (let ((current-solution (funcall exploration-gen)))
	       (if current-solution
		   ;; if branch
		   (let* (
			  ;; here we compute the operation list for this coordinates
			  (current-ops-list (from-coordinates-to-operations current-solution))
			  ;; here we compute the delta cost for this operations
			  ;; (current-delta-cost (delta-cost current-ops-list ;; the operation list
			  ;; 				     work-cop         ;; the working copy
			  ;; 				     problem          ;; the problem
			  ;; 				     action           ;; the action
			  ;;))
			  )
		     ;; if we are still in the exploration phase then
		     ;; we add the current solution to the table for statistical analysis
		     ;; before the intensification phase

		     (do-suite-operations graph current-ops-list)
		     (setf a (clone (solution-track graph)))  
		     (when on-exploration-phase
		       (push (output-value (output graph))
			     (gethash (from-coordinate-list-to-region-tuple current-solution)
				      result-table)))
		     (setf result (output-value (output graph)))

		     (undo-suite-operations graph current-ops-list) 
		     (setf b (clone (solution-track graph)))   
		     ;; here we return the current operation list and its delta cost evaluation
		     (values current-ops-list result)))

	       ;; else branch
	       ;; on this branch is when the transition between exploration and intensification
	       ;; takes places, for that reason the statistical analysis is made in this branch.
	       (progn
		 (setf on-exploration-phase nil)
		 (if on-intensification-phase
		     ;; if branch
		     (values nil nil)
		     ;; else branch		      
		     (let* ((non-exhausted-regions (mapcar #'(lambda (reg) (number-id reg))
							   (remove-if #'(lambda (reg)
									  (is-exhausted-p reg))
								      (region-list neighborhood)))))
		       (if (null non-exhausted-regions)
			   ;; then we have explore the entire neighborhood
			   (progn
			     ;; here we make a generator that always return nil
			     (setf exploration-gen (lambda () nil)
				   on-intensification-phase t)
			     (values nil nil))
			   ;; else
			   (progn
			     ;; use a statistical technique to compute an order for the non-exhausted neighborhhod
			     ;; regions		      
			     (setf region-ordered-intensification-list
				   (funcall *statistical-technique*
					    neighborhood
					    result-table
					    non-exhausted-regions
					    file-name))

			     ;; here we make the sequential generator with the previously computed list
			     (setf exploration-gen (sequential-exploration neighborhood
									   region-ordered-intensification-list
									   (* total-to-expl *intensification-proportion*)
									   ))
			     ;; now we are in the intensification phase
			     (setf on-intensification-phase t
				   current-solution (funcall exploration-gen))

			     (let* (
				    ;; here we compute the operation list for this coordinates
				    (current-ops-list (from-coordinates-to-operations current-solution)))
;;			       (do-suite-operations graph current-ops-list)
;;			       (setf a (clone (solution-track graph)))  
			       (setf result (output-value (output graph)))  
			       (undo-suite-operations graph current-ops-list) 
;;			       (setf b (clone (solution-track graph)))  
			       (values current-ops-list result)))))))))))
