(in-package :vrp)

(defmethod create-neigh-operation-from ((symbol (eql 'select-route))
					element)
  ;; ('select-route ri)
  (declare (special operations-id))
  (let (result)
    (setf result (new-r-operation)
	  ;; here we save the route symbol
	  (route-symbol result) (second element)
	  ;; get the operation identifier
	  (operation-id result) (incf (getf operations-id (first element) -1)))
    result))

(defmethod create-neigh-operation-from ((symbol (eql 'select-client))
					element)
  ;; ('select-client ci from rj)
  (declare (special operations-id))
  (let (result)
    (setf result (new-a-operation)
	  ;; here we save the client and route symbol
	  (client-symbol result) (second element)
	  (route-symbol result) (fourth element)
	  ;; get the operation identifier
	  (operation-id result) (incf (getf operations-id (first element) -1)))
    result))

(defmethod create-neigh-operation-from ((symbol (eql 'insert-client))
					element)
  ;; ('select-client ci from rj)
  (declare (special operations-id))
  (let (result)
    (setf result (new-b-operation)
	  ;; here we save the client and route symbol
	  (client-symbol result) (second element)
	  (route-symbol result) (fourth element)
	  ;; get the operation identifier
	  (operation-id result) (incf (getf operations-id (first element) -1)))
    result))

(defmethod create-neigh-operation-from ((symbol (eql 'swap-clients))
					element)
  ;; (swap-clients ci cj)
  (declare (special operations-id))
  (let (result)
    (setf result (new-c-operation)
	  ;; here we save the client1 and client2 symbols
	  (client-1-symbol result) (second element)
	  (client-2-symbol result) (third element)
	  ;; get the operation identifier
	  (operation-id result) (incf (getf operations-id (first element) -1)))
    result))

(defmethod create-neigh-operation-from ((symbol (eql 'select-subroute))
					element)
  ;; ('select-subroute sri from rj)
  (declare (special operations-id))
  (let (result)
    (setf result (new-e-operation)
	  ;; here we save the route and subroute symbols
	  (subroute-symbol result) (second element)
	  (route-symbol result) (fourth element)
	  ;; get the operation identifier
	  (operation-id result) (incf (getf operations-id (first element) -1)))
    result))

(defmethod create-neigh-operation-from ((symbol (eql 'insert-subroute))
					element)
  ;; (insert-subroute sri into rj)
  (declare (special operations-id))
  (let (result)
    (setf result (new-f-operation)
	  ;; here we save the route and subroute symbols
	  (route-symbol result) (fourth element)
	  (subroute-symbol result) (second element)
	  ;; get the operation identifier
	  (operation-id result) (incf (getf operations-id (first element) -1)))
    result))

(defmethod create-neigh-operation-from ((symbol (eql 'swap-subroutes))
					element)
  ;; (swap-subroutes sri srj)
  (declare (special operations-id))
  (let (result)
    (setf result (new-g-operation)
	  ;; here we save the subroute1 and subroute2 symbols
	  (subroute-1-symbol result) (second element)
	  (subroute-2-symbol result) (third element)
	  ;; get the operation identifier
	  (operation-id result) (incf (getf operations-id (first element) -1)))
    result))

(defmethod create-neigh-operation-from ((symbol (eql 'reverse-subroute))
					element)
  ;; (reverse-subroute sri)
  (declare (special operations-id))
  (let (result)
    (setf result (new-h-operation)
	  ;; here we save the subroute symbol
	  (subroute-symbol result) (second element)
	  ;; get the operation identifier
	  (operation-id result) (incf (getf operations-id (first element) -1)))
    result))

(defun create-operation-instances-from (list)
  (let* (operations-id
	 route-ops
	 other-ops
	 (init-op (new-init-operation)))
    (declare (special operations-id))
    (loop for element in list
       doing
	 (if (eql (first element) 'select-route)
	     (push (create-neigh-operation-from (first element) element)
		   route-ops)
	     (push (create-neigh-operation-from (first element) element)
		   other-ops)))
    (append (list init-op) (reverse route-ops) (reverse other-ops))))
