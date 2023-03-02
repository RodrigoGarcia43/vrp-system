(in-package :vrp)

(defgeneric actual-position (coordinate working-copy)
  (:documentation "A function to return the coordinates in the original solution of the client that in the working-copy is at coordinate coordinate."))

(defmethod actual-position (coordinate (wc basic-working-copy))
  "Return the result of the function in the top of the actual-position-functions-stack in the working-copy."
  (funcall (first (actual-position-functions-stack wc)) coordinate))

(defgeneric make-initial-actual-position (working-copy)
 (:documentation "A function to create the initial actual-position function."))

(defmethod make-initial-actual-position ((wc basic-working-copy))
  "Sets the actual-position-functions-stack of the working copy appropriately to an identity function."
  (setf (actual-position-functions-stack wc)
        (list (lambda (coord) coord))))

(defgeneric update-actual-position-after (op working-copy)
 (:documentation "A function to update the actual-position function in a working-copy."))

;; Method for select-client operation
(defmethod update-actual-position-after
    ((operation operation-select-client)
     (wc basic-working-copy))
  ;; first we get the actual function from te stack
  (let* ((old-function (first (actual-position-functions-stack wc)))
         (new-function
          (lambda (coord)
            ;; coord is of the form (i j)
            ;; where i is the route and j is the position.
            ;; first let's increment p if it is in route r
            ;; and it is greater or equal to p
            (let* ((i (first coord))
                   (j (second coord))
                   (newj (if (and
                              (obj= i (route operation))
                              (>= j (pos operation)))
                             (then (+ 1 j))
                             (else j))))
              ;; now let's call the previous function
              (funcall old-function (list i newj))))))

    ;; now we push the new function into the stack
    (push new-function (actual-position-functions-stack wc))))

;; Method for select-client operation
(defmethod update-actual-position-after
    ((operation operation-insert-client)
     (wc basic-working-copy))
  ;; first we get the actual function from te stack
  (let* ((old-function (first (actual-position-functions-stack wc)))
         (new-function
          (lambda (coord)
            ;; coord is of the form (i j)
            ;; where i is the route and j is the position.
           ;; first let's decrement j if it is in route i
           ;; and it is greater or equal to j
            (let* ((i (first coord))
                   (j (second coord))
                   (newj (if (and
                              (obj= i (route operation))
                              (>= j (pos operation)))
                             (then (- j 1))
                             (else j))))
              ;; now let's call the previous function
              (funcall old-function (list i newj))))))

    ;; now we push the new function into the stack
    (push new-function (actual-position-functions-stack wc))))

(defgeneric undo-actual-position (op working-copy)
  (:documentation "A function to undo the update of the working-copy when the operation op is simulated."))

;; Method for select-client operation
(defmethod undo-actual-position
    (operation (wc basic-working-copy))
  ;; pop from the stack
  (pop (actual-position-functions-stack wc)))

(defgeneric make-initial-routes-lengths (working-copy)
  (:documentation "Sets the slot initial-route-length of the working copy to its value according to the solution."))

(defmethod make-initial-routes-lengths ((wc basic-working-copy))
 "This function returns an array with the lengths of each route in the given basic solution. It assumes that the id of the routes are consecutive and ordered from 1 to n."

 (let* ((solution (solution wc))
        (initial-lengths (loop for route in (routes solution)
                               collecting (length (clients route))))
        (length-array (make-array (1+ (length initial-lengths))
                                  :initial-contents
                                  (append '(0) initial-lengths)
                                  :fill-pointer (1+ (length initial-lengths))
                                  :adjustable t)))
   (setf (initial-routes-lengths wc) length-array)))

(defgeneric update-routes-lengths-after (operation working-copy)
  (:documentation "Updates the current routes-length in the given working copy according to the operation was was made."))

(defmethod update-routes-lengths-after ((op operation-select-client)
                                        (wc basic-working-copy))
 "This function updates the routes-length property of the working copy when the working copy is a basic solution and the operation is a select-client operation."

 ;; here we just decrease the number in the route's
 ;; idth element in the routes-lengths slot in the working-copy. 
 (decf (aref (routes-lengths wc) (route op))))

(defmethod update-routes-lengths-after ((op operation-insert-client)
                                        (wc basic-working-copy))
 "This function updates the routes-length property of the working copy when the working copy is a basic solution and the operation is an insert-client operation."

 ;; here we just increase the number in the route's
 ;; idth element in the routes-lengths slot in the working-copy. 
 (incf (aref (routes-lengths wc) (route op))))

(defgeneric route-length (route working-copy)
  (:documentation "Returns the length of the given route in the working-copy."))

(defmethod route-length ((route number) (wc basic-working-copy))
  "Check the value of the property route-length-array in the given position in the given working copy."
  (aref (routes-lengths wc) route))

(defgeneric undo-routes-lengths (operation working-copy)
  (:documentation "Undoes the effect of the update in the routes-lengths properties of the working-copy."))

(defmethod undo-routes-lengths ((op operation-select-client)
                                (wc basic-working-copy))
 "This function undoes the effect of an update in the routes-lengths property of the working copy when the working copy is a basic solution and the operation is a select-client operation."

 ;; here we just increase the number in the route's
 ;; idth element in the routes-lengths slot in the working-copy. 
 (incf (aref (routes-lengths wc) (route op))))

(defmethod undo-routes-lengths ((op operation-insert-client)
                                (wc basic-working-copy))
 "This function undoes the effect of an update in the routes-lengths property of the working copy when the working copy is a basic solution and the operation is an insert-client operation."

 ;; here we just decrease the number in the route's
 ;; idth element in the routes-lengths slot in the working-copy. 
 (decf (aref (routes-lengths wc) (route op))))

(defgeneric initialize-number-of-routes (working-copy)
  (:documentation "This function initializes the number of routes in the working copy.  It should be initialized to the number of routes in the solution."))

(defmethod initialize-number-of-routes
    ((wc basic-working-copy))
 "This function initializes the number of routes in the working copy.  It should be initialized to the number of routes in the solution."
 ;; here we just increase the number in the route's
 ;; idth element in the routes-lengths slot in the working-copy. 
 (setf (number-of-routes wc) (length (routes (solution wc)))))

(defgeneric get-route-with-id (id working-copy)
  (:documentation "Returns the route with the given id in the working copy."))

(defmethod get-route-with-id ((id number) (solution basic-solution))
  "In this function we are going to assume that in a solution there is only a route with a given id."
  (loop for route in (routes solution)
        when (= id (id route)) do (return route)))

(defmethod get-route-with-id
    ((id number) (wc basic-working-copy))
  "In this function we are going to assume that in a solution there is only a route with a given id."
  (loop for route in (routes (solution wc))
        when (= id (id route)) do (return route)))

(defgeneric get-client-at-pos (pos working-copy)
  (:documentation "Returns the client at the given pos in the working copy."))

(defmethod get-client-at-pos ((pos list) (solution basic-solution))
    "This function returns the client at the given pos in a basic solution. pos is a list with two elements (r,p). r is the id of the desired route, and p is the position of the desired client in the route with id r.

In this function we are going to assume that in a solution there is only a route with a given id, and that the position inside the route is 1-based. That means that the first element in the route is at pos 1.
TODO: Maybe this approach is inneficient for big instances because of the nth function."
    (let* ((route-id (first pos))
           (client-pos (second pos))
           selected-route)
      ;; get the selected-route
      (setf selected-route (get-route-with-id route-id solution))
      ;; return the (client-pos - 1)th element in the route
      ;; the -1 is because we want the position to be 1-based
      ;; The next step is potentially a bottleneck because
      ;; of the nth function. That should be improved in future
      ;; versions.
      (nth (- client-pos 1) (clients selected-route))))

(defgeneric there-was-an-insertion-at (pos working-copy)
  (:documentation "If there-was-an-insertion-at the given position returns the client that was inserted.  Otherwise returns nil."))

(defmethod there-was-an-insertion-at ((pos list)
                                      (wc basic-working-copy))
  "If there was an insertion at the given position in the given basic-solution, return the client that was inserted.  Otherwise return nil.
pos is a list of the form (r,p) where r is the id of the route and p is the position inside that route. The position is 1 based, meaning that the position of the first element is 1.

This function uses the variable *insertions-made*."

  (loop for (r p c) in (insertions-made wc)
        ;; if there was a match, return it
        if (and (= r (first pos))
                (= p (second pos)))
        do (return c)
        ;; if there was no match, return nil.
        finally (return nil)))

(defgeneric client-at (pos working-copy)
  (:documentation "Returns the client at the given pos in the given working copy."))

(defmethod client-at ((pos list) (wc basic-working-copy))
  "Returns the client at position pos in solution after some selections or insertions have been simulated.  solution is not actually modified by these selections or insertions.  If the position in the route is 0 or greater than the route-length, then return the depot in that route."

  (let* ((solution (solution wc))
         (route-id (first pos))
         (actual-route nil)
         (client-pos (second pos))
         (there-was-an-insertion (there-was-an-insertion-at pos wc))
         actual-pos)
    (cond
      ((< client-pos 1)
       (then ;; return the depot of the route
         (setf actual-route (get-route-with-id route-id solution))
         (if actual-route
             (depot actual-route)
             (depot wc))))
      ((> client-pos (route-length route-id wc))
       (then ;; return the depot of the route
         (setf actual-route (get-route-with-id route-id solution))
         (if actual-route
             (end-depot actual-route)
             (end-depot wc))))
      (t (else
         (if there-was-an-insertion
             (then ;; return it
               there-was-an-insertion)
             (else ;; get the client in the actual-pos
               (setf actual-pos
                     (actual-position (list route-id client-pos) wc))
               ;; return that value
               (get-client-at-pos actual-pos solution))))))))

(defgeneric initialize-selected-clients (working-copy)
 (:documentation "A function initialize the selected-clients in a working copy."))

(defmethod initialize-selected-clients ((wc basic-working-copy))
  "initialize an array with size the number of clients in the routes of the solution. This is the length of the array because that's the maximum number of clients that can be selected."
  (setf (selected-clients wc)
        (make-array
         ;; the size of the array is the number
         ;; of clients in the solution, because this is the
         ;; maximum number of possible selections.
         (loop for c across (initial-routes-lengths wc)
               summing c)
         :initial-element nil)))

(defgeneric client-selected-at-operation (op working-copy)
 (:documentation "Returns client selected at the operation op."))

(defmethod client-selected-at-operation ((op number)
                                         (wc basic-working-copy))
  "Returns client selected at the operation with operand op."
  (first (aref (selected-clients wc) op)))

(defgeneric info-of-client-selected-at-operation (op working-copy)
 (:documentation "Returns the coordinates of the client selected at the operation op."))

(defmethod info-of-client-selected-at-operation
    ((op number)
     (wc basic-working-copy))
  "Returns info of the client selected at the operation with operand op."
  (second (aref (selected-clients wc) op)))

(defgeneric route-of-client-selected-at-operation (op working-copy)
 (:documentation "Returns the route of the client selected at the operation op."))

(defmethod route-of-client-selected-at-operation
    ((op number)
     (wc basic-working-copy))
  "Returns info of the client selected at the operation with operand op."
  (first (second (aref (selected-clients wc) op))))

(defgeneric pos-of-client-selected-at-operation (op working-copy)
 (:documentation "Returns the route of the client selected at the operation op."))

(defmethod pos-of-client-selected-at-operation
    ((op number)
     (wc basic-working-copy))
  "Returns info of the client selected at the operation with operand op."
  (second (second (aref (selected-clients wc) op))))

(defgeneric incf-pos-of-client-selected-at-operation (op working-copy)
 (:documentation "Incfs the pos of the client selected at the operation op."))

(defmethod incf-pos-of-client-selected-at-operation
    ((op number)
     (wc basic-working-copy))
  "Returns info of the client selected at the operation with operand op."
  (incf (second (second (aref (selected-clients wc) op)))))

(defgeneric decf-pos-of-client-selected-at-operation (op working-copy)
 (:documentation "Decfs the pos of the client selected at the operation op."))

(defmethod decf-pos-of-client-selected-at-operation
    ((op number)
     (wc basic-working-copy))
  "Returns info of the client selected at the operation with operand op."
  (decf (second (second (aref (selected-clients wc) op)))))

(defun pp-selected-clients-array (wc &optional (ind 2) (stream t))
  (if (< (selections-count wc) 0)
      (then (format t "selected-clients: NIL~%"))
      (else
        (format stream "selected-clients:~%")
        (loop for op from 0 to (selections-count wc)
              doing (format stream "~a~a: ~a~%"
                            (make-string ind :initial-element #\SPACE)
                            op 
                            (aref (selected-clients wc) op))))))

(defgeneric initialize-original-positions (working-copy)
  (:documentation "Initializes the original-positions slot in the working-copy."))

(defmethod initialize-original-positions ((wc basic-working-copy))
  "Initializes the original-positions array in the working-copy."
  ;; let's get the data from the insertion object.
  (let* ((number-of-clients (loop for r across (routes-lengths wc)
                                  summing r))
         (solution (solution wc)))
    ;; let's create the array
    (setf (original-positions wc) 
          (make-array (1+ number-of-clients)))
    ;; now let's fill the array
    (loop for r in (routes solution)
          for r-id = (id r)
          do (loop for c in (clients r)
                   for pos from 1
                   do (progn
                        (setf (aref (original-positions wc) (id c))
                              (list r-id pos)))))))

(defgeneric client-c1-was-before-client-c2  (c1 c2 working-copy)
  (:documentation "Returns non nil if client c1 was in the same route as c2 and in a smaller position."))

(defmethod client-c1-was-before-client-c2
    ((c1 number) (c2 number) (wc basic-working-copy))
  "Returns non nil if, in the original solution, the client with id c1 was in a smaller position than the client with id c2 and in the same route. We don't check that they are in the same route. That is supposed to be done by the caller."
  ;; now let's compare the second value of the entries c1 and c2
  ;; in the slot original-positions of the working-copy.
    (< (second (aref (original-positions wc) c1))
       (second (aref (original-positions wc) c2))))

(defgeneric update-selected-clients-after (op working-copy)
 (:documentation "A function to update the selected-clients in a working copy after some operation has been made."))

(defmethod update-selected-clients-after
    ((op operation-select-client)
     (wc basic-working-copy))
  "Update the positions of all the previously selected clients according to the new selection, increment =selections-count=, and add the new element to the array."
  (let* ((selections-count (selections-count wc))
         (route (route op))
         (pos   (pos op)))
    ;; let's update the positions of the previously selected clients
    ;; in the same route than this selection
    (loop for op from 0 to selections-count
          when (and ;; the client is in the same route as the selection
                (= (route-of-client-selected-at-operation op wc)
                   route)
                ;; and the position is greater than the one
                ;; in this in op
                ;; (it cannot be the same because if they are the same
                ;;  it means that the previously selected client was
                ;;  in a position smaller than the one we are selecting
                ;;  now and thus, this selection does not affect it.)
                (> (pos-of-client-selected-at-operation op wc) pos))
          do (decf-pos-of-client-selected-at-operation op wc))
    ;; let's incf the selections-count
    (incf (selections-count wc))
    ;; let's store the information of the new insertion
    (setf (aref (selected-clients wc) (operand op))
          (list (client-at (list route pos) wc)   ;; the selected client
                (list route pos)        ;; the coordinates of the client
                nil                     ;; mark it as not yet inserted
                ))))

(defmethod update-selected-clients-after ((op operation-insert-client)
                                          (wc basic-working-copy))
    "Update the positions of all the previously selected clients according to the new selection."
    (let* ((selections-count (selections-count wc))
           (route (route op))
           (pos   (pos op)))
      ;; let's update the positions of the previously selected clients
      ;; in the same route than this selection
      (loop for op from 0 to selections-count
            when (and ;; the client is in the same route as the selection
                         (= (route-of-client-selected-at-operation op wc)
                            route)
                         ;; and the position is greater or equal to the one in
                         ;; this in op increment the position
                         (>= (pos-of-client-selected-at-operation op wc) pos))
            do (incf-pos-of-client-selected-at-operation op wc))))

(defgeneric undo-selected-clients (op working-copy)
 (:documentation "A function to undo the effects of the update in the selected-clients of a working copy when an operation is simulated."))

(defmethod undo-selected-clients
    ((operation operation-select-client)
     (wc basic-working-copy))
  "Increment the position of all the previously selected-clients in the same route as the one in the op, and decrease the value of =selections-count=."
  ;; let's decrease the value of the =selections-count=
  (decf (selections-count wc))
  ;; increment the pos of all the previously selected-clients
  ;; in the operations route.
  (let* ((selections-count (selections-count wc))
         (route (route operation))
         (pos   (pos operation)))
    ;; let's update the positions of the previously selected clients
    ;; in the same route than this selection
    (loop for op from 0 to selections-count
          ;; if the the client op is was in a different route
          ;; than the client in the op that we are undoing
          ;; there is nothing to do. We are only interested in
          ;; the case where they both are in the same route.
          ;; That's what we check in the condition of the =when=.
          when ;; the client is in the same route as the selection
               (= (route-of-client-selected-at-operation op wc)
                   route)
          do (progn ;; there are two (interesting) options
               (cond
                 ;; the client is after the position
                 ;; of the selection we are undoing
                 ((> (pos-of-client-selected-at-operation op wc)
                     pos)
                  ;; in this case we incf the position of the element
                  (incf-pos-of-client-selected-at-operation op wc))
                 ;; the other (and the actually) interesting
                 ;; case is when the client is at the
                 ;; same position than the
                 ;; selection that we are undoing
                 ((= (pos-of-client-selected-at-operation op wc)
                     pos)
                  ;; in this case we should only increase
                  ;; its position if the client selected
                  ;; at op was after the client selected
                  ;; at the selection we are undoing.
                  ;; We can check that with the function
                  ;; client-c1-was-before-client-c2
                  (let* ((c-id ;; the id of the client
                          ;; selected at the op
                          ;; we are undoing
                          (id (client-selected-at-operation
                               (operand operation) wc)))
                         (op-id ;; the id of the client
                          ;; at the position op
                          ;; in the selected-clients
                          (id (client-selected-at-operation
                               op wc))))
                    (if (client-c1-was-before-client-c2
                         c-id op-id wc)
                        (incf-pos-of-client-selected-at-operation
                         op wc)))))))))

(defmethod undo-selected-clients ((op operation-insert-client)
                                  (wc basic-working-copy))
  "Undoes the update of the positions of all the previously selected clients according to the new selection."
  (let* ((selections-count (selections-count wc))
         (route (route op))
         (pos   (pos op)))
    ;; let's update the positions of the previously selected clients
    ;; in the same route than this selection
    (loop for op from 0 to selections-count
          when (and ;; the client is in the same route as the selection
                (= (route-of-client-selected-at-operation op wc)
                   route)
                ;; and the position is greater or equal to the one in
                ;; this in op increment the position
                (> (pos-of-client-selected-at-operation op wc) pos))
          do (decf-pos-of-client-selected-at-operation op wc))))

(defgeneric update-insertions-after  (op working-copy)
  (:documentation "A function to update the insertions made during the neighborhood exploration."))

(defmethod update-insertions-after  ((operation operation-insert-client)
                              (wc basic-working-copy))
  "Updates the insertions-made list of working-copy with the new insertion."
  ;; let's get the data from the insertion object.
  (let*
      ((insertions-made (insertions-made wc))
       (route-id (route operation))
       (pos (pos operation))
       (cindex (operand operation))
       (client (client-selected-at-operation cindex wc)))

    ;; update the insertions previously made to the same route
    (loop for (r p c) in insertions-made
          for insertion in insertions-made
          if (and (= r route-id)
                  (>= p pos))
          do (incf (second insertion)))
    ;; push the insertion into the list
    (push `(,route-id ,pos ,client) (insertions-made wc))))

(defmethod update-insertions-after  ((operation operation-select-client)
                              (wc basic-working-copy))
"After a selection operation we just update the insertions previously made to the same route."
(let*
    ((route-id (route operation))
     (pos (pos operation)))
  ;; update the insertions previously made to the same route
  (loop for (r p c) in (insertions-made wc)
     for insertion in (insertions-made wc)
     if (and (= r route-id)
             (>= p pos))
     do (decf (second insertion)))))

(defgeneric undo-insertions  (op working-copy)
  (:documentation "A function to undo the updates due to the insertions made during the neighborhood exploration."))

(defmethod undo-insertions  ((operation operation-insert-client)
                             (wc basic-working-copy))
  "Remove the insertion from the insertions-made list of the working-copy and restores the values of the previously made insertions to the same route."
  ;; let's remove the insertions from the insertions list
  ;; it should be the first element of the list
  ;; because it is added with a push
  (pop (insertions-made wc))
  ;; now restore the pos of the previously inserted clients
  ;; in the same route than the one in the operation
  (let* ((route (route operation))
         (pos   (pos   operation))
         (insertions-made (insertions-made wc)))
    (loop for (r p c) in insertions-made
                for insertion in insertions-made
                if (and (= r route)
                        (> p pos))
                do (decf (second insertion)))))

(defmethod undo-insertions ((op operation-select-client)
                            (wc basic-working-copy))
  "After a selection operation we just undo the updates in the insertions-made due to the insertions previously made to the same route.  As we are not going to allow the selections of clients after the first insertion has been made, there is nothing to undo here."
  nil)

(defgeneric get-inserted-client-info (operand working-copy)
 (:documentation "Returns the coordinates where the client selected at op operand was inserted."))

(defmethod get-inserted-client-info
    ((op number)
     (wc basic-working-copy))
  "Returns info of the client selected at the operation with operand op."
  (let* ((client (client-selected-at-operation op wc)))
    ;; now let's get the info about the insertion
    (loop for (r p c) in (insertions-made wc)
          when (obj= c client)
          do (return (list r p)))))

(defgeneric get-inserted-client-route (op working-copy)
 (:documentation "Returns the route where the client selected at the operation op was inserted."))

(defmethod get-inserted-client-route
    ((op number)
     (wc basic-working-copy))
  "Returns the route where the client selected at the operation with operand op was inserted."
  (first (get-inserted-client-info op wc)))

(defgeneric get-inserted-client-pos (op working-copy)
 (:documentation "Returns the position where the client selected at the operation op was inserted."))

(defmethod get-inserted-client-pos
    ((op number)
     (wc basic-working-copy))
  "Returns the position where the client selected at the operation with operand op was inserted."
  (second (get-inserted-client-info op wc)))

(defgeneric prepare-solution-for-neighborhood-exploration
    (working-copy)
 (:documentation "A function to create the working copy from a given solution."))

(defmethod prepare-solution-for-neighborhood-exploration
    ((wc basic-working-copy))
  "Initializes the working-copy to start a neighborhood exploration."

  ;; initialize the actual-position function
  (make-initial-actual-position wc)
  ;; initialize the insertions-made to nil
  (setf (insertions-made wc) nil)
  ;; add the number of routes
  (initialize-number-of-routes wc)
  ;; add the info about the routes lengths
  ;;   first the initial routes
  (make-initial-routes-lengths wc)
  ;;   and now the current routes length
  (setf (routes-lengths wc) (clone (initial-routes-lengths wc)))
  ;; set the selections count to -1
  (setf (selections-count wc) -1)
  ;; initialize-selected-clients
  (initialize-selected-clients wc)

  ;; initialize the original positions
  (initialize-original-positions wc)

  ;; initialize the selected-subroutes info
  (setf (selected-subroutes wc) nil)

  ;; initialize the selected-clients-during-apply
  (setf (selected-clients-during-apply wc) nil)

  )

(defgeneric pp-route (route working-copy stream)
  (:documentation "Pretty prints the given route from the given working copy to the given stream."))

(defmethod pp-route ((route number)
                     (wc basic-working-copy)
                     stream)
    "Pretty prints a route of the given working copy using the client-at and the route-length functions, by writing to the given stream."

    (let* ((length (route-length route wc)))
      (format stream "  R~a: (" route)
      (if (> length 0)
          (then
            (format stream "~a"
                    (id 
                     (funcall 'client-at (list route 1) wc)))
            (loop for pos from 2 to length 
                  doing (format stream ", ~a"
                                (id (client-at (list route pos) wc))))))
      (format stream ")")))

(defmethod pp-route ((route-id number)
                     (wc basic-solution)
                     stream)
    "Pretty prints a route of the given solution using the client-at and the route-length functions, by writing to the given stream."

    (let* ((route (get-route-with-id route-id wc))
           (length (length (clients route))))
      (format stream "  R~a: (" route-id)
      (if (> length 0)
          (then
            (format stream "~a"
                    (id 
                     (get-client-at-pos (list route-id 1) wc)))

            (loop for pos from 2 to length 
                  doing (format stream ", ~a"
                                (id (get-client-at-pos
                                         (list route-id pos) wc))))))
      (format stream ")")))

(defgeneric pp-solution ( working-copy stream)
  (:documentation "Pretty prints the given solution (usually a working-copy) to the given stream."))

(defmethod pp-solution ((wc basic-working-copy) stream)
  "Pretty prints a basic-working-copy using the client-at and the route-length functions."
  ;; first let's print the id of the working copy
  (format stream "S~a:~%" (id (solution wc)))
  ;; now let's print each route on one line each
  (loop for route-id from 1 to (number-of-routes wc)
        ;; add some indentation,
        doing (format stream "  ")
        ;; pp the route,
        doing (pp-route route-id wc stream)
        ;; add a newline after the route.
        doing (format stream "~%")))

(defmethod pp-solution ((wc basic-solution) stream)
  "Pretty prints a basic-working-copy using the client-at and the route-length functions."
  ;; first let's print the id of the working copy
  (format stream "S~a:~%" (id wc))
  ;; now let's print each route on one line each
  (loop for route in (routes wc)
        for id = (id route)
        ;; add some indentation,
        doing (format stream "  ")
        ;; pp the route,
        doing (pp-route id wc stream)
        ;; add a newline after the route.
        doing (format stream "~%")))

(defgeneric simulate-neighborhood-operation 
    (op working-copy)
  (:documentation "Updates the working copy in a way that client-at will behave as if the operation described by the first argument had actually taken place."))

(defmethod simulate-neighborhood-operation
    (op (wc basic-working-copy))
  "Simulates a selection in a basic-working-copy."

  (update-selected-clients-after op wc)
  (update-insertions-after       op wc)
  (update-actual-position-after  op wc)
  (update-routes-lengths-after   op wc))

(defgeneric undo-neighborhood-operation 
    (op working-copy)
  (:documentation "Undoes the updates done by the simulate-neighborhood-operation when processing the given op."))

(defmethod undo-neighborhood-operation
   (operation (working-copy basic-working-copy))
 "Undoes the effect of the operation passed as the first argument in the working-copy. It restores the actual-position, the routes-lengths, the insertions-made and the selected-clients to the state they were before the operation was simulated."
 ;; first we undo the effect in the route-lengths
 (undo-routes-lengths operation working-copy)
 ;; now we undo the actual-positions
 (undo-actual-position operation working-copy)
 ;; now we undo the effect on the insertions-made
 (undo-insertions operation working-copy)
 ;; undo the selected-clients
 (undo-selected-clients operation working-copy))

(defgeneric initialize-selected-clients-during-apply
    (length working-copy)
  (:documentation "Initializes the selected-clients-during-apply slot in the given working-copy."))

(defmethod initialize-selected-clients-during-apply
    ((length number) (solution basic-working-copy))
  "Initializes the selected-clients-during-apply slot in the given working-copy."
  (setf  (selected-clients-during-apply solution)
         (make-array (1+ length) :initial-element nil)))

(defgeneric store-client-selected-during-apply
    (index client working-copy)
  (:documentation "Stores the information about the client selected at op index in the working-copy, during the application of the operation."))

(defmethod store-client-selected-during-apply
    (index client (solution basic-working-copy))
  "Stores the given client in the given position."
  (setf 
   (aref (selected-clients-during-apply solution) index)
   (list client)))

(defgeneric retrieve-client-selected-during-apply
    (index working-copy)
  (:documentation "Retrieves the stored information about the client selected at op index in the working-copy, during the application of the operation."))

(defmethod retrieve-client-selected-during-apply
    ((index number) (solution basic-working-copy))
  "Retrieves the client stored at the given index."
  (first (aref (selected-clients-during-apply solution) index)))

(defgeneric apply-neighborhood-operation
    (op working-copy)
  (:documentation "Applies the operation to the given working copy.  Destructively modifies the solution."))

(defbehavior apply-neighborhood-operation
   ((operation operation-select-client)
    (working-copy basic-working-copy))
 :log-str "Removing selected client"

 "Physically modifies the solution to remove the client specified by the operation.  It also stores the selected client."
 (let* ((route-id (route operation))
        (pos (pos operation))
        (route (get-route-with-id route-id (solution working-copy)))
        (clients (clients route))
        (index (operand operation)))
   ;; now let's remove the client

   ;; (format t "debug: inside apply-op. Route:~%~a~%"
   ;;         route)
   (cond
     ;; if pos is 0 then set the route to the rest of it
     ((= pos 1)
      (setf (clients route) (rest (clients route)))
      (store-client-selected-during-apply
       index (first clients) working-copy))
     ;; else traverse the route collecting
     ;; all the clients except the one in the
     ;; selected pos.
     (t (setf (clients route)
              (loop for c in clients
                    for i from 1
                    if (= i pos)
                       do (store-client-selected-during-apply
                           index c working-copy)
                    else
                       collect c
                    end))))
   ))

(defbehavior apply-neighborhood-operation
   ((operation operation-insert-client)
    (wc basic-working-copy))
 :log-str "Applying op insert-client"

 "Physically modifies the solution to insert the client specified by the operation.  It uses the stored client."
 (let* ((solution (solution wc))
        (route-id (route operation))
        (pos (pos operation))
        (route (get-route-with-id route-id solution))
        (index (operand operation))
        (client (retrieve-client-selected-during-apply
                 index wc))
        (clients (clients route))
        (length (length clients)))
   ;; ;; now let's insert the client into the route
   (setf (clients route)
         (cond
           ((= pos 1) ;; the position is the first
            ;; just append the clients to the selected one
            (append (list client) clients)
            )
           ((> pos length) ;; insert it at the end
            ;; append the client to the clients
            (append clients (list client)))
           (t ;; insert it in the middle
            ;; :-/, traverse the list and insert it
            ;; where it should be.
            (loop for c in (clients route)
                  for i from 1
                  if (= i pos) collect client
                  collect c))))))

(defgeneric finish-apply-neighborhood-operations
    (working-copy)
  (:documentation "Does everything that should be done after all the neighborhood operations have been applied to the working-copy."))

(defbehavior finish-apply-neighborhood-operations
   ((working-copy basic-working-copy))
 :log-str "Do nothing.  Wait for after methods."

 "Do nothing.  Wait for after methods."
 nil)

(defbehavior finish-apply-neighborhood-operations :after 
   ((working-copy has-infinite-fleet))
 :log-str "Removing empty routes."

 "Physically removes all the empty routes and renumber the remaining ones."
 (let* ((solution (solution working-copy))
        (new-routes nil)
        (new-id 1))
   ;; now let's remove the client

   ;; (format t "debug: inside finish-apply-op. Route:~%~a~%"
   ;;         route)
   (loop for route in (routes solution)
         when (clients route)
         ;; when the route is non empty
         ;; that is, it has clients
         do (progn
              ;; first we push the route
              (push route new-routes)
              ;; then, we change the id
              (setf (id (first new-routes)) new-id)
              ;; and finally we incf the new-id
              (incf new-id)))
   ;; now set the routes of the solution
   ;; as the new-routes reversed
   ;; (because they were pushed)
   (setf (routes solution) (reverse new-routes))))

(defgeneric apply-set-of-operations
    (operations working-copy)
  (:documentation "Applies the set of operations to the given solution.  It destructively modifies the working-copy."))

;;; A generic function to apply a set of operations
(defmethod apply-set-of-operations
    ((set-of-operations list)
     (wc basic-working-copy))
  "Applies the set of operations to the given basic-working-copy. It destructively modifies the working-copy."

  ;; initialize the slot selected-clients-for-apply
  (initialize-selected-clients-during-apply (length set-of-operations) wc)

  ;; apply each of the operations
  (loop for op in set-of-operations
          do (apply-neighborhood-operation op wc))
  ;; finally, finish-apply-neighborhood-operations
  (finish-apply-neighborhood-operations wc))

(defparameter *vrp-stop-neighborhood-search* nil
  "A variable to decide whether or not to stop the neighborhood search.")

(defun stop-neighborhood-search ()
  "Stops the exploration of a neighborhood by setting the variable *vrp-stop-neighborhood-search* to T."
  (declare (special *vrp-stop-neighborhood-search*))
  (setf *vrp-stop-neighborhood-search* T))

(defparameter *vrp-max-route-length* 20000
  "A value so big that no route should have that number of clients.")

(defparameter *vrp-do-macros-constraint-symbols*
  `(ge ;; greater or equal to (route pos) or (pos)
    gt ;; greater than (route pos) or (pos)
    le ;; less or equal to (route pos) or (pos) 
    lt ;; less than (route pos) or (pos)
    dt ;; different to (route pos) or (pos)
    ex ;; exclude (route pos) or (pos)
    ex-cond ;; exclude the current coord if this code
            ;; returns non nil
    )
  "A variable with the symbols that can be used as keyword arguments in a do-neighborhood macro.")

(defmacro make-do-neighborhood-route-selection
    (macro-name
     (&rest args)
     &key
       op-name
       op-params
       example-str
       (simpler-ops-macro
        'simulate-simpler-ops-macro)
       (undo-simpler-ops-macro
        'undo-simpler-ops-macro))
     ;;{{{ documentation
     "Creates a do-neighborhood macro for a selection. The created macro searches the neighborhood in an exhaustive way.

      There will always be a gensym named `pos' that will iterate through all the positions in the route. One of the parameter passed by the user should be named `route'.

      The arguments are:

         - macro-name :: the name of the macro.
         - args :: the args that the created macro will receive.
                   Apparently we'll always need parameters named `cname',
                   `route', and `working-copy'.
                    - `cname' should be a symbol that will be bound to
                      (1+ (selections-count working-copy)).
                    - `route' should be a number with the id of a route
                       in the working-copy, where the selections should
                       take place.
         - op-name :: the constructor of the neighborhood operation to use.
         - op-params :: the params that should be passed to the constructor
                        of the operation.
         - example-str ::  this is a string to add to the description of the macro with an intended usage example.
         - simpler-ops-macro :: is the name of a macro to be used to simulate the simpler-ops. It defaults to simulate-simpler-ops-macro.
         - undo-simpler-ops-macro :: is the name of a macro to be used to undo the simulation of the simpler-ops. It defaults to undo-simpler-ops-macro."

;;;}}}

     (let* ((comment-str (format nil "Iterates through each position in the given route, creates an instance of ~a, pushes it int ops-lists, simulates it, do whatever the users wants to do, and finally restores everything to the previous state.

   The user can control what clients to select with the keyword args `le', `lt', `ge', `gt', and `dt'.
      Example:
         ~a" op-name example-str))

            (actual-args (append
                          args
                          `(&key 
                            ,@*vrp-do-macros-constraint-symbols*))))
       `(defmacro ,macro-name (,actual-args &body body)
          ,comment-str


          (let* ((pos (symb cname ".position"))
                 (cname.route (symb cname ".route"))
                 (cname.client (symb cname ".client"))
                 (cname.position (symb cname ".select.position"))
                 (cname.coord (symb cname ".select.coord"))
                 (cname.prev.coord (symb cname ".prev.insert.coord"))
                 )

            (with-gensyms (op
                           simpler-ops
                           initial-pos
                           final-pos
                           initial-target-route
                           final-target-route
                           exclude-target-route
                           exclude-pos
                           default-final-position)
              `(symbol-macrolet ((,cname.position
                                  (pos-of-client-selected-at-operation
                                   ,cname ,working-copy))
                                 (,cname.route
                                  (route-of-client-selected-at-operation
                                   ,cname ,working-copy))
                                 (,cname.client
                                  (client-selected-at-operation
                                   ,cname ,working-copy))
                                 (,cname.coord
                                  (info-of-client-selected-at-operation
                                   ,cname ,working-copy))
                                 (,cname.prev.coord
                                  (let* ()
                                    (cond
                                      ((> ,cname.position 1)
                                       (list ,cname.route
                                             (1- ,cname.position)))
                                      ;; we are in (1 1)
                                      ;; so there is not previous
                                      ((= ,route 1)
                                       (list 1 -1))
                                      ;; otherwise the previous
                                      ;; is the last of the
                                      ;; previous route
                                      (t
                                       (list (1- ,route)
                                             (1+
                                              (route-length
                                               (1- ,route)
                                               ,working-copy)))))))
                                 )
                   (with-variables-for-constraints-for-selection
                      (,initial-pos ,ge ,gt
                       ,final-pos ,le ,lt
                       ,route ,working-copy
                       ,initial-target-route
                       ,final-target-route
                       ,exclude-target-route
                       ,exclude-pos
                       ,dt) 

                     (let* ((,cname.route ,route)
                            (,default-final-position
                             (route-length ,route ,working-copy)))

                       (declare (ignorable ,cname.route))

                       (loop for ,pos from
                             ;; in the initial pos we'll
                             ;; take into account
                             ;; the target route
                             (initial-for-value-in-all-macros
                              ,initial-target-route
                              ,route
                              ,initial-pos)
                             to ;; ;; let's set the final pos
                             ;; ;; according to the arguments
                             (final-value-for-loop-in-route-operation-macro
                                  ,final-target-route
                                  ,route
                                  ,final-pos
                                  ,default-final-position)
                             ;; the following line allows the final user
                             ;; to stop the iteration by setting that
                             ;; variable to t
                             while (not *vrp-stop-neighborhood-search*)

                             do ;; let's execute the body if
                             ;; we are not in a forbidden coord
                             (unless-forbidden-condition (,ex-cond)
                               (unless-forbidden-coord
                                (,ex
                                 ,route
                                 ,pos
                                 ,dt
                                 ,exclude-target-route ,exclude-pos)
                                (let* ((,cname
                                        (1+ (selections-count ,working-copy)))
                                       (,op
                                        (,',op-name ,,@op-params))
                                       (,simpler-ops
                                        (get-simpler-operations-from
                                         ,op ,working-copy)))


                                  ;; here I want to add some code to be
                                  ;; executed before the simulation of
                                  ;; the operation but I'm not yet sure
                                  ;; about how to do it right :-/

                                  ;; now the simulation
                                  (,',simpler-ops-macro
                                   ,simpler-ops ,working-copy)

                                  ;; let's do what the user wants
                                  (progn ,@body)

                                  ;; now the clean up
                                  (,',undo-simpler-ops-macro
                                   ,simpler-ops
                                   ,working-copy)))))))))))))

(defmacro make-do-neighborhood-route-insertion% (macro-name
                                                 (&rest args)
                                                 &key
                                                   op-name
                                                   op-params
                                                   example-str)
    ;;{{{ documentation
    "Creates a do-neighborhood macro for a insertion. The created macro searches the neighborhood in an exhaustive way.

 There will always be a gensym named `pos' that will iterate through all the positions in the route. One of the parameter passed by the user should be named `route'.

 The arguments are:

    - macro-name :: the name of the macro.
    - args :: the args that the created macro will receive.
              Apparently we'll always need parameters named `cname',
              `route', and `working-copy'.
               - `cname' should be a symbol that will be bound to
                 (1+ (selections-count working-copy).
               - `route' should be a number with the id of a route
                  in the working-copy, where the selections should
                  take place.
    - op-name :: the constructor of the neighborhood operation to use.
    - op-params :: the params that should be passed to the constructor
                   of the operation.
    - example-str ::  this is a string to add to the description of the macro with an intended usage example." ;;;}}}

    (let* ((comment-str (format nil "Iterates through each position in the given route, creates an instance of ~a, pushes it int ops-lists, simulates it, do whatever the users wants to do, and finally restores everything to the previous state.
 Example:
    ~a" op-name example-str))
           (actual-args (append
                        args
                        `(&key 
                          ,@*vrp-do-macros-constraint-symbols*)))
           )
      `(defmacro ,macro-name (,actual-args &body body)
         ,comment-str

         (let* ((pos (symb cname ".ins.pos"))
                (cname.ins.route (symb cname ".insert.route"))
                (cname.ins.position (symb cname ".insert.position")))
           (with-gensyms (op
                          simpler-ops
                          initial-pos
                          final-pos
                          initial-target-route
                          final-target-route
                          exclude-target-route
                          exclude-pos)
             `(symbol-macrolet ((,cname.ins.position
                                 (get-inserted-client-pos
                                     ,cname ,working-copy))
                                   (,cname.ins.route
                                    (get-inserted-client-route
                                     ,cname ,working-copy)))
                ;; here compute the appropriate
                ;; values for the variables
                ;; related to the constraints
                (with-variables-for-constraints-for-insertion
                    (,initial-pos ,ge ,gt
                     ,final-pos ,le ,lt
                     ,route ,working-copy
                     ,initial-target-route
                     ,final-target-route
                     ,exclude-target-route
                     ,exclude-pos
                     ,dt)


                   ;; for this point we start the insertions
                   (loop for ,pos from
                         ;; in the initial pos we'll take into account
                         ;; the target route
                         (if ,initial-target-route
                             (then ;; select the appropriate initial-pos
                               (cond
                                 ((= ,initial-target-route ,route)
                                  ,initial-pos)
                                 ((< ,route ,initial-target-route)
                                  ,*vrp-max-route-length*)
                                 (t 1)))
                             (else ;; set pos to initial-pos
                               ;; because we apply the
                               ;; constraint to all the routes
                               ,initial-pos))

                         to ;; ;; let's set the final pos
                         ;; ;; according to the constraints
                         (if ,final-target-route
                          (then ;; select the appropriate final-pos
                            (cond
                              ((= ,final-target-route ,route)
                               (min ,final-pos
                                    (1+ (route-length ,route wc))))
                              ((> ,route ,final-target-route)
                               ;; don't iterate through this route
                               ;; so set the final pos to
                               -1)
                              (t ;; we are in a route before
                               ;; the final-target-route
                               ;; so we can iterate through all the route
                               (1+ (route-length ,route ,working-copy)))))
                          (else
                            (min ,final-pos
                                 (1+ (route-length ,route wc)))))

                         ;; the following line allows the final user
                         ;; to stop the iteration by setting that
                         ;; variable to t
                         while (not *vrp-stop-neighborhood-search*)

                         ;; let's execute the body if
                         ;; we are not in a forbidden coord
                         do (unless (or ;; we have two scenarios
                                     ;; in one...
                                     (and ,dt ;; we have a different to
                                          ;; with a route
                                          ,exclude-target-route
                                          ;; and we are in that route
                                          (= ,exclude-target-route ,route)
                                          ;; and that position
                                          (= ,exclude-pos ,pos)
                                          ;; so don't do anything
                                          )
                                     ;; in the other scenario
                                     (and ,dt ;; we have a different to
                                          ;; but we don't have a route
                                          (null ,exclude-target-route)
                                          ;; so, if we are in that
                                          ;; forbidden position,
                                          ;; don't do anything
                                          (= ,exclude-pos ,pos)))
                              ;; if we are here, it means that
                              ;; we are not in the forbidden coord
                              ;; so let's do our thing
                              (let* ((,op (,',op-name ,,@op-params))
                                     ;; get the simpler operations
                                     (,simpler-ops
                                      (get-simpler-operations-from
                                       ,op ,working-copy)))


                                ;; now the simulation
                                (simulate-simpler-ops-macro
                                 ,simpler-ops ,working-copy)

                                ;; let's do what the user wants
                                (progn ,@body)

                                ;; now the clean up
                                (undo-simpler-ops-macro
                                 ,simpler-ops ,working-copy)))))))))))

(defmacro make-do-neighborhood-route-insertion (macro-name
                                                (&rest args)
                                                &key
                                                  op-name
                                                  op-params
                                                  example-str
                                                  (simpler-ops-macro 'simulate-simpler-ops-macro)
                                                  (undo-simpler-ops-macro 'undo-simpler-ops-macro))
       ;;{{{ documentation
       "Creates a do-neighborhood macro for a insertion. The created macro searches the neighborhood in an exhaustive way.

    There will always be a gensym named `pos' that will iterate through all the positions in the route. One of the parameter passed by the user should be named `route'.

    The arguments are:

       - macro-name :: the name of the macro.
       - args :: the args that the created macro will receive.
                 Apparently we'll always need parameters named `cname',
                 `route', and `working-copy'.
                  - `cname' should be a symbol that will be bound to
                    (1+ (selections-count working-copy).
                  - `route' should be a number with the id of a route
                     in the working-copy, where the selections should
                     take place.
       - op-name :: the constructor of the neighborhood operation to use.
       - op-params :: the params that should be passed to the constructor
                      of the operation.
       - example-str ::  this is a string to add to the description of the macro with an intended usage example."
    ;;;}}}

       (let* ((comment-str (format nil "Iterates through each position in the given route, creates an instance of ~a, pushes it int ops-lists, simulates it, do whatever the users wants to do, and finally restores everything to the previous state.
    Example:
       ~a" op-name example-str))
              (actual-args (append
                           args
                           `(&key 
                             ,@*vrp-do-macros-constraint-symbols*)))
              )
         `(defmacro ,macro-name (,actual-args &body body)
            ,comment-str

            (let* ((pos (symb cname ".ins.pos"))
                   (cname.ins.route (symb cname ".insert.route"))
                   (cname.ins.position (symb cname ".insert.position")))
              (with-gensyms (op
                             simpler-ops
                             initial-pos
                             final-pos
                             initial-target-route
                             final-target-route
                             exclude-target-route
                             exclude-pos
                             default-final-position)
                `(symbol-macrolet ((,cname.ins.position
                                    (get-inserted-client-pos
                                        ,cname ,working-copy))
                                      (,cname.ins.route
                                       (get-inserted-client-route
                                        ,cname ,working-copy)))
                   ;; here compute the appropriate
                   ;; values for the variables
                   ;; related to the constraints
                   (with-variables-for-constraints-for-insertion
                       (,initial-pos ,ge ,gt
                        ,final-pos ,le ,lt
                        ,route ,working-copy
                        ,initial-target-route
                        ,final-target-route
                        ,exclude-target-route
                        ,exclude-pos
                        ,dt)

                     ;; let's set the value for
                     ;; default-final-pos
                     ;; to (1+ (route-length ,route ,working-copy)).
                     ;; The 1+ is because we can insert at the
                     ;; end of the route.
                     (let* ((,default-final-position
                             (1+ (route-length ,route ,working-copy))))

                       ;; for this point we start the insertions
                       (loop for ,pos from
                             ;; in the initial pos we'll take into account
                             ;; the target route
                             (initial-for-value-in-all-macros
                              ,initial-target-route
                              ,route
                              ,initial-pos)

                             to ;; ;; let's set the final pos
                             ;; ;; according to the constraints
                             (final-value-for-loop-in-route-operation-macro
                              ,final-target-route
                              ,route
                              ,final-pos
                              ,default-final-position)

                             ;; the following line allows the final user
                             ;; to stop the iteration by setting that
                             ;; variable to t
                             while (not *vrp-stop-neighborhood-search*)

                             ;; let's execute the body if
                             ;; we are not in a forbidden coord
                             do
                             (unless-forbidden-coord
                                 (,ex
                                  ,route
                                  ,pos
                                  ,dt
                                  ,exclude-target-route
                                  ,exclude-pos
                                  ,ex-cond)

                               ;; if we are here, it means that
                               ;; we are not in the forbidden coord
                               ;; so let's do our thing
                               (let* ((,op (,',op-name ,,@op-params))
                                      ;; get the simpler operations
                                      (,simpler-ops
                                       (get-simpler-operations-from
                                        ,op ,working-copy)))


                                 ;; now the simulation
                                 (,',simpler-ops-macro
                                  ,simpler-ops ,working-copy)

                                 ;; let's do what the user wants
                                 (progn ,@body)

                                 ;; now the clean up
                                 (,',undo-simpler-ops-macro
                                    ,simpler-ops ,working-copy))))))))))))

(defmacro make-do-neighborhood-static-macro
    (name 
     args
     constructor-name
     constructor-args
     doc-string
     &key
       (simpler-ops-macro
        'simulate-simpler-ops-macro)
       (undo-simpler-ops-macro
        'undo-simpler-ops-macro))
  "A static operation is an operation that, in an exhaustive search of the neighborhood, does not need to traverse a route.  Examples of these operations are the swap-something, and the reverse-subroute.  In this section we write a macro to create this kind of do-neighborhood operation macro such as doswap or doreverse-subroute.

         Actually, the only things that change in these macros from operation to operation, are the args, the comment, and how to instantiate the operation.

         I'm going to write a macro here that receives those 3 parameters and returns the corresponding macro.  It should be shomething like this:
         (make-do-neighborhood-static-macro
            (doswap-clients (client1 client2 working-copy)
            op-swap-clients (client1 client2)
             \"This is the comment for the doswap-clients macro\"))"

  `(defmacro ,name (,args &body body)
     ,doc-string

     (with-gensyms (op simpler-ops)
       ;; create the instance of the swap operation
       `(unless *vrp-stop-neighborhood-search*
                (let* ((,op (,',constructor-name ,,@constructor-args))
                       (,simpler-ops (get-simpler-operations-from
                                      ,op ,working-copy)))

                  ;; do the automatic stuff
                  ;; with the simpler-operations
                  ;; now the simulation
                  (,',simpler-ops-macro
                   ,simpler-ops ,working-copy)

                  ;; let's do what the user wants
                  (progn ,@body)

                  ;; now the clean up
                  (,',undo-simpler-ops-macro
                   ,simpler-ops
                   ,working-copy))))))

(defmacro doselect-route ((route-name
                           working-copy
                           &key gt ge lt le dt ex ex-cond)
                          &body body)
  "Iterates through each route in the working-copy passed as argument.  The symbol route-name is bound to the id of the current route.

     Example:
         (doselect-route (r1 wc1)
            (format t \"Route ~a is ~a\" r1 (get-route-with-id r1 wc1)))

    The optional paramaters `gt', `ge', `lt', `le', `dt' control which are the routes than should be iterated through.

       In the following example:
         (doselect-route (r1 wc1 :ge 2)
            (doselect-route (r2 wc1 :gt r1)
                (format t \"Route ~a is ~a\" r1 (get-route-with-id r1 wc1)))

       r1 will iterate through all the routes greater or equal to 2, and r2 will iterate through all the routes greater than r1."

  (declare (ignorable ex))
  (let* ((initial-route
          ;; let's find out what is the starting route
             (cond (ge ge)
                   (gt `(1+ ,gt))
                   (t 1)))
         (final-route
          ;; let's find out which should be the last route
             (cond (le le)
                   (lt `(1- ,lt))
                   (t `(number-of-routes ,working-copy))))
         (exclude
          ;; let's find out which should be the last route
             (if dt `(unless (= ,dt ,route-name) do)
                 ;; else
                 `(do))))
    `(loop for ,route-name from ,initial-route
           to ,final-route
           while (not *vrp-stop-neighborhood-search*)
           ,@exclude (unless ,ex-cond ,@body))))

(setf (macro-function 'doselect-route*) 
      (macro-function 'doselect-route))

(make-do-neighborhood-route-selection
              doselect-client
              (cname route working-copy)
              :op-name op-select-client
              :op-params (route pos cname)
              :example-str "(doselect-client (c1 r1 wc1)
              (format t \"Selecting client ~a from route ~a~%\"
               (client-selected-at-operation c1)
               (get-route-with-id r1 wc1)))")

(make-do-neighborhood-route-selection
   doselect-client*
   (cname route working-copy)
   :op-name op-select-client
   :op-params (route pos cname)
   :example-str "(doselect-client (c1 r1 wc1)
   (format t \"Selecting client ~a from route ~a~%\"
   (client-selected-at-operation c1)
   (get-route-with-id r1 wc1)))"
   :simpler-ops-macro simulate-simpler-ops-macro*
   :undo-simpler-ops-macro undo-simpler-ops-macro*)

(make-do-neighborhood-route-insertion
 doinsert-client
 (cname route working-copy)
 :op-name op-insert-client
 :op-params (route pos cname)
 :example-str "(doinsert-client (c1 r1 wc1)
                     (format t \"Inserting client ~a.~%\"
                      (client-selected-at-operation c1 wc1)))

                c1   is the index of the client that we want to insert
                     (actually it should be the selections-count corresponding
                     to its selection)
                r1  is the id of the route we want to make the insertion in
                wc1 is the working-copy where we are working.")

(make-do-neighborhood-route-insertion
   doinsert-client*
   (cname route working-copy)
 :op-name op-insert-client
 :op-params (route pos cname)
 :example-str "(doinsert-client (c1 r1 wc1)
   (format t \"Inserting client ~a.~%\"
      (client-selected-at-operation c1 wc1)))

      c1   is the index of the client that we want to insert
           (actually it should be the selections-count corresponding
           to its selection)
     r1  is the id of the route we want to make the insertion in
     wc1 is the working-copy where we are working."
   :simpler-ops-macro simulate-simpler-ops-macro*
   :undo-simpler-ops-macro undo-simpler-ops-macro*)
