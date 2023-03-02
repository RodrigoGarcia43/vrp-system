(in-package :vrp)

(def-vrp-class operation-swap-clients ()
  ;; slots
  (;; the ids of the clients that should be swapped
   (client1)
   (client2))
  ;; the rest of the class elements
  :documentation "A class to represent the swap-clients operation in a neighborhood criterion."
  :constructor (op-swap-clients (client1 client2))
  :print-object-string ("<op:s ~a ~a>" client1 client2)
  :slots-for-obj= (client1 client2)
  :slots-for-clone (client1 client2))

(defmethod  get-simpler-operations-from ((op operation-swap-clients)
                                         (wc basic-working-copy))
   "Here we return a list with the two insertions that should be made because of the swap operation."

   (let* ((c1 (client1 op))  ;; the selected-clients id of c1
          (c2 (client2 op)) ;; the selected-clients id of c2
          (c1-id (id (client-selected-at-operation c1 wc)))
          (c2-id (id (client-selected-at-operation c2 wc)))) 
     (symbol-macrolet
         ((c1-route (route-of-client-selected-at-operation c1 wc))
          (c1-pos (pos-of-client-selected-at-operation c1 wc))
          (c2-route (route-of-client-selected-at-operation c2 wc))
          (c2-pos (pos-of-client-selected-at-operation c2 wc)))

       (cond
         ((not (= c1-route c2-route)) ;; they were in different routes
          ;; so the orden doesn't matter.
          ;; Return the list in any order
          (list
           (op-insert-client c1-route c1-pos c2)
           (op-insert-client c2-route c2-pos c1)))
         ;; from here on they are on the same route
         ((< c1-pos c2-pos)
          ;; c1 was in a position before c2
          ;; so we need to insert first c2 were c1 was
          ;; and then c1 into 1+ from where c2 was
          ;; because they are in the same route, c1 was
          ;; selected first, and from a position smaller
          ;; than c2. When we insert c2 into c1's position
          ;; the 'original' position of c2 should be increased,
          ;; otherwise we get the insertions wrong
          (list
           (op-insert-client c1-route c1-pos c2)
           (op-insert-client c2-route (1+ c2-pos) c1)))
         ((< c2-pos c1-pos)
          ;; c2 was in a position before c1
          (list
           ;; so we need to insert first c1 were c2 was
           (op-insert-client c2-route c2-pos c1)
           ;; and now insert c1 into 1+ where c2 was.
           ;; We need to 1+ the position of c1 because now
           ;; we are "compiling" the position for the
           ;; second insertion and we need to take into
           ;; account that the first insertion will 1+ to the
           ;; position of all the clients after it.
           (op-insert-client c1-route (1+ c1-pos) c2)))
         ;; from here on they were on the same route and 
         ;; in the same position :-o
         ;; (they were one after the other)
         ((= c2-pos c1-pos)
          ;; We are assuming that there are not
          ;; selections after the first insertion is made,
          ;; so if we want to know which one should we insert first
          ;; we just need to check the new function
          ;; client-c1-was-before-client-c2
          ;; and insert first the one that was behind
          (if (client-c1-was-before-client-c2 c1-id c2-id wc) 
              (then ;; c1 was before c2 so

                (list
                 ;; first insert c2 into c1 position
                 (op-insert-client c1-route c1-pos c2)
                 ;; and then c1 into 1+ of c2's position
                 ;; because we are "compiling" the position
                 ;; of the second insertion, and the
                 ;; first one will add 1+ to all the positions
                 ;; after it, including the one we want to insert
                 (op-insert-client c2-route (1+ c2-pos) c1)))

              (else ;; c2 was before c1
                (list
                 ;; so we should first insert c1 into c2's pos
                 (op-insert-client c2-route c2-pos c1)
                 ;; and then insert c2 into 1+ c1's pos because
                 ;; of the explanation in the then's part
                 (op-insert-client c1-route (1+ c1-pos) c2)))))))))

(make-do-neighborhood-static-macro doswap-clients 
   (client1 client2 working-copy)
   op-swap-clients (client1 client2)
   "Pushes into ops-lists the insertions that should be made because of this swap, do whatever the users wants to do, and restores everything to the previous state.

    client1  is the index of the first client that we want to swap
        (it should be the selections-count corresponding
         to its selection, or in other words the index of the client
         in the (selected-clients working-copy).
    client2  is the index of the second client that we want to swap
        (it should be the selections-count corresponding
         to its selection, or in other words the index of the client
         in the (selected-clients working-copy).
    working-copy is the working-copy where we are working.
    Example:
        (doswap (c1 c2 wc1)
           (format t \"Swapping clients ~a and ~a~%\"
            (aref *selected-clients-array* c1)
            (aref *selected-clients-array* c2)))")

(make-do-neighborhood-static-macro
 doswap-clients*
 (client1 client2 working-copy)
 op-swap-clients (client1 client2)
 "Pushes into ops-lists the insertions that should be made because of this swap, do whatever the users wants to do, and restores everything to the previous state.

    client1  is the index of the first client that we want to swap
        (it should be the selections-count corresponding
         to its selection, or in other words the index of the client
         in the (selected-clients working-copy).
    client2  is the index of the second client that we want to swap
        (it should be the selections-count corresponding
         to its selection, or in other words the index of the client
         in the (selected-clients working-copy).
    working-copy is the working-copy where we are working.
    Example:
        (doswap* (c1 c2 wc1)
           (format t \"Swapping clients ~a and ~a~%\"
            (aref *selected-clients-array* c1)
            (aref *selected-clients-array* c2)))"
 :simpler-ops-macro simulate-simpler-ops-macro*
 :undo-simpler-ops-macro undo-simpler-ops-macro*)

;;The select-subroute class definition.
(def-vrp-class operation-select-subroute (neighborhood-operation)
  ((size :documentation "The size of the subroute that should be selected."))
  :documentation "A class to represent the select subroute operation in a neighborhood criterion"
  :constructor (op-select-subroute (route pos size operand))
  :print-object-string
  ("<op:ss ~a ~a ~a ~a>" route pos size operand)
  :slots-for-obj= (route pos size operand)
  :slots-for-clone (route pos size operand))

(defgeneric add-subroute-info (op working-copy)
  (:documentation "Adds the info of the selected-subroute to the working-copy."))

(defmethod add-subroute-info ((op operation-select-subroute)
                              (wc basic-working-copy))
  (let* ((id (operand op))
         (size (size op)))
    (push (make-array
           3 :initial-contents (list id size nil))
          (selected-subroutes wc))))

(defgeneric get-subroute-info (id working-copy)
  (:documentation "Returns the info of subroute with the given id.  This id is the selections-count of the first client in the subroute."))

(defmethod get-subroute-info (id (wc basic-working-copy))
  (let* ((result)
         (subroute-found nil))
    (loop for subr in (selected-subroutes wc)
          while (not subroute-found)
          do (if (= id (aref subr 0))
                 (progn
                   (setf result subr)
                   (setf subroute-found t))))
    ;; return result
    result))

(defgeneric get-subroute-size (id working-copy)
  (:documentation "Returns the size of the selected-subroute with the given id."))

(defmethod get-subroute-size (subroute-id
                              (working-copy basic-working-copy))
  (aif (get-subroute-info subroute-id working-copy)
       (aref it 1)))

(defgeneric subroute-reversed-p (id working-copy)
  (:documentation "Returns the reversed status of the given subroutev"))

(defmethod subroute-reversed-p (subroute-id
                                (working-copy basic-working-copy))
         (aif (get-subroute-info subroute-id working-copy)
              (aref it 2)))

(defgeneric mark-subroute-as-reversed (id working-copy)
  (:documentation "Toggle the reversed status of the given subroute."))

(defmethod mark-subroute-as-reversed
    (subroute-id (working-copy basic-working-copy))
  (let* ((subroute-info (get-subroute-info
                         subroute-id working-copy)))
    (setf (aref subroute-info 2)
          (not (aref subroute-info 2)))))

(defgeneric subroute-selected-at-operation (id working-copy)
  (:documentation "Returns the subroute whose first element was selected at the operation id."))

(defmethod subroute-selected-at-operation
    (id (wc basic-working-copy))
  (loop for i from 0 to (1- (get-subroute-size id wc))
        collecting (client-selected-at-operation (+ id i) wc)))

(defmethod simulate-neighborhood-operation
     ((op operation-select-subroute)
      (wc basic-working-copy))
   (add-subroute-info op wc))

(defmethod get-simpler-operations-from
    ((op operation-select-subroute)
     (wc basic-working-copy))
  "We return a list with all the client selections that will be made because of this select-subroute, and we also return the operation itself, because we need to add the subroute info and that is done in the simulate-neighborhood-operation for the select-subroute."
  (let* ((route (route op))
         (pos (pos op))
         (size (size op))
         (first-index (1- (operand op))))
    ;; append the op
    `(,op
      ;; with all the client selections
      ,@(loop for i from 1 to size
           collect (op-select-client
                    route pos (+ i first-index))))))

(defmethod undo-neighborhood-operation
     ((op operation-select-subroute)
      (wc basic-working-copy))
   (pop (selected-subroutes wc)))

(defmacro make-doselect-subroute-with-length
    (name
     &key
       (simpler-ops-macro
        'simulate-simpler-ops-macro)
       (undo-simpler-ops-macro
        'undo-simpler-ops-macro))
    `(defmacro ,name
         ((cname route length working-copy
                 &key ge gt le lt dt ex ex-cond)
          &body body)

       "Iterates through each position in the given route, creates an instance of op-select-subroute, pushes it int ops-lists, simulates it, do whatever the users wants to do, and finally restores everything to the previous state.
Example:
   (doselect-subroute-with-length (z1 r1 3 wc1)
                 (format t \"Selecting length 3 subroute stariting with
~a  form route ~a~%\"
                  (client-selected-at-operation c1)
                  (get-route-with-id r1 wc1)))"

       (with-gensyms (op
                      simpler-ops
                      initial-pos
                      final-pos
                      initial-target-route
                      final-target-route
                      exclude-target-route
                      exclude-pos
                      ;; last-pos
                      default-final-position
                      )

         ;; here we create the names of the
         ;; variables we create on the fly
         (let* ((cname.position (symb cname ".select.position"))
                (static-pos (symb cname ".position"))
                (subroute-length (symb cname ".subroute.length"))
                (cname.coord (symb cname ".select.coord"))
                (cname.route (symb cname ".select.route")))

           ;; here we assign the value to the variables
           ;; created on the fly
           ;; some on the symbol-macrolet and
           ;; and some others on the let
           `(symbol-macrolet ((,cname.position
                               (pos-of-client-selected-at-operation
                                ,cname ,working-copy))
                              (,cname.coord
                               (info-of-client-selected-at-operation
                                ,cname ,working-copy))
                              (,cname.route
                               (route-of-client-selected-at-operation
                                ,cname ,working-copy)))
              (let* ((,default-final-position
                      (1+ (- (route-length ,route ,working-copy) ,length)))
                     (,subroute-length ,length))

                (declare (ignorable ,subroute-length))

                (with-variables-for-constraints-for-subroute-selection
                    (,initial-pos
                     ,ge ,gt
                     ,final-pos ,le ,lt
                     ,initial-target-route
                     ,final-target-route
                     ,exclude-target-route
                     ,exclude-pos
                     ,dt
                     ,default-final-position)

                  (loop for ,static-pos from
                        ;; here we use the macro
                        ;; for the initialization of the values
                        (initial-for-value-in-all-macros
                         ,initial-target-route
                         ,route
                         ,initial-pos)

                        to (final-value-for-loop-in-route-operation-macro
                            ,final-target-route
                            ,route
                            ,final-pos
                            ,default-final-position)

                        ;; the next line allows the user to
                        ;; stop the search
                        while (not *vrp-stop-neighborhood-search*)


                        do ;; let's execute the body if
                        ;; we are not in a forbidden coord
                        (unless-forbidden-coord
                            (,ex
                             ,route
                             ,static-pos
                             ,dt
                             ,exclude-target-route
                             ,exclude-pos
                             ,ex-cond)
                          (let* (;; here we increase the index
                                 ;; of the operation
                                 (,cname
                                  (1+ (selections-count ,working-copy)))
                                 ;; we create the op and keep going
                                 ;; with the rest
                                 (,op (op-select-subroute
                                       ,route ,static-pos ,length ,cname))

                                 (,simpler-ops
                                  (get-simpler-operations-from
                                   ,op ,working-copy)))

                            ;; here I want to add some code to execute it
                            ;; before the simulation of the operation
                            ;; but I'm not yet sure about how
                            ;; to do it right :-/

                            ;; now the simulation
                            (,',simpler-ops-macro
                             ,simpler-ops ,working-copy)

                            ;; let's do what the user wants
                            (progn ,@body)

                            ;; now the clean up
                            (,',undo-simpler-ops-macro
                             ,simpler-ops
                             ,working-copy)))))))))))

(make-doselect-subroute-with-length
 doselect-subroute-with-length)

(defmacro make-variable-length-doselect-subroute
    (name &key
            (fixed-length-macro 'doselect-subroute-with-length))
  `(defmacro ,name
           ((cname route working-copy
                   &key ge gt le lt dt ex ex-cond ldt lex lex-cond)
            &body body)

         (with-gensyms (route-length)
           (let* ((current-length-name (symb cname ".length")))
             `(let ((,route-length (route-length ,route ,working-copy)))
                (loop for ,current-length-name from 1 to ,route-length
                      doing (unless (or
                                     (and
                                      (numberp ,ldt)
                                      (= ,current-length-name ,ldt))
                                     (and
                                      ,lex
                                      (member ,current-length-name ,lex)))
                              (unless-forbidden-length-code (,lex-cond)

                                (,',fixed-length-macro
                                    (,cname
                                     ,route
                                     ,current-length-name
                                     ,working-copy
                                     :ge ,ge :gt ,gt :le ,le
                                     :lt ,lt :dt ,dt :ex ,ex
                                     :ex-cond ,ex-cond)
                                  ,@body)))))))))

(make-variable-length-doselect-subroute
 doselect-subroute)

(make-doselect-subroute-with-length
 doselect-subroute-with-length*
 :simpler-ops-macro simulate-simpler-ops-macro*
 :undo-simpler-ops-macro undo-simpler-ops-macro*)

(make-variable-length-doselect-subroute
 doselect-subroute*
 :fixed-length-macro doselect-subroute-with-length*)

(defbehavior apply-neighborhood-operation
  ((operation operation-select-subroute)
   (wc basic-working-copy))
:log-str "(Not) Applying op select-subroute"

"Don't do anything.  This is here to avoid errors when applying a set of operations.  The actual changes will be made by the simpler-operations."
nil)

;;The insert-subroute class definition.
(def-vrp-class operation-insert-subroute (neighborhood-operation)
   ()
   :documentation "A class to represent the insert-subroute operation in a neighborhood criterion."
   :constructor (op-insert-subroute (route pos operand))
   :print-object-string ("<op:is ~a ~a ~a>" route pos operand)
   :slots-for-obj= (route pos operand)
   :slots-for-clone (route pos operand))

(defmethod get-simpler-operations-from
    ((op operation-insert-subroute)
     (wc basic-working-copy))
  "We return a list with all the insertions that will be made because of this insert-subroute operation. We need to check whether or not the subroute has been reversed."
  (let* ((route-for-insertion (route op))
         (pos-for-insertion (pos op))
         (subroute-id (operand op))
         ;; let's get the size of the subroute
         (subroute-size (get-subroute-size subroute-id wc)))


    (if (not (subroute-reversed-p subroute-id wc))
        (then
          (loop for i from 1 to subroute-size
                for current-id from subroute-id
                for current-position from pos-for-insertion

                collect (op-insert-client
                         route-for-insertion
                         current-position
                         current-id)))
        (else ;; insert in the opposite order
          ;; This can be achieved by inserting
          ;; in the same order but always in
          ;; the same position, because we
          ;; always insert before the previous
          ;; element
          (loop for i from 1 to subroute-size
                for current-insert-id from subroute-id
                collect (op-insert-client
                         route-for-insertion
                         pos-for-insertion
                         current-insert-id))))))

(defmacro make-doinsert-subroute
    (name
     &key
       (simpler-ops-macro
        'simulate-simpler-ops-macro)
       (undo-simpler-ops-macro
        'undo-simpler-ops-macro))
    `(defmacro ,name
         ((cname route working-copy
                 &key ge gt le lt dt ex ex-cond)
          &body body)
       "Iterates through all the positions in the given route, creates an instance of op-insert-subroute, pushes all its simpler-operations into ops-lists, simulates them, do whatever the users wants to do, and finally restores everything to the previous state.
Example:
   (doinsert-subroute (z1 r1 wc1)
     (format t \"Inserting subroute ~a.~%\"
        (client-selected-at-operation c1 wc1)))
          z1   is the index of the subroute that we want to insert
               (actually it should be the selections-count corresponding
               to its selection)
          r1  is the id of the route we want to make the insertion in.
          wc1 is the working-copy where we are working."
       (with-gensyms (op
                      simpler-ops
                      initial-pos
                      final-pos
                      initial-target-route
                      final-target-route
                      exclude-target-route
                      exclude-pos
                      last-pos)

         ;; here we create the names of the
         ;; variables we create on the fly
         (let* ((pos (symb cname ".ins.pos"))
                (cname.ins.pos (symb cname ".insert.position"))
                (cname.ins.coord (symb cname ".insert.coord"))
                (cname.ins.route (symb cname ".insert.route")))

           ;; here we assign values to cname.insert.route
           `(symbol-macrolet ((,cname.ins.pos
                               (pos-of-client-selected-at-operation
                                ,cname ,working-copy))
                              (,cname.ins.route
                               (get-inserted-client-route
                                ,cname ,working-copy))
                              (,cname.ins.coord
                               (get-inserted-client-info
                                ,cname ,working-copy)))
              ;; here we need to set the value of the last pos
              ;; to 1+ than the route-length because we can
              ;; insert at the end of the route
              (let* ((,last-pos (1+ (route-length ,route ,working-copy))))

                (with-variables-for-constraints-in-exhaustive-macro
                    (,initial-pos
                     ,ge ,gt
                     ,final-pos ,le ,lt
                     ,initial-target-route
                     ,final-target-route
                     ,exclude-target-route
                     ,exclude-pos
                     ,dt
                     ,last-pos)
                  (loop for ,pos from
                        ;; here we use the macro
                        ;; for the initialization of
                        ;; the values
                        (initial-for-value-in-all-macros
                         ,initial-target-route
                         ,route
                         ,initial-pos)
                        ;; here we also use the macro
                        ;; for the initialization of
                        ;; the macro
                        to (final-value-for-loop-in-route-operation-macro
                            ,final-target-route
                            ,route
                            ,final-pos
                            ,last-pos)
                        ;; the next line allows the user
                        ;; to stop the search
                        while (not *vrp-stop-neighborhood-search*)

                        do ;; let's execute the code if
                        ;; we are not in a forbidden coord
                        (unless-forbidden-coord
                            (,ex
                             ,route
                             ,pos
                             ,dt
                             ,exclude-target-route
                             ,exclude-pos
                             ,ex-cond)
                          (let* ( ;; we create the op and keep going
                                 ;; with the rest
                                 (,op (op-insert-subroute
                                       ,route ,pos ,cname))
                                 ;; we get the simpler ops
                                 (,simpler-ops
                                  (get-simpler-operations-from
                                   ,op ,working-copy)))

                            ;; here I want to add some code to execute it
                            ;; before the simulation of the operation
                            ;; but I'm not yet sure about how
                            ;; to do it right :-/

                            ;; now the simulation
                            (,',simpler-ops-macro
                             ,simpler-ops ,working-copy)

                            ;; let's do what the user wants
                            (progn ,@body)

                            ;; now the clean up
                            (,',undo-simpler-ops-macro
                             ,simpler-ops
                             ,working-copy)))))))))))

(make-doinsert-subroute doinsert-subroute)

(make-doinsert-subroute
 doinsert-subroute*
 :simpler-ops-macro simulate-simpler-ops-macro*
 :undo-simpler-ops-macro undo-simpler-ops-macro*)

;;The reverse-subroute class definition.
(def-vrp-class operation-reverse-subroute ()
  ((subroute-id
    :documentation "The id of the subroute that should be reversed."))
  ;; the rest of the class options
  :documentation "A class to represent the reverse-subroute operation in a neighborhood criterion."
  :constructor (op-reverse-subroute (subroute-id))
  :print-object-string ("<op:rs ~a>" subroute-id)
  :slots-for-obj= (subroute-id)
  :slots-for-clone (subroute-id))

(defmethod simulate-neighborhood-operation
    ((op operation-reverse-subroute)
      (wc basic-working-copy))
  (mark-subroute-as-reversed (subroute-id op) wc))

(defmethod undo-neighborhood-operation
    ((op operation-reverse-subroute)
      (wc basic-working-copy))
  (mark-subroute-as-reversed (subroute-id op) wc))

(make-do-neighborhood-static-macro
   doreverse-subroute  (subroute-id working-copy)
   op-reverse-subroute
   (subroute-id)
   "Reverses the given selected subroute. If the subroute has not been previously selected things will break.")

(defbehavior apply-neighborhood-operation
    ((operation operation-reverse-subroute)
     (wc basic-working-copy))
  :log-str "(Not) Applying op select-subroute"

"Don't do anything.  This is here to avoid errors when applying a set of operations.  The actual changes will be made by the simpler-operations."
nil)

(make-do-neighborhood-static-macro
  doreverse-subroute*
  (subroute-id working-copy)
  op-reverse-subroute
  (subroute-id)
  "Reverses the given selected subroute. If the subroute has not been previously selected things will break."
  :simpler-ops-macro simulate-simpler-ops-macro*
  :undo-simpler-ops-macro undo-simpler-ops-macro*)

;;The swap-subroutes class definition.
(def-vrp-class operation-swap-subroutes ()
  ((subroute1
    :documentation "The id of the first subroute that should be swapped.")
   (subroute2
    :documentation "The id of the second subroute that should be swapped."))

  :documentation "A class to represent the swap-subroute operation in a neighborhood criterion."
  :constructor (op-swap-subroutes (subroute1 subroute2))
  :print-object-string ("<op:sz ~a ~a>" subroute1 subroute2)
  :slots-for-obj= (subroute1 subroute2)
  :slots-for-clone (subroute1 subroute2))

(defmethod get-simpler-operations-from
    ((op operation-swap-subroutes)
     (wc basic-working-copy))
  "Returns all the insert-clients related to a swap subroutes operation."


  (let* ((s1 (subroute1 op))
         (s2 (subroute2 op))
         (s1-id (id (client-selected-at-operation s1 wc)))
         (s2-id (id (client-selected-at-operation s2 wc)))
         first-subroute-insertion
         second-subroute-insertion)

    (symbol-macrolet
        ((s1-route (route-of-client-selected-at-operation s1 wc))
         (s1-pos (pos-of-client-selected-at-operation s1 wc))
         (s2-route (route-of-client-selected-at-operation s2 wc))
         (s2-pos (pos-of-client-selected-at-operation s2 wc)))

      ;; first we create the two subroute-insertions
      ;; and then we'll create the insertions from them
      ;; we'll store the two subroute insertions in the
      ;; variables first-subroute-insertion and
      ;; second-subroute-insertion

      (cond
        ;; they are in different routes
        ((/= s1-route s2-route)
         ;; they are in different routes
         ;; so the order doesn't matter
         ;; return the list in any order
         ;; let's create the first insertion (s2 in s1 coordinates)
         (setf first-subroute-insertion
               (op-insert-subroute s2-route s2-pos s1))
         ;; and then the insertion (s1 in s2 coordinates)
         (setf second-subroute-insertion
               (op-insert-subroute s1-route s1-pos s2)))

        ;; from here on they are on the same route
        ((< s1-pos s2-pos) ;; s1 is clearly before s2

         ;; s1 was in a position before s2
         ;; so we need to insert first s2 were s1 was
         ;; and then, s1 where s2 was
         ;; but to do that, I'm going to add first s1 where s2 was
         ;; and then s2 in s1 position.
         ;; This way, the insertion of s1 does non affect the insertions
         ;; made by s2
         (setf first-subroute-insertion  (op-insert-subroute s2-route s2-pos s1))
         (setf second-subroute-insertion (op-insert-subroute s1-route s1-pos s2)))

        ((< s2-pos s1-pos) ;; s2 is evidently before s1 and no overlap

         ;; s2 was in a position before s1 so,
         ;; after the swap s1 should be before s2
         ;; To do that, I'm going to insert first s2 to where s1 was
         ;; and then s1 into s2 position.
         ;; This way, the insertions of s2 do not affect the insertions
         ;; made by s1
         (setf first-subroute-insertion
               (op-insert-subroute s1-route s1-pos s2))
         (setf second-subroute-insertion
               (op-insert-subroute s2-route s2-pos s1)))

        ;; from here on they are on the same route and same pos :-o
        ;; (they were right one after another or there was an overlap)
        ((= s1-pos s2-pos)
         ;; to know which one we should insert first,
         ;; we need to know which one was before the other
         ;; in the original solution.
         ;; we are assumming that there are not selections
         ;; after insertions, so we can use the fuction
         ;; client-c1-was-before-client-c2 to find out
         (if (client-c1-was-before-client-c2 s1-id s2-id wc)
             (then ;; s1 was before s2
               ;; so first insert s1 in the same place
               (setf first-subroute-insertion
                     (op-insert-subroute s1-route s1-pos s1))
               ;; and now insert s2 in the same place
               (setf second-subroute-insertion
                     (op-insert-subroute s2-route s2-pos s2)))
             (else ;; s2 was before s1
               ;; so first insert s2 in the same place
               (setf first-subroute-insertion
                     (op-insert-subroute s2-route s2-pos s2))
               ;; and now insert s1 in the same place
               (setf second-subroute-insertion
                     (op-insert-subroute s1-route s1-pos s1))))))
      ;; now create and return the list with all the insertions
      (append (get-simpler-operations-from
               first-subroute-insertion wc)
              (get-simpler-operations-from
               second-subroute-insertion wc)))))

(make-do-neighborhood-static-macro
   doswap-subroutes  (subroute1 subroute2 working-copy)
   op-swap-subroutes 
   (subroute1 subroute2)
   "Swaps the selected-subroutes passed as arguments.")

(make-do-neighborhood-static-macro
   doswap-subroutes*
   (subroute1 subroute2 working-copy)
   op-swap-subroutes 
   (subroute1 subroute2)
   "Swaps the selected-subroutes passed as arguments."
   :simpler-ops-macro simulate-simpler-ops-macro*
   :undo-simpler-ops-macro undo-simpler-ops-macro*)

(def-vrp-class operation-add-route ()
  ;; slots
  (;; the id of the client that should be inserted in the new route
   (client))
  ;; the rest of the class elements
  :documentation "A class to represent the operation add-route in a neighborhood criterion."
  :constructor (op-add-route (client))
  :print-object-string ("<op:add-r ~a>" client)
  :slots-for-obj= (client)
  :slots-for-clone (client))

(defmethod simulate-neighborhood-operation
    ((op operation-add-route)
      (wc basic-working-copy))
  (incf (number-of-routes wc))
  ;; we need to set the length of the new route
  ;; to 0 because right after simulating this op
  ;; we'll simulate an insertion
  ;; and that insertion adds 1+ to that route-length.
  (vector-push-extend 0 (routes-lengths wc)))

(defmethod get-simpler-operations-from
    ((op operation-add-route)
     (wc basic-working-copy))
  "We return a list with the actual add-route operation (to update the routes-lengths) and the client insertion."
  (let* (;; we need to add in the route 1+ number-of-routes
         ;; because when we create this insertion
         ;; we haven't simulated the add-route yet
         ;; and so, the number-of-routes is the
         ;; actual number of routes.
         (route (1+ (number-of-routes wc)))
         (pos 1)
         (client (client op)))


    ;; return a list
    (list
     ;; with the add-route operation
     op
     ;; and the insertion
     (op-insert-client route pos client))))

(defmethod undo-neighborhood-operation
    ((op operation-add-route)
     (wc basic-working-copy))
  "We undo the effects of adding a new route."
  ;; decrease the number-of-routes
  (decf (number-of-routes wc))
  ;; and pop the routes-lengths vector
  (vector-pop (routes-lengths wc)))

(make-do-neighborhood-static-macro
   doadd-route  (client-id working-copy)
   op-add-route
   (client-id)
   "Adds a new route by adding to it the client selected at operation client-id.")

(defbehavior apply-neighborhood-operation
    ((operation operation-add-route)
     (wc basic-working-copy))
  :log-str "Creating a new empty route"

  "Creates a new empty route with the appropriate id."
  (if (null (routes (solution wc)))
      (error "Trying to add a new route to an empty solution :-(, check apply-neighborhood-operation for add-route and basic-working-copy."))

  (let* ((solution (solution wc))
         (routes (routes solution))
         (max-route-id (loop for route in routes
                             maximizing (id route)))
         (vehicle (vehicle (first routes)))
         (depot (depot (first routes)))
         (new-route (route-for-simulation
                     :id (1+ max-route-id)
                     :vehicle vehicle
                     :depot depot
                     :clients nil)))
    ;; now let's add the new route
    (setf (routes solution)
          (append (routes solution)
                  (list new-route)))))

(make-do-neighborhood-static-macro
   doadd-route*
   (client-id working-copy)
   op-add-route
   (client-id)
   "Adds a new route by adding to it the client selected at operation client-id."
   :simpler-ops-macro simulate-simpler-ops-macro*
   :undo-simpler-ops-macro undo-simpler-ops-macro*)

(defmacro random-select-route ((route-name
                                working-copy 
                                &key gt ge lt le dt ex ex-cond)
                               &body body)
  "Selects a random route from the working-copy passed as argument.  The symbol route-name is bound to the id of the current route.
     Example:
         (random-select-route (r1 wc1)
            (format t \"Route ~a is ~a\"
                    r1 (get-route-with-id r1 wc1)))"
  (declare (ignorable gt ge lt le dt ex ex-cond))
  (with-gensyms (g!length)
  `(when (not *vrp-stop-neighborhood-search*)
     (let* ((,g!length (number-of-routes ,working-copy))
            (,route-name (1+ (random ,g!length))))

       (progn
         ,@body)))))

(defmacro random-select-client
    ((cname route working-copy
      &key gt ge lt le dt ex ex-cond)
     &body body)
  "Selects a random route from the working-copy passed as argument.  The symbol route is bound to the id of the current route.
     Example:
         (random-select-client (c1 r1 wc1)
           (format t \"Selecting client ~a from route ~a~%\"
               (client-selected-at-operation c1)
               (get-route-with-id r1 wc1)))"
  (declare (ignorable gt ge lt le dt ex ex-cond))
  (with-gensyms (g!length op simpler-ops)

    (let* ((pos (symb cname ".select.position"))
           (cname.route (symb cname ".route"))
           (cname.client (symb cname ".client"))
           (cname.position (symb cname ".select.position"))
           (cname.coord (symb cname ".select.coord"))
           (cname.prev.coord (symb cname ".prev.insert.coord")))

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
        (when (and (not *vrp-stop-neighborhood-search*)
                   (> (route-length ,route ,working-copy) 0))
          (let* ((,g!length (route-length ,route ,working-copy))
                 (,pos (1+ (random ,g!length)))
                 (,cname (1+ (selections-count ,working-copy)))
                 (,op (op-select-client ,route ,pos ,cname))
                 (,simpler-ops (get-simpler-operations-from
                                ,op ,working-copy))
                 )
            ;; the automatic-stuff
            (simulate-simpler-ops-macro ,simpler-ops ,working-copy)
            ;; the user's code
            (progn
              ,@body)
            ;; let's clean up
            (undo-simpler-ops-macro ,simpler-ops ,working-copy)))))))

(defmacro random-insert-client
    ((cname route-name working-copy
      &key gt ge lt le dt ex ex-cond)
     &body body)
  "Inserts a random client in the given route in the working-copy passed as argument.
     Example:
         (random-insert-client (c1 r1 wc1)
           (format t \"Inserting client ~a in position ~a~%\"
               (client-selected-at-operation c1)
               c1.insert.pos))"
  (declare (ignorable gt ge lt le dt ex ex-cond))

  (with-gensyms (g!length op simpler-ops)

    (let* ((pos (symb cname ".insert.position"))
           (cname.ins.route (symb cname ".insert.route"))
           (cname.ins.position (symb cname ".insert.position")))

      `(symbol-macrolet ((,cname.ins.position
                                 (get-inserted-client-pos
                                  ,cname ,working-copy))
                         (,cname.ins.route
                                 (get-inserted-client-route
                                  ,cname ,working-copy)))

         (when (not *vrp-stop-neighborhood-search*)

           (let* ((,g!length (route-length ,route-name ,working-copy))
                 ;; we have two +1 because we can insert
                 ;; beyond the last client in the route
                 ;; and that would the position:
                 ;; (+ 2 route-length)
                 (,pos (1+ (random (1+ ,g!length))))
                 (,op (op-insert-client ,route-name ,pos ,cname))
                 (,simpler-ops (get-simpler-operations-from
                                ,op ,working-copy))
                 )
            ;; the automatic-stuff
            (simulate-simpler-ops-macro ,simpler-ops ,working-copy)
            ;; the user's code
            (progn
              ,@body)
            ;; let's clean up
            (undo-simpler-ops-macro ,simpler-ops ,working-copy)))))))

(setf (macro-function 'random-swap-clients) 
       (macro-function 'doswap-clients))

(defmacro random-select-subroute-with-length
    ((cname route-name length working-copy
            &key ge gt le lt dt ex ex-cond)
     &body body)

  "Iterates through each position in the given route, creates an instance of op-select-subroute, pushes it int ops-lists, simulates it, do whatever the users wants to do, and finally restores everything to the previous state.
Example:
   (doselect-subroute-with-length (z1 r1 3 wc1)
                 (format t \"Selecting length 3 subroute stariting with
~a  form route ~a~%\"
                  (client-selected-at-operation c1)
                  (get-route-with-id r1 wc1)))"

  (declare (ignorable ge gt le lt dt ex ex-cond))

  (with-gensyms (g!length op simpler-ops last-pos)
    (let* ((pos (symb cname ".select.position"))
           (subroute-length (symb cname ".subroute.length")))

    `(when (and (not *vrp-stop-neighborhood-search*)
                (> (route-length ,route-name ,working-copy)
                   ,length))
       (let* ((,g!length (route-length ,route-name ,working-copy))
              (,last-pos ;; the last pos is the length
               ;; minus the subroute length.
               (1+ (- ,g!length ,length)))
              (,pos (1+ (random ,last-pos)))
              (,cname (1+ (selections-count ,working-copy)))
              (,op (op-select-subroute
                    ,route-name ,pos ,length ,cname))
              (,simpler-ops (get-simpler-operations-from
                             ,op ,working-copy))
              (,subroute-length ,length))

         (declare (ignorable ,subroute-length))

         ;; the automatic-stuff
         (simulate-simpler-ops-macro ,simpler-ops ,working-copy)
         ;; the user's code
         (progn
           ,@body)
         ;; let's clean up
         (undo-simpler-ops-macro ,simpler-ops ,working-copy))))))

(defmacro random-select-subroute
    ((cname route-name working-copy
            &key ge gt le lt dt ex ex-cond ldt lex lex-cond)
     &body body)

  (declare (ignorable ge gt le lt dt ex ex-cond ldt lex lex-cond))


  (with-gensyms (current-length route-length)

    `(let* ((,route-length (route-length ,route-name ,working-copy))
            (,current-length (+ 2 (random (1- ,route-length)))))

       ;; the following conditional is to avoid
       ;; getting subroutes from routes without enough
       ;; elements
       (when (> ,route-length 1)
         (random-select-subroute-with-length
             (,cname ,route-name ,current-length ,working-copy)
           ,@body)))))

(defmacro random-insert-subroute
    ((cname route-name working-copy
      &key ge gt le lt dt ex ex-cond)
     &body body)
  "Selects a random position from all the possitions in the given route and inserts the subroute at that postion.
    Example:
       (random-insert-subroute (z1 r1 wc1)
         (format t \"Inserting subroute ~a at pos ~a.~%\"
            (client-selected-at-operation c1 wc1)
            z1.insert.position))
              z1   is the index of the subroute that we want to insert
                   (actually it should be the selections-count corresponding
                   to its selection)
              r1  is the id of the route we want to make the insertion in.
              wc1 is the working-copy where we are working."

  (declare (ignorable ge gt le lt dt ex ex-cond))

  (with-gensyms (g!length op simpler-ops)
    (let* ((pos (symb cname ".insert.position")))

      `(when (not *vrp-stop-neighborhood-search*)
         (let* ((,g!length (route-length ,route-name ,working-copy))
                ;; we have two +1 because we can insert
                ;; beyond the last client in the route.
                (,pos (1+ (random (1+ ,g!length))))
                (,op (op-insert-subroute ,route-name ,pos ,cname))
                (,simpler-ops (get-simpler-operations-from
                               ,op ,working-copy)))


           ;; the automatic-stuff
           (simulate-simpler-ops-macro ,simpler-ops ,working-copy)
           ;; the user's code
           (progn
             ,@body)
           ;; let's clean up
           (undo-simpler-ops-macro ,simpler-ops ,working-copy))))))

(setf (macro-function 'random-reverse-subroute) 
      (macro-function 'doreverse-subroute))

(setf (macro-function 'random-swap-subroutes) 
      (macro-function 'doswap-subroutes))

(setf (macro-function 'random-add-route) 
      (macro-function 'doadd-route))
