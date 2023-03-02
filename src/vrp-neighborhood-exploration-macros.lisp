(in-package :vrp)

(defmacro simulate-simpler-ops-macro (simpler-ops working-copy)
  (with-gensyms (simple-op)
    `(progn
       ;; here we process all the operations
       ;; in simpler-ops
       (dolist (,simple-op ,simpler-ops)
         ;; push the current op into ops-list
         (push ,simple-op ops-list)
         ;; simulate the operation
         (simulate-neighborhood-operation
          ,simple-op ,working-copy)))))

(defmacro undo-simpler-ops-macro (simpler-ops working-copy)
  (with-gensyms (simple-op)
    `(progn
       ;; here we undo all the operations in
       ;; simpler-ops:
       (dolist (,simple-op (reverse ,simpler-ops))
         ;; pop the op from the list of operations
         (pop ops-list)
         ;; undo the operation
         (undo-neighborhood-operation
          ,simple-op ,working-copy)))))

(defmacro simulate-simpler-ops-macro* (simpler-ops working-copy)
  (with-gensyms (simple-op)
    `(progn
       ;; here we process all the operations
       ;; in simpler-ops
       (dolist (,simple-op ,simpler-ops)
         ;; push the current op into ops-list
         (push ,simple-op ops-list)
         ;; compute the delta cost after
         (compute-delta-cost-after ,simple-op ,working-copy problem action)
         ;; simulate the operation
         (simulate-neighborhood-operation
          ,simple-op ,working-copy)))))

(defmacro undo-simpler-ops-macro* (simpler-ops working-copy)
  (with-gensyms (simple-op)
    `(progn
       ;; here we undo all the operations in
       ;; simpler-ops:
       (dolist (,simple-op (reverse ,simpler-ops))
         ;; pop the op from the list of operations
         (pop ops-list)
         ;; undo the operation
         (undo-neighborhood-operation
          ,simple-op ,working-copy)
         ;; let's undo the delta-cost computation
         (undo-delta-cost-computation ,simple-op ,working-copy problem action)))))

(defmacro with-variables-for-constraints-for-selection
    ((initial-pos ge gt
     final-pos le lt
     route working-copy
     initial-target-route
     final-target-route
     exclude-target-route
     exclude-pos
     dt)
     &body body)
  `(let* ((,initial-pos
           ;; let's compute the starting pos
              (max 1 (cond (,ge
                            (if (and (listp ,ge))
                                (second ,ge)
                                ,ge))
                           (,gt
                            (if (listp ,gt)
                                (1+ (second ,gt))
                                (1+ ,gt)))
                           (t 1))))
          (,final-pos
           ;; let's compute the last pos
              (cond (,le
                     (if (and (listp ,le))
                         (second ,le)
                         ,le))
                    (,lt
                     (if (listp ,lt)
                         (1- (second ,lt))
                         (1- ,lt)))
                    ;; we need to add 1+ because we can
                    ;; add at the end of the route
                    (t (route-length ,route ,working-copy))))
          (,initial-target-route
           (cond
             (,ge (if (listp ,ge) (first ,ge)))
             (,gt (if (listp ,gt) (first ,gt)))))
          (,final-target-route
           (cond
             (,le (if (listp ,le) (first ,le)))
             (,lt (if (listp ,lt) (first ,lt)))))
          (,exclude-target-route
           (cond
             (,dt (if (listp ,dt) (first ,dt)))))
          (,exclude-pos
           ;; let's check if we should exclude something
           (cond (,dt
                  (if (and (listp ,dt))
                      (second ,dt)
                      ,dt)))))
     ,@body))

(defmacro with-variables-for-constraints-for-insertion
    ((initial-pos ge gt
     final-pos le lt
     route working-copy
     initial-target-route
     final-target-route
     exclude-target-route
     exclude-pos
     dt)
     &body body)
  `(let* ((,initial-pos
           ;; let's compute the starting pos
              (max 1 (cond (,ge
                            (if (and (listp ,ge))
                                (second ,ge)
                                ,ge))
                           (,gt
                            (if (listp ,gt)
                                (1+ (second ,gt))
                                (1+ ,gt)))
                           (t 1))))
          (,final-pos
           ;; let's compute the last pos
              (cond (,le
                     (if (and (listp ,le))
                         (second ,le)
                         ,le))
                    (,lt
                     (if (listp ,lt)
                         (1- (second ,lt))
                         (1- ,lt)))
                    ;; we need to add 1+ because we can
                    ;; add at the end of the route
                    (t (1+ (route-length ,route ,working-copy)))))
          (,initial-target-route
           (cond
             (,ge (if (listp ,ge) (first ,ge)))
             (,gt (if (listp ,gt) (first ,gt)))))
          (,final-target-route
           (cond
             (,le (if (listp ,le) (first ,le)))
             (,lt (if (listp ,lt) (first ,lt)))))
          (,exclude-target-route
           (cond
             (,dt (if (listp ,dt) (first ,dt)))))
          (,exclude-pos
           ;; let's check if we should exclude something
           (cond (,dt
                  (if (and (listp ,dt))
                      (second ,dt)
                      ,dt)))))
     ,@body))

(defmacro with-variables-for-constraints-for-subroute-selection
    ((initial-pos
      ge gt
      final-pos le lt
      initial-target-route
      final-target-route
      exclude-target-route
      exclude-pos
      dt
      last-pos)
     &body body)
  `(let* ((,initial-pos
           ;; let's compute the starting pos
              (max 1 (cond (,ge
                            (if (and (listp ,ge))
                                (second ,ge)
                                ,ge))
                           (,gt
                            (if (listp ,gt)
                                (1+ (second ,gt))
                                (1+ ,gt)))
                           (t 1))))
          (,final-pos
           ;; let's compute the last pos
              (cond (,le
                     (if (and (listp ,le))
                         (second ,le)
                         ,le))
                    (,lt
                     (if (listp ,lt)
                         (1- (second ,lt))
                         (1- ,lt)))
                    ;; we need to add 1+ because we can
                    ;; add at the end of the route
                    (t ,last-pos)))
          (,initial-target-route
           (cond
             (,ge (if (listp ,ge) (first ,ge)))
             (,gt (if (listp ,gt) (first ,gt)))))
          (,final-target-route
           (cond
             (,le (if (listp ,le) (first ,le)))
             (,lt (if (listp ,lt) (first ,lt)))))
          (,exclude-target-route
           (cond
             (,dt (if (listp ,dt) (first ,dt)))))
          (,exclude-pos
           ;; let's check if we should exclude something
           (cond (,dt
                  (if (and (listp ,dt))
                      (second ,dt)
                      ,dt)))))
     ,@body))

(defmacro with-variables-for-constraints-in-exhaustive-macro
    ((initial-pos
      ge gt
      final-pos le lt
      initial-target-route
      final-target-route
      exclude-target-route
      exclude-pos
      dt
      last-pos)
     &body body)
  `(let* ((,initial-pos
           ;; let's compute the starting pos
              (max 1 (cond (,ge
                            (if (and (listp ,ge))
                                (second ,ge)
                                ,ge))
                           (,gt
                            (if (listp ,gt)
                                (1+ (second ,gt))
                                (1+ ,gt)))
                           (t 1))))
          (,final-pos
           ;; let's compute the last pos
              (cond (,le
                     (if (and (listp ,le))
                         (second ,le)
                         ,le))
                    (,lt
                     (if (listp ,lt)
                         (1- (second ,lt))
                         (1- ,lt)))
                    ;; we need to add 1+ because we can
                    ;; add at the end of the route
                    (t ,last-pos)))
          (,initial-target-route
           (cond
             (,ge (if (listp ,ge) (first ,ge)))
             (,gt (if (listp ,gt) (first ,gt)))))
          (,final-target-route
           (cond
             (,le (if (listp ,le) (first ,le)))
             (,lt (if (listp ,lt) (first ,lt)))))
          (,exclude-target-route
           (cond
             (,dt (if (listp ,dt) (first ,dt)))))
          (,exclude-pos
           ;; let's check if we should exclude something
           (cond (,dt
                  (if (and (listp ,dt))
                      (second ,dt)
                      ,dt)))))
     ,@body))

(defmacro unless-forbidden-coord
    ((ex
      route
      pos
      dt
      exclude-target-route
      exclude-pos
      &optional ex-cond)
     &body body)

  `(unless ,ex-cond
       (unless
           (or
            (and ,dt ;; there is a different from
                 ;; argument, and there is
                 ;; an exclude-target-route
                 ,exclude-target-route
                 ;; and we are in the
                 ;; forbidden route
                 (= ,exclude-target-route ,route)
                 ;; and we are in the
                 ;; forbidden pos
                 (= ,exclude-pos ,pos)
                 ;; then we should not
                 ;; do this
                 )
            (and ,dt ;; there is a :dt arg
                 ;; and there is not exclude-target-route
                 (null ,exclude-target-route)
                 ;; if we are in the :dt pos
                 (= ,exclude-pos ,pos)
                 ;; we should do nothing
                 )
            ;; ;; we also need to check that the current
            ;; ;; coord is not in the list of
            ;; ;; excluded coords [ex]
            (member (list ,route ,pos)
                    ,ex
                    :test(lambda (x y)
                           (and
                            (= (first x) (first y))
                            (= (second x) (second y)))))
            ) 
         ,@body)))

(defmacro unless-forbidden-length-code
    ((lex-cond)
     &body body)

  `(unless ,lex-cond
     ,@body))

(defmacro unless-forbidden-condition
    ((ex-cond)
     &body body)

  `(unless ,ex-cond
     ,@body))

(defmacro initial-for-value-in-all-macros
    (initial-target-route
     route
     initial-pos)

  (declare (special *vrp-max-route-length*))

  `(if ,initial-target-route
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
         ,initial-pos)))

(defmacro initial-for-value-in-insertion-macro
    (initial-target-route
     route
     initial-pos)

  (declare (special *vrp-max-route-length*))

  `(if ,initial-target-route
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
         ,initial-pos)))

(defmacro initial-for-value-in-insertion-macro
    (initial-target-route
     route
     initial-pos)

  (declare (special *vrp-max-route-length*))

  `(if ,initial-target-route
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
         ,initial-pos)))

(defmacro final-value-for-loop-in-route-operation-macro
    (final-target-route
     route
     final-pos
     default-final-position)
  "This macro writes the code to compute the last position in a route for a do-neighborhood macro (it could be for insertion, selection, or another operation that iterates through the route). We can use it for several operations because of the parameter default-final-position, that is the only thing that changes from one operation to another."

  `(if ,final-target-route
       ;; if there was a final route
       ;; as a target
       (then ;; select the appropriate
         ;; final-pos
         (cond
           ;; let's check if we are
           ;; in the same route as the
           ;; target-route
           ((= ,final-target-route ,route)
            ;; if we are, return the final-pos
            ;; unless it is beyond the default
            ;; final-position
            ;; (that's why we take the min)
            (min ,final-pos ,default-final-position))
           ;; if we are in a route
           ;; after the target-route
           ((> ,route ,final-target-route)
            ;; then don't iterate
            ;; through it
            ;; so set the final pos to
            -1)
           (t ;; this is the case where
            ;; we are in a route before
            ;; the final-target-route
            ;; so, we can iterate through
            ;; all the route, up to the
            ;; default-final-position
            ,default-final-position)))
       (else ;; there was not a final-target-route
         ;; so we have to impose the restrictions
         ;; in all the routes :-/
         (min ,final-pos ,default-final-position))))
