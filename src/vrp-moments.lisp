(in-package :vrp)

;;;{{{

(defparameter *vrp-logging* 0
  "This variable indicates if we are logging the operations or not.
0 means not to log.  The greater the number, the greater the amount of information that will be logged.")

(defparameter *vrp-logging-stream* t
  "The stream where all the logging information will be written.")

(defun vrp-log (method-name log-str args stream)
  "This functions log the vrp info to the given stream."
  (let* ((name (first method-name))
         (modifier (second method-name))
         (args-types (mapcar (lambda (x) (if (listp x)
                                             (second x)
                                             t))
                             args))
         (args-values (mapcar (lambda (x) (if (listp x)
                                              (first x)
                                              x)) args))
         ;; the logging functions
         ;; level 1
         (level1-string (format nil
              "VRPLOG: ~a (~a):  ~a"
              name
              (if modifier modifier :primary)
              log-str))
         ;; level 2
         (level2-string (format nil
              ", with instances of:~%~{     ~a~%~}~%"
              args-types))
         ;; level 3
         (level3-string (format nil
              ", with args:~%~{     ~a~%~}~%"
              args-values))

         ;; final string
         (final-string (concatenate
                        'string
                        level1-string
                        (cond ((= *vrp-logging* 1) (format nil ".~%"))
                              ((= *vrp-logging* 2) level2-string)
                              ((= *vrp-logging* 3) level3-string)))))
    ;; here we add the logging behavior
    (if (> *vrp-logging* 0)
        (format stream "~a" final-string))))

;;;}}}

(defvar *add-calls-to-vrp-log-in-defbehavior* nil
  "A variable that controls whether or not to add calls to the function vrp-log in defbehavior.  It defaults to nil.")

(defmacro defbehavior (moment &rest args)
  (let* (;; let's check if there is a modifier
         ;; if there is, the first element of args
         ;; should be a symbol
         (modifier (if (symbolp (car args)) (car args)))
         ;; let's gather the rest of the arguments
         ;; if there is a modifier, get the rest of the args
         (other-args (if (symbolp (car args)) (rest args)
                                  ;; else get all the args
                                  (else args))))
    ;; now let's see what's in the other-args
    (destructuring-bind ((&rest arg-definitions) &body body)
        other-args
      ;; let's prepare the method arguments
      (let* (;; first let's replace any undefined class-name with t
             (args-and-classes (mapcar
                                (lambda (x)
                                  (if (listp
                                       x)
                                      x
                                       (else
                                        (list x t))))
                                arg-definitions))
             ;; now let's create the method name
             (method-name-and-modifier
              (if modifier `(,moment ,modifier) `(,moment))))
        ;; let's get the log-str from the body
        (with-elements (((log-str :log-str)) body)
          ;; let's create the call to vrp-log
          (let* ((call-to-vrp-log
                  `(vrp-log
                    ;; the first parameter is the name
                    ;; and the modifier
                    ',method-name-and-modifier
                    ;; the second parameter should
                    ;; be string
                    ,log-str
                    ;; now we add all the
                    ;; arguments and it classes
                    ,(append '(list)
                             (loop for (var class) in args-and-classes
                                   collecting `(list ,var ',class)))
                    ;; finally we need to add the stream to
                    ;; write the info to. Right now it will be the
                    ;; variable *vrp-logging-stream*
                    *vrp-logging-stream*))
                 ;; here is where we decide if we
                 ;; add the call to vrp-log or not
                 (actual-method-code
                  (if *add-calls-to-vrp-log-in-defbehavior*
                      `(,call-to-vrp-log
                        ,@body)
                      ;; else
                      body)))
          ;; let's create the method name and modifier (if any)
           `(defmethod ,@method-name-and-modifier
                ;; now let's add the method signature
                ,args-and-classes

              ;; let's add the logging functionality


              ;; TODO: add the abbrevs

              ;; add the body
              ,@actual-method-code)))))))

(defgeneric simulate-solution (solution problem action)
  (:documentation "A generic function to compute the cost of a solution."))

(defgeneric finish-the-solution-simulation (solution problem action)
  (:documentation "A generic function to be called after all the routes in the solution have been simulate."))

(defgeneric simulate-route (route solution problem action)
  (:documentation "A generic function to compute the cost of a route."))

(defgeneric when-route-begins (vehicle route solution problem action)
  (:documentation "A generic function to compute the cost of a route."))

(defgeneric move-from-to (vehicle
                          client1
                          client2
                          route
                          solution
                          problem
                          action)
  (:documentation "A generic function to be called when the vehicle moves from one client to another. Here client is anything that can be visited."))

(defgeneric visit-client (vehicle client route solution problem action)
  (:documentation "A generic function to describe what happens when a vehicle visits a client in a given route."))

(defgeneric when-route-ends (route solution problem action)
  (:documentation "A generic function to describe what happens after all the clients have been visited."))

(defgeneric unload (vehicle client route solution problem action)
  (:documentation "A generic function to unload the cargo from the vehicle."))

(defbehavior unload ((vehicle  unconditionally-unload-vehicle)
                     (client   demand-client)
                     (route    t)
                     (solution t)
                     (problem  t)
                     (action simulate-load-action))
  :log-str "Downloading the client's demand from the vehicle" 
  (decf (cargo vehicle) (demand client)))

(defbehavior visit-client ((vehicle  t)
                           (client   t)
                           (route    t)
                           (solution t)
                           (problem  t)
                           (action   t))
  :log-str "Do nothing.  Wait for the :after methods")

(defbehavior visit-client :after ((vehicle  t)
                                  (client   t)
                                  (route    route-for-simulation)
                                  (solution t)
                                  (problem  t)
                                  (action   t))
  :log-str "Set the previous client"
  (setf (previous-client route) client))

(defbehavior visit-client :after ((vehicle unload-vehicle)
                                  (client demand-client)
                                  (route t)
                                  (solution t)
                                  (problem t)
                                  (action simulate-load-action))
        :log-str "Dowloading demand"
        (unload vehicle client route solution problem action))

(defbehavior when-route-begins (vehicle
                                (route route-for-simulation)
                                solution
                                problem
                                action)
  :log-str "Setting the depot as the previous-client"
  (setf (previous-client route) (depot route)))

(defbehavior when-route-begins :after
  (vehicle
   route
   solution
   problem
   (action route-distance-action))
  :log-str "Initializing current-distance to 0"
  (setf (current-distance action) 0))

(defbehavior when-route-begins :after
  ((vehicle cargo-vehicle)
   route
   solution
   problem
   (action simulate-load-action))
  :log-str "Loading the vehicle"

  ;; load the vehicle with all the demands in th route
  (setf (cargo vehicle) (loop for c in (clients route)
                      summing (demand c))))

(defbehavior when-route-begins :after
  (vehicle
   route
   solution
   problem
   (action basic-vehicle-capacity-action))

  :log-str "Checking capacity violation"

  (let* ((capacity (capacity (vehicle route)))
         (cargo (loop for c in (clients route)
                      summing (demand c))))
    (incf (capacity-violation action)
          (max 0 (- cargo capacity)))))

(defbehavior move-from-to ((vehicle     t)
                           (from-client t)
                           (to-client   t)
                           (route       t)
                           (solution    t)
                           (problem     t)
                           (action      t))
  :log-str "Do nothing: wait for the auxiliary methods"
  nil)

(defbehavior move-from-to :after
  ((vehicle     t)
   (from-client t)
   (to-client   t)
   (route       t)
   (solution    t)
   (problem     distance-problem)
   (action      route-distance-action))

  :log-str "Updating distance"
  (incf (current-distance action)
        (get-distance-from-to from-client to-client problem)))

(defbehavior when-route-ends (route
                              solution
                              problem
                              action)
  :log-str "Do nothing, wait for the auxiliary methods"
  nil)

(defbehavior when-route-ends :after ((route route-for-simulation)
                                       solution
                                       problem
                                       action)
  :log-str "Moving the vehicle to the depot (if there are clients)"
  (if (clients route)
      (move-from-to (vehicle route)
                    (previous-client route)
                    (end-depot route)
                    route
                    solution
                    problem
                    action)))

(defbehavior simulate-route ((route    route-for-simulation)
                             (solution t)
                             (problem  t)
                             (action   t))
  :log-str "Basic simulation of route"

  ;; do whatever happens at the beginning of the route
  (when-route-begins (vehicle route) route solution problem action)

  ;; move to each client and visit them
  (loop for client in (clients route)
        ;; move the vehicle
        doing (move-from-to (vehicle route)
                            (previous-client route)
                            client
                            route
                            solution
                            problem
                            action)
        ;; visit each client
        doing (visit-client (vehicle route)
                            client
                            route
                            solution
                            problem
                            action))
  ;; finish the route
  (when-route-ends route solution problem action))

(defbehavior simulate-route :after
  ((route    t)
   (solution t)
   (problem  t)
   (action   basic-solution-distance-action))

  :log-str "Updating the total-distance in the solution"

  (incf (total-distance action) (current-distance action)))

(defbehavior finish-the-solution-simulation ((solution t)
                                             (problem  t)
                                             (action   t))

  :log-str "Do nothing. Wait for the auxiliary methods"
  nil)

(defbehavior finish-the-solution-simulation :after
    ((solution t)
     (problem  t)
     (action   basic-capacity-penalty-action))

  :log-str "Penalizing capacity violations"

  (setf (total-penalty action)
        (* (penalty-factor action)
           (capacity-violation action))))

(defbehavior simulate-solution ((solution basic-solution)
                                (problem  t)
                                (action   t))

  :log-str "Basic simulation of the solution"

  ;; just simulate each of the routes
  (loop for route in (routes solution)
        doing (simulate-route route solution problem action))

  ;; finish the solution-simulation
  (finish-the-solution-simulation solution problem action))

(defbehavior simulate-solution :before
  ((solution t)
   (problem  t)
   (action   basic-solution-distance-action))

  :log-str "Initializing the basic-solution-distance-action"

  ;; set the total-distance to 0
  (setf (total-distance action) 0))

(defbehavior simulate-solution :before
  ((solution t)
   (problem  t)
   (action   basic-vehicle-capacity-action))

  :log-str "Initializing the capacity-violation of the action to 0."

  ;; set the capacity-violation to 0
  (setf (capacity-violation action) 0))
