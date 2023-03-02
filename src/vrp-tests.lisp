(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                            (5 0 6 7 2 4)  ;1
                                            (1 8 0 3 9 1)  ;2
                                            (4 5 1 0 5 7)  ;3
                                            (4 5 1 5 0 6)  ;4
                                            (1 5 7 6 9 0)) ;5
                                            ;0 1 2 3 4 5 
                                :demands '(20 10 15 40 30)
                                :capacity 40)
  (let* ((s1 nil))

    (setf s1 (make-initial-solution-for-cvrp-deterministic p1))
    (format t "Type-of s1: ~a~%" (type-of s1))
    (format t "Infinite-fleet s1: ~a~%" (subtypep (type-of s1)
                                                  'has-infinite-fleet))
    (pp-solution s1 t) (terpri)))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                            (5 0 6 7 2 4)  ;1
                                            (1 8 0 3 9 1)  ;2
                                            (4 5 1 0 5 7)  ;3
                                            (4 5 1 5 0 6)  ;4
                                            (1 5 7 6 9 0)) ;5
                                            ;0 1 2 3 4 5 
                                :demands '(20 10 15 40 30)
                                :capacity 40)
  (let* ((s1 nil))

    (setf s1 (make-initial-solution-for-cvrp-random p1))
    (format t "Type-of s1: ~a~%" (type-of s1))
    (format t "Infinite-fleet s1: ~a~%" (subtypep (type-of s1)
                                                  'has-infinite-fleet))
    (pp-solution s1 t) (terpri)))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                            (5 0 6 7 2 4)  ;1
                                            (1 8 0 3 9 1)  ;2
                                            (4 5 1 0 5 7)  ;3
                                            (4 5 1 5 0 6)  ;4
                                            (1 5 7 6 9 0)) ;5
                                            ;0 1 2 3 4 5 
                                :demands '(20 10 15 40 30)
                                :capacity 40)
  (let* ((s1 nil))

    (setf s1 (make-initial-random-cvrp-solution p1))
    (pp-solution s1 t) (terpri)))

(with-finite-fleet-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                                 (5 0 6 7 2 4)  ;1
                                                 (1 8 0 3 9 1)  ;2
                                                 (4 5 1 0 5 7)  ;3
                                                 (4 5 1 5 0 6)  ;4
                                                 (1 5 7 6 9 0)) ;5
                                                 ;0 1 2 3 4 5 
                                    :demands '(20 10 15 40 30)
                                    :capacities `(40 40 50))
  (let* ((s1 nil))

    (setf s1 (make-initial-solution-for-finite-fleet-cvrp-deterministic
              p1))
    (pp-solution s1 t) (terpri)))

(let* ((s1 nil)
       (action (basic-cvrp-action)))

    (setf s1 (make-initial-solution-for-finite-fleet-cvrp-deterministic
              ff-a-n33-k6-problem))
    (pp-solution s1 t) (terpri)
    ;; let's simulate the solution
    (simulate-solution s1 ff-a-n33-k6-problem action)
    (format t "Distance: ~a~%Penalty: ~a~%"
            (total-distance action)
            (total-penalty action)))

(with-finite-fleet-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                                 (5 0 6 7 2 4)  ;1
                                                 (1 8 0 3 9 1)  ;2
                                                 (4 5 1 0 5 7)  ;3
                                                 (4 5 1 5 0 6)  ;4
                                                 (1 5 7 6 9 0)) ;5
                                                 ;0 1 2 3 4 5 
                                    :demands '(20 10 15 40 30)
                                    :capacities `(40 40 50))
  (let* ((s1 nil))

    (setf s1 (make-initial-solution-for-finite-fleet-cvrp-random
              p1))
    (pp-solution s1 t) (terpri)))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))
           (criterion-code `((select-route r1)
                             (select-client c1 from r1)
                             (select-route r2)
                             (select-client c2 from r2)
                             (swap-clients c1 c2)))

           (rarac-exhaustive
            (make-neighborhood-criterion
             criterion-code
             +exhaustive-search-strategy+
             +best-improvement+))
           )

      (bformat t "Testing DNS")




      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (descent-neighborhood-search 
                     p1 s1 ref-best
                     :max-iter 30
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

      ;; (if best-solution-first-improvement
      ;;     (then
      ;;       (format t "Best value through 1st: ~a~%"
      ;;               (cost best-solution-first-improvement))
      ;;       (format t "Best neighbor through 1st:~%")
      ;;       (pp-working-copy
      ;;        (prepare-solution-for-neighborhood-exploration
      ;;         best-solution-first-improvement) t) (terpri)))

      ;; (if best-solution-random-improvement
      ;;     (then
      ;;       (format t "Best value through random: ~a~%"
      ;;               (cost best-solution-first-improvement))
      ;;       (format t "Best neighbor through random::~%")
      ;;       (pp-working-copy
      ;;        (prepare-solution-for-neighborhood-exploration
      ;;         best-solution-random-improvement) t) (terpri)))


          )))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((2) (4) (5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))
           (criterion-code `((select-route r1)
                             (select-client c1 from r1)
                             (select-route r2)
                             (insert-client c1 into r2)))

           (rarac-exhaustive
            (make-neighborhood-criterion
             criterion-code
             +exhaustive-search-strategy+
             +best-improvement+)))

      (bformat t "Testing DNS")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (descent-neighborhood-search  
                     p1 s1 rarac-exhaustive
                     :max-iter 30
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

      ;; (if best-solution-first-improvement
      ;;     (then
      ;;       (format t "Best value through 1st: ~a~%"
      ;;               (cost best-solution-first-improvement))
      ;;       (format t "Best neighbor through 1st:~%")
      ;;       (pp-working-copy
      ;;        (prepare-solution-for-neighborhood-exploration
      ;;         best-solution-first-improvement) t) (terpri)))

      ;; (if best-solution-random-improvement
      ;;     (then
      ;;       (format t "Best value through random: ~a~%"
      ;;               (cost best-solution-first-improvement))
      ;;       (format t "Best neighbor through random::~%")
      ;;       (pp-working-copy
      ;;        (prepare-solution-for-neighborhood-exploration
      ;;         best-solution-random-improvement) t) (terpri)))


          )))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((3 2 4) (1 5)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))
           (criterion-code `((select-route r1)
                             (select-client c1 from r1)
                             (select-route r2)
                             (insert-client c1 into r2)))

           (rarac-exhaustive
            (make-neighborhood-criterion
             criterion-code
             +exhaustive-search-strategy+
             +best-improvement+)))

      (bformat t "Testing DNS")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (descent-neighborhood-search  
                     p1 s1 rarac-exhaustive
                     :max-iter 30
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

      ;; (if best-solution-first-improvement
      ;;     (then
      ;;       (format t "Best value through 1st: ~a~%"
      ;;               (cost best-solution-first-improvement))
      ;;       (format t "Best neighbor through 1st:~%")
      ;;       (pp-working-copy
      ;;        (prepare-solution-for-neighborhood-exploration
      ;;         best-solution-first-improvement) t) (terpri)))

      ;; (if best-solution-random-improvement
      ;;     (then
      ;;       (format t "Best value through random: ~a~%"
      ;;               (cost best-solution-first-improvement))
      ;;       (format t "Best neighbor through random::~%")
      ;;       (pp-working-copy
      ;;        (prepare-solution-for-neighborhood-exploration
      ;;         best-solution-random-improvement) t) (terpri)))


          )))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6 8)  ;0
                                    (5 0 6 7 2 4 9)  ;1
                                    (1 8 0 3 9 1 2)  ;2
                                    (4 5 1 0 5 7 4)  ;3
                                    (4 5 1 5 0 6 5)  ;4
                                    (1 5 7 6 9 0 8)  ;5
                                    (3 5 8 2 7 9 0)) ;6
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20 20)
                                    :capacity 40)
  (with-cvrp-solution ;;(s1 ((1 3 2 5) (6) (4)) p1)
                      (s1 ((1 4 5) (2) (6 3)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (strategy (penalty-obj-function 1000))
           (results nil)
           ;(search :first-improvement)
           (search :exhaustive)
           ;(search :random-improvement)
           (cvrp-action (cvrp-action-with-penalty
                         :vehicle-capacity-violation 0
                         :current-distance 0
                         :factor 1000))
           (criterion-code `((select-route route1)
                             (select-client client1 from route1) 
                             (select-route route2)
                             (select-client client2 from route2)
                             (select-route route3)
                             (select-client client3 from route3)
                             (insert-client client1 into route2)
                             (swap client2 client3)))
           (neighborhood-exhaustive
            (generate-code-for-neighborhood-with-strategy
             criterion-code search)))

      (format t "~%======================~%")

      ;; first we compute the cost of the solution
      (solution-cost s1 p1 cvrp-action)
      (format t "Action: ~a~%" cvrp-action)

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-working-copy s1 t)

      (setf results (descent-neighborhood-search
                     p1 s1 neighborhood-exhaustive
                     :max-iter 30
                     :action action
                     :strategy strategy))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%" search
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%" search)
            (pp-working-copy best-solution-exhaustive t)

            (solution-cost best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (cost best-solution-exhaustive)))

          (else
            (format t "Initial solution was optimum!~%")))



          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       ;(selection-strategy +first-improvement+)
       (selection-strategy +random-improvement+)
       ;(selection-strategy +best-improvement+)
       (max-iterations 150)

       (cvrp-action (basic-cvrp-action))

       (criterion-code '((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (select-client c2 from r2)
                         (swap-clients c1 c2)))

       (neighborhood-exhaustive
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         selection-strategy)))

      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (descent-neighborhood-search
                     p1 s1 neighborhood-exhaustive
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            ;; (solution-cost best-solution-exhaustive p1 cvrp-action)

            ;; (setf best-solution-exhaustive (first results))
            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (cost best-solution-exhaustive))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(with-basic-cvrp-solution (s1 ((1 2 3)
                               (4 5 6)
                               (7 8 9)
                               (10 11 12)
                               (13 14 15)
                               (16 17 18)
                               (19 20 21)
                               (22 23 24)
                               (25 26 27)
                               (28 29 30)
                               (31 32)) a-n33-k6-problem)
    (let* ((p1 a-n33-k6-problem)
        (best-solution-exhaustive nil)
        (action (delta-cvrp-action))
        (results nil)

        ;;(selection-strategy +first-improvement+)
        (selection-strategy +random-improvement+)
        ;;(selection-strategy +best-improvement+)
        (max-iterations 150)

        (cvrp-action (basic-cvrp-action))

        (criterion-code '((select-route r1)
                          (select-client c1 from r1)
                          (select-route r2)
                          (insert-client c1 into r2)))

        (neighborhood-exhaustive
         (make-neighborhood-criterion
          criterion-code
          +exhaustive-search-strategy+
          selection-strategy)))

   (bformat t "Testing DNS with a-n33-k6")

   ;; first we compute the cost of the solution
   (simulate-solution s1 p1 cvrp-action)
   (setf (cost s1) (+ (total-distance cvrp-action)
                      (total-penalty cvrp-action)))

   (format t "Original solution (with cost ~a):~%"
           (cost s1))
   (pp-solution s1 t)

   (setf results (descent-neighborhood-search
                  p1 s1 neighborhood-exhaustive
                  :max-iter max-iterations
                  :action action))

   (format t "Iterations: ~a. Optimum found ~a.~%"
           (second results) (third results))

   (setf best-solution-exhaustive (first results))

   (if best-solution-exhaustive
       (then
         (format t "Best value through ~a: ~a~%"
                 selection-strategy
                 (cost best-solution-exhaustive))
         (format t "Best neighbor through ~a:~%"
                 selection-strategy)
         (pp-solution best-solution-exhaustive t)

         ;; (solution-cost best-solution-exhaustive p1 cvrp-action)

         ;; (setf best-solution-exhaustive (first results))
         ;; (format t "Best value through Yoel's: ~a~%"
         ;;         (cost best-solution-exhaustive))
         ;; (format t "action: ~a~%" cvrp-action)
         )

       (else
         (format t "Initial solution was optimum!~%")))



   ))

(let* ((p1 a33problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (strategy (penalty-obj-function 1000))
       (results nil)
       (maximum-iterations 200)
       (search :first-improvement)
       ;(search :exhaustive)
       ;(search :random-improvement)
       (cvrp-action (cvrp-action-with-penalty
                     :vehicle-capacity-violation 0
                     :current-distance 0
                     :factor 1000))
       (criterion-code '((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))
       (neighborhood-exhaustive
        (generate-code-for-neighborhood-with-strategy
         criterion-code search)))

      (format t "~%======================~%")

      ;; first we compute the cost of the solution
      (solution-cost s1 p1 cvrp-action)
      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-working-copy s1 t)

      (setf results (descent-neighborhood-search
                     p1 s1 neighborhood-exhaustive
                     :max-iter maximum-iterations
                     :action action
                     :strategy strategy))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%" search
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%" search)
            (pp-working-copy best-solution-exhaustive t))

          (else
            (format t "Initial solution was optimum!~%"))))

(let* ((p1 a33problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (strategy (penalty-obj-function 1000))
       (results nil)
       (maximum-iterations 200)
       ;(search :first-improvement)
       (search :exhaustive)
       ;(search :random-improvement)
       (cvrp-action (cvrp-action-with-penalty
                     :vehicle-capacity-violation 0
                     :current-distance 0
                     :factor 1000))
       (criterion-code `((select-route route1)
                         (select-client client1 from route1) 
                         (select-route route2)
                         (select-client client2 from route2)
                         (select-route route3)
                         (select-client client3 from route3)
                         (insert-client client1 into route2)
                         (swap client2 client3)))
       (neighborhood-exhaustive
        (generate-code-for-neighborhood-with-strategy
         criterion-code search)))

      (format t "~%======================~%")

      ;; first we compute the cost of the solution
      (solution-cost s1 p1 cvrp-action)
      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-working-copy s1 t)

      (setf results (descent-neighborhood-search
                     p1 s1 neighborhood-exhaustive
                     :max-iter maximum-iterations
                     :action action
                     :strategy strategy))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%" search
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%" search)
            (pp-working-copy best-solution-exhaustive t))

          (else
            (format t "Initial solution was optimum!~%"))))

(let* ((p1 a33problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (strategy (penalty-obj-function 1000))
       (results nil)
       (maximum-iterations 200)
       ;(search :first-improvement)
       (search :exhaustive)
       ;(search :random-improvement)
       (cvrp-action (cvrp-action-with-penalty
                     :vehicle-capacity-violation 0
                     :current-distance 0
                     :factor 1000))
       (criterion-code `((select-route route1)
                         (select-client client1 from route1) 
                         (select-route route2)
                         (select-client client2 from route2)
                         (select-route route3)
                         (select-route route4)
                         (insert-client client1 into route3)
                         (insert-client client2 into route4)))
       (neighborhood-exhaustive
        (generate-code-for-neighborhood-with-strategy
         criterion-code search)))

      (format t "~%======================~%")

      ;; first we compute the cost of the solution
      (solution-cost s1 p1 cvrp-action)
      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-working-copy s1 t)

      (setf results (descent-neighborhood-search
                     p1 s1 neighborhood-exhaustive
                     :max-iter maximum-iterations
                     :action action
                     :strategy strategy))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%" search
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%" search)
            (pp-working-copy best-solution-exhaustive t))

          (else
            (format t "Initial solution was optimum!~%"))))

(let* ((p1 a33problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (strategy (penalty-obj-function 1000))
       (results nil)
       (maximum-iterations 50)
       ;(search :first-improvement)
       ;(search :exhaustive)
       (search :random-improvement)
       (cvrp-action (cvrp-action-with-penalty
                     :vehicle-capacity-violation 0
                     :current-distance 0
                     :factor 1000))
       (criterion-code `((select-route route1)
              (select-subroute subroute1 from route1 size 3)
              (select-route route2)
              (insert-subroute subroute1 into route2)))

       ;; (criterion-code `((select-route route1)
       ;;                   (select-client client1 from route1) 
       ;;                   (select-route route2)
       ;;                   (select-client client2 from route2)
       ;;                   (select-route route3)
       ;;                   (select-route route4)
       ;;                   (insert-client client1 into route3)
       ;;                   (insert-client client2 into route4)))
       (neighborhood-exhaustive
        (generate-code-for-neighborhood-with-strategy
         criterion-code search)))

      (format t "~%======================~%")

      ;; first we compute the cost of the solution
      (solution-cost s1 p1 cvrp-action)
      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-working-copy s1 t)

      (setf results (descent-neighborhood-search
                     p1 s1 neighborhood-exhaustive
                     :max-iter maximum-iterations
                     :action action
                     :strategy strategy))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%" search
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%" search)
            (pp-working-copy best-solution-exhaustive t))

          (else
            (format t "Initial solution was optimum!~%"))))

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       (selection-strategy +first-improvement+)
       ;(selection-strategy +best-improvement+)
       ;(selection-strategy +random-improvement+)
       (max-iterations 150)

       (cvrp-action (basic-cvrp-action))

       (criterion-code '((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (select-client c2 from r2)
                         (swap-clients c1 c2)))

       (neighborhood-exhaustive
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         selection-strategy)))

      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (descent-neighborhood-search
                     p1 s1 neighborhood-exhaustive
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            ;; (solution-cost best-solution-exhaustive p1 cvrp-action)

            ;; (setf best-solution-exhaustive (first results))
            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (cost best-solution-exhaustive))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n80-k10-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       ;(selection-strategy +first-improvement+)
       ;(selection-strategy +best-improvement+)
       (selection-strategy *random-improvement*)
       (max-iterations 150)

       (cvrp-action (basic-cvrp-action))

       (criterion-code rereehf)
       ;; (criterion-code rab)

       (neighborhood-exhaustive
        (make-neighborhood-criterion
         criterion-code
         *exhaustive-search-strategy*
         selection-strategy)))

      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (descent-neighborhood-search
                     p1
                     s1
                     neighborhood-exhaustive
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            (cost best-solution-exhaustive)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (cost best-solution-exhaustive))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n80-k10-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       ;(selection-strategy +first-improvement+)
       (selection-strategy +best-improvement+)
       ;(selection-strategy *random-improvement*)
       (max-iterations 1)

       (cvrp-action (basic-cvrp-action))

       (criterion-code rehrehgs*)
       ;; (criterion-code rab)

       (neighborhood-exhaustive
        (make-neighborhood-criterion
         criterion-code
         *exhaustive-search-strategy*
         selection-strategy)))

      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (descent-neighborhood-search
                      p1
                      s1
                      neighborhood-exhaustive
                      :max-iter max-iterations
                      :action action)))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            (cost best-solution-exhaustive)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (cost best-solution-exhaustive))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((s1 (make-initial-solution-for-finite-fleet-cvrp-deterministic
            ff-a-n33-k6-problem))
       (p1 ff-a-n33-k6-problem)
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+)))

  (bformat t "Testing DNS")

  ;; first we compute the cost of the solution
  (simulate-solution s1 p1 cvrp-action)
  (setf (cost s1) (+ (total-distance cvrp-action)
                     (total-penalty cvrp-action)))

  (format t "Original solution (with cost ~a):~%"
          (cost s1))
  (pp-solution s1 t)

  (setf results (funcall neighborhood-exploration s1 p1 action))

  ;; (format t "Cost: ~a~%" (cost results))
  ;; (pp-solution results t) (terpri)

  (setf results (descent-neighborhood-search  
                 p1 s1 neighborhood-exploration
                 :max-iter 300
                 :action action))

  ;; (format t "Cost: ~a~%" (cost (first results)))
  ;; (pp-solution (first results) t) (terpri)


  (format t "Iterations: ~a. Optimum found ~a.~%"
          (second results) (third results))      
  (setf best-solution-exhaustive (first results))

  (if best-solution-exhaustive
      (then
        (format t "Best value through exhaustive: ~a~%"
                (cost best-solution-exhaustive))
        (format t "Best neighbor through exhaustive search:~%")
        (pp-solution best-solution-exhaustive t)

        ;; (format t "Best value through Yoel's: ~a~%"
        ;;         (solution-cost best-solution-exhaustive
        ;;                        p1 cvrp-action))
        ;; (format t "action: ~a~%" cvrp-action)
        )

      (else
        (format t "Initial solution was optimum!~%")))

  ;; (if best-solution-first-improvement
  ;;     (then
  ;;       (format t "Best value through 1st: ~a~%"
  ;;               (cost best-solution-first-improvement))
  ;;       (format t "Best neighbor through 1st:~%")
  ;;       (pp-working-copy
  ;;        (prepare-solution-for-neighborhood-exploration
  ;;         best-solution-first-improvement) t) (terpri)))

  ;; (if best-solution-random-improvement
  ;;     (then
  ;;       (format t "Best value through random: ~a~%"
  ;;               (cost best-solution-first-improvement))
  ;;       (format t "Best neighbor through random::~%")
  ;;       (pp-working-copy
  ;;        (prepare-solution-for-neighborhood-exploration
  ;;         best-solution-random-improvement) t) (terpri)))


  )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (v1 (cvrp-vehicle 1 70))
       (v2 (cvrp-vehicle 2 70))

       (distance #2A ((0 1 2 3 5 6)
                      (1 0 4 5 6 7)
                      (2 4 0 6 7 9)
                      (3 5 6 0 8 9)
                      (5 6 7 8 0 1)
                      (3 5 6 4 8 0)))
       (p1 (make-instance 'finite-fleet-end-depot-cvrp-problem
                          :depot d0
                          :end-depot d1
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :fleet (list v1 v2)
                          :distance-matrix distance))

       ;; now the algorithms related data
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+))

       (*vrp-logging* 0))

  (bformat t "Testing VND with end-depot")

  (with-finite-fleet-end-depot-cvrp-solution (s1 ((1 1 2) (2 3 4)) p1)

    ;; first we compute the cost of the solution
    (simulate-solution s1 p1 cvrp-action)
    (setf (cost s1) (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

    (format t "Original solution (with cost ~a):~%"
            (cost s1))
    (pp-solution s1 t)

    (setf results (descent-neighborhood-search  
                   p1 s1 neighborhood-exploration
                   :max-iter 300
                   :action action))

    (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))


    (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))

           )

      (bformat t "Testing VNS")




      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns 
                     p1 s1 (list rab-best
                                 rarb-best
                                 rarac-best)
                     :max-iter 150
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       ;(selection-strategy +first-improvement+)
       (selection-strategy +random-improvement+)
       ;(selection-strategy +best-improvement+)
       (max-iterations 550)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns
                     p1 s1
                     (list rab-random
                           rehrf-random
                           rarac-random
                           ref-random
                           rereg-random
                           rerf-random
                           rehf-random
                           rarb-random
                           )
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       ;(search-strategy +exhaustive-search-strategy+)
       (search-strategy (random-neighborhood-search-strategy 100))

       (selection-strategy +first-improvement+)
       ;(selection-strategy +random-improvement+)
       ;(selection-strategy +best-improvement+)
       (max-iterations 550)

       (rarac-r-ex (make-neighborhood-criterion
                    rarac-description
                    search-strategy
                    selection-strategy))

       (rab-r-ex (make-neighborhood-criterion
                  rab-description
                  search-strategy
                  selection-strategy))

       (rereg-r-ex (make-neighborhood-criterion
                    rereg-description
                    search-strategy
                    selection-strategy))


       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns
                     p1 s1
                     (list
                      rarac-r-ex
                      rab-r-ex
                      rereg-r-ex
                           )
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       ;(selection-strategy +first-improvement+)
       (selection-strategy +random-improvement+)
       ;(selection-strategy +best-improvement+)
       (max-iterations 550)

       (cvrp-action (basic-cvrp-action))

       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn
                     p1 s1
                     (list rab-random
                           rarac-random
                           ref-random
                           rereg-first
                           rerf-first
                           rarb-random
                           )
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2 3 4 5)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))
           )

      (bformat t "Testing VNS with add-route")


      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-shake 
                     p1 s1 (list
                            rab   ; 0
                            rarb  ; 1
                            rarac ; 2
                            rad   ; 3   
                            )
                     :max-iter 150
                     :action action
                     :selection-strategy +best-improvement+))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code
                      p1 s1
                      (list rabs*     ;1
                            rarbs*   ;2
                            raracs*   ;3
                            refs*     ;4
                            reregs*   ;5
                            rehfs*    ;6
                            rerfs*    ;7
                            rehrfs*   ;8
                            rerehgs*  ;9
                            rehregs*  ;10
                            rehrehgs* ;11
                            )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy
                      ;; (random-improvement-smart 0.5 )
                      *random-improvement-with-candidates*
                      ;; +first-improvement+
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      ;; (time
      ;;  (setf results (vns-code
      ;;                 p1 s1
      ;;                 (list rab         ;1
      ;;                       rarb        ;2
      ;;                       rarac       ;3
      ;;                       ref     ;4
      ;;                       rereg   ;5
      ;;                       rerf    ;6
      ;;                       rehf    ;7
      ;;                       rehrf   ;8
      ;;                       )
      ;;                 ;; :inner-search-max-iter max-iterations
      ;;                 :action action
      ;;                 :max-iter max-iterations
      ;;                 :selection-strategy +best-improvement+
      ;;                 ;; :shake-search (jump-around-search-strategy no-of-jumps)
      ;;                 :search-strategy +exhaustive-search-strategy+)))

      (time
       (setf results (vns-code
                      p1 s1
                      (list
                       rabs             ;0
                       rarbs            ;1
                       raracs           ;2
                       refs             ;3
                       rerfs            ;4
                       rehfs            ;5
                       rehrfs           ;6
                       reregs           ;7
                       rehrfs           ;8
                       rerehgs          ;9
                       rehregs          ;10
                       rehrehgs          ;11
                       )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy +best-improvement+
                      ;; :shake-search (jump-around-search-strategy no-of-jumps)
                      :search-strategy +exhaustive-search-strategy+)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code
                      p1 s1
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*             ;3
                       rerfs*            ;4
                       rehfs*            ;5
                       rehrfs*           ;6
                       reregs*           ;7
                       rehrfs*           ;8
                       rerehgs*          ;9
                       rehregs*          ;10
                       rehrehgs*         ;11
                       )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy *random-improvement*
                      ;; :shake-search (jump-around-search-strategy no-of-jumps)
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n80-k10-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code
                      p1 s1
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*             ;3
                       rerfs*            ;4
                       rehfs*            ;5
                       rehrfs*           ;6
                       reregs*           ;7
                       rehrfs*           ;8
                       rerehgs*          ;9
                       rehregs*          ;10
                       rehrehgs*         ;11
                       )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter 1000
                      :selection-strategy *random-improvement*
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((2 4 5) (3) (1)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000)))

      (bformat t "Testing VNS")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-no-output 
                     p1 s1 (list rab-best
                                 rarb-best
                                 rarac-best)
                     :max-iter 150
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       ;(selection-strategy +first-improvement+)
       (selection-strategy +random-improvement+)
       ;(selection-strategy +best-improvement+)
       (max-iterations 550)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-no-output
                     p1 s1
                     (list rab-random
                           rehrf-random
                           rarac-random
                           ref-random
                           rereg-random
                           rerf-random
                           rehf-random
                           rarb-random
                           )
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       ;(search-strategy +exhaustive-search-strategy+)
       (search-strategy (random-neighborhood-search-strategy 100))

       (selection-strategy +first-improvement+)
       ;(selection-strategy +random-improvement+)
       ;(selection-strategy +best-improvement+)
       (max-iterations 550)

       (rarac-r-ex (make-neighborhood-criterion
                    rarac-description
                    search-strategy
                    selection-strategy))

       (rab-r-ex (make-neighborhood-criterion
                  rab-description
                  search-strategy
                  selection-strategy))

       (rereg-r-ex (make-neighborhood-criterion
                    rereg-description
                    search-strategy
                    selection-strategy))


       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns
                     p1 s1
                     (list
                      rarac-r-ex
                      rab-r-ex
                      rereg-r-ex
                           )
                     :max-iter max-iterations
                     :action action))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through ~a: ~a~%"
                    selection-strategy
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through ~a:~%"
                    selection-strategy)
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2 3 4 5)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))
           )

      (bformat t "Testing VNS with add-route")


      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-code-no-output 
                     p1 s1 (list
                            rab   ; 0
                            rarb  ; 1
                            rarac ; 2
                            rad   ; 3   
                            )
                     :max-iter 150
                     :action action
                     :selection-strategy +best-improvement+))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code-no-output
                      p1 s1
                      (list rabs*     ;1
                            rarbs*   ;2
                            raracs*   ;3
                            refs*     ;4
                            reregs*   ;5
                            rehfs*    ;6
                            rerfs*    ;7
                            rehrfs*   ;8
                            rerehgs*  ;9
                            rehregs*  ;10
                            rehrehgs* ;11
                            )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy
                      ;; (random-improvement-smart 0.5 )
                      *random-improvement*
                      ;; +first-improvement+
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(with-cvrp-problem (p1 :distances `((0 2 3 4 5 6)  ;0
                                    (5 0 6 7 2 4)  ;1
                                    (1 8 0 3 9 1)  ;2
                                    (4 5 1 0 5 7)  ;3
                                    (4 5 1 5 0 6)  ;4
                                    (1 5 7 6 9 0)) ;5
                                    ;0 1 2 3 4 5 
                                    :demands '(10 10 15 40 20)
                                    :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2 3 4 5)) p1)
    (let* ((best-solution-exhaustive nil)
           (action (delta-cvrp-action))
           (results nil)
           (cvrp-action (basic-cvrp-action 
                         :penalty-factor 1000))
           )

      (bformat t "Testing VNS with add-route")


      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-shake 
                     p1 s1 (list
                            rab   ; 0
                            rarb  ; 1
                            rarac ; 2
                            rad   ; 3   
                            )
                     :max-iter 150
                     :action action
                     :selection-strategy +best-improvement+))

      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))      
      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through exhaustive: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor through exhaustive search:~%")
            (pp-solution best-solution-exhaustive t)

            ;; (format t "Best value through Yoel's: ~a~%"
            ;;         (solution-cost best-solution-exhaustive
            ;;                        p1 cvrp-action))
            ;; (format t "action: ~a~%" cvrp-action)
            )

          (else
            (format t "Initial solution was optimum!~%")))

          )))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-code-bar
                     p1 s1
                     (list
                      rabs*             ;0
                      rarbs*            ;1
                      raracs*           ;2
                      refs*             ;3
                      rerfs*            ;4
                      rehfs*            ;5
                      rehrfs*           ;6
                      reregs*           ;7
                      rehrfs*           ;8
                      rerehgs*          ;9
                      rehregs*          ;10
                      rehrehgs*         ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy *first-improvement*
                     ;; :shake-search (jump-around-search-strategy no-of-jumps)
                     :search-strategy *exhaustive-search-strategy*))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-code-bar
                     p1 s1
                     (list
                      rabs*             ;0
                      rarbs*            ;1
                      raracs*           ;2
                      refs*             ;3
                      rerfs*            ;4
                      rehfs*            ;5
                      rehrfs*           ;6
                      reregs*           ;7
                      rehrfs*           ;8
                      rerehgs*          ;9
                      rehregs*          ;10
                      rehrehgs*         ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +first-improvement+
                     :search-strategy +exhaustive-search-strategy+))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (v1 (cvrp-vehicle 1 70))
       (v2 (cvrp-vehicle 2 70))

       (distance #2A ((0 1 2 3 5 6)
                      (1 0 4 5 6 7)
                      (2 4 0 6 7 9)
                      (3 5 6 0 8 9)
                      (5 6 7 8 0 1)
                      (3 5 6 4 8 0)))
       (p1 (make-instance 'finite-fleet-end-depot-cvrp-problem
                          :depot d0
                          :end-depot d1
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :fleet (list v1 v2)
                          :distance-matrix distance))

       ;; now the algorithms related data
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)
       (cvrp-action (basic-cvrp-action 
                     :penalty-factor 1000))
       (criterion-code `((select-route r1)
                         (select-client c1 from r1)
                         (select-route r2)
                         (insert-client c1 into r2)))

       (neighborhood-exploration
        (make-neighborhood-criterion
         criterion-code
         +exhaustive-search-strategy+
         +best-improvement+))

       (results nil)
       (max-iterations 1000)

       (*vrp-logging* 0))

  (bformat t "Testing VNS with end-depot")

  (with-finite-fleet-end-depot-cvrp-solution (s1 ((1 1 2) (2 3 4)) p1)

    ;; first we compute the cost of the solution
    (simulate-solution s1 p1 cvrp-action)
    (setf (cost s1) (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))

    (format t "Original solution (with cost ~a):~%"
            (cost s1))
    (pp-solution s1 t)

    (setf results (vns-code-bar
                     p1 s1
                     (list
                      rabs*             ;0
                      rarbs*            ;1
                      raracs*           ;2
                      refs*             ;3
                      rerfs*            ;4
                      rehfs*            ;5
                      rehrfs*           ;6
                      reregs*           ;7
                      rehrfs*           ;8
                      rerehgs*          ;9
                      rehregs*          ;10
                      rehrehgs*         ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy *first-improvement*
                     ;; :shake-search (jump-around-search-strategy no-of-jumps)
                     :search-strategy *exhaustive-search-strategy*))

    (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

    (setf best-solution-exhaustive (first results))

    (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))


    ))

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-bar 
                     p1 s1
                     (mapcar (lambda (x)
                               (make-neighborhood-criterion
                                x
                                *exhaustive-search-strategy*
                                *random-improvement*
                                ))
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*            ;3
                       rerfs*           ;4
                       rehfs*           ;5
                       rehrfs*          ;6
                       reregs*          ;7
                       rehrfs*          ;8
                       rerehgs*         ;9
                       rehregs*         ;10
                       rehrehgs*        ;11
                       ))
                     :max-iter max-iterations 
                     :action action))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      (format t "length delta-distance-stack: ~a~%"
              (length (delta-distance-stack action )))
      (format t "length total-penalty: ~a~%"
              (length (total-penalty-stack action )))

      )

(let* ((p1 a-n80-k10-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-bar 
                     p1 s1
                     (mapcar (lambda (x)
                               (make-neighborhood-criterion
                                x
                                *exhaustive-search-strategy*
                                *random-improvement*
                                ))
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*            ;3
                       rerfs*           ;4
                       rehfs*           ;5
                       rehrfs*          ;6
                       reregs*          ;7
                       rehrfs*          ;8
                       rerehgs*         ;9
                       rehregs*         ;10
                       rehrehgs*        ;11
                       ))
                     :max-iter max-iterations 
                     :action action))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      (format t "length delta-distance-stack: ~a~%"
              (length (delta-distance-stack action )))
      (format t "length total-penalty: ~a~%"
              (length (total-penalty-stack action )))

      )

(let* ((p1 ff-a-n33-k6-problem)
       (s1 (make-initial-solution-for-finite-fleet-cvrp-deterministic
            p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (get-cost-from-action cvrp-action))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (setf results (vns-fn-bar 
                     p1 s1
                     (mapcar (lambda (x)
                               (make-neighborhood-criterion
                                x
                                *exhaustive-search-strategy*
                                *random-improvement*
                                ))
                      (list
                       rabs*            ;0
                       rarbs*           ;1
                       raracs*          ;2
                       refs*            ;3
                       rerfs*           ;4
                       rehfs*           ;5
                       rehrfs*          ;6
                       reregs*          ;7
                       rehrfs*          ;8
                       rerehgs*         ;9
                       rehregs*         ;10
                       rehrehgs*        ;11
                       ))
                     :max-iter max-iterations 
                     :action action))


      (format t "~%Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      (format t "length delta-distance-stack: ~a~%"
              (length (delta-distance-stack action )))
      (format t "length total-penalty: ~a~%"
              (length (total-penalty-stack action )))

      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action)))



      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code-scrambling
                      p1 s1
                      (list
                       rabs*            ;1
                       rarbs*           ;2
                       raracs*          ;3
                       refs*             ;4
                       rerfs*            ;5
                       rehfs*            ;6
                       rehrfs*           ;7
                       reregs*           ;8
                       rehrfs*           ;9
                       rerehgs*          ;10
                       rehregs*          ;11
                       rehrehgs*         ;12
                       )
                      ;; :inner-search-max-iter max-iterations
                      :action action
                      :max-iter max-iterations
                      :selection-strategy *random-improvement*
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 1000)
       ;; (no-of-jumps 4)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (setf results (vns-code-scrambling
                      p1 s1
                      (list
                       rabs*            ;1
                       rarbs*           ;2
                       raracs*          ;3
                       refs*            ;4
                       rerfs*           ;5
                       rehfs*           ;6
                       rehrfs*          ;7
                       reregs*          ;8
                       rehrfs*          ;9
                       rerehgs*         ;10
                       rehregs*         ;11
                       rehrehgs*        ;12
                       )
                      :action action
                      :max-iter max-iterations
                      :selection-strategy *random-improvement*
                      :search-strategy *exhaustive-search-strategy*)))


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       (max-iterations 200)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      ;; (setf results (vns-shake
      ;;                p1 s1
      ;;                (list
      ;;                 rab
      ;;                 rehrf
      ;;                 rarac
      ;;                 ref
      ;;                 rereg
      ;;                 rerf
      ;;                 rehf
      ;;                 rarb
      ;;                 )
      ;;                :max-iter max-iterations
      ;;                :action action
      ;;                ;; :shake-search (jump-around-search-strategy 10)
      ;;                :shake-search (jump-around-search-strategy 5)
      ;;                :shake-selection +jump-around-return-last-neighbor+
      ;;                :descent-selection-strategy +random-improvement+))

      (setf results (vns-code
                     p1 s1
                     (list
                      rab
                      rehrf
                      rarac
                      ref
                      rereg
                      rerf
                      rehf
                      rarb
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +random-improvement+))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))



      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       (max-iterations 100)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
       ;; (setf results (vns-shake
       ;;                p1 s1
       ;;                (list
       ;;                 ; rabs
       ;;                 rab
       ;;                 rehrf
       ;;                 ;raracs
       ;;                 rarac
       ;;                 ref
       ;;                 rereg
       ;;                 rerf
       ;;                 rehf
       ;;                 ;rarbs
       ;;                 rarb
       ;;                 )
       ;;                :max-iter max-iterations
       ;;                :action action
       ;;                ;; :shake-search (jump-around-search-strategy 10)
       ;;                :shake-search (jump-around-search-strategy 1)
       ;;                :shake-selection +jump-around-return-last-neighbor+
       ;;                :selection-strategy +first-improvement+))

         (setf results (vns-shake 
                     p1 s1
                     (list
                      rab
                      ;;rehrf
                      raracs
                      ref
                      reregs
                      ;;rerf
                      ;;rehf
                      rarb
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +random-improvement+
                     :inner-search-max-iter 20))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      ))

      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action))
       (results nil)

       (max-iterations 100)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake 
                     p1 s1
                     (list
                      rabs*              ;1
                      rarbs*             ;2
                      raracs*            ;3
                      refs*              ;4
                      reregs*            ;5
                      rehfs*             ;6
                      rerfs*             ;7
                      rads*              ;8
                      rehrfs*            ;9
                      rerehgs*           ;10
                      rehregs*           ;11
                      rehrehgs*          ;12
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +random-improvement+
                     :inner-search-max-iter 20))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%")))

      ))

      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (results nil)

       (max-iterations 20)

       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake 
                     p1 s1
                     (list
                      rabs*              ;1
                      rarbs*             ;2
                      raracs*            ;3
                      refs*              ;4
                      ;; reregs*            ;5
                      ;; rehfs*             ;6
                      ;; rerfs*             ;7
                      ;; rads*              ;8
                      ;; rehrfs*            ;9
                      ;; rerehgs*           ;10
                      ;; rehregs*           ;11
                      ;; rehrehgs*          ;12
                      )
                     :max-iter max-iterations
                     :action action
                     :selection-strategy +random-improvement+
                     :inner-search-max-iter 10))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (+ (total-distance cvrp-action)
                       (total-penalty cvrp-action)))
            )

          (else
            (format t "Initial solution was optimum!~%"))))) ;; time

      (format t "distance-stack: ~a~%"
              (length (delta-distance-stack action)))

      )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy *random-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 200))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%"))))) ;; time

      ;; (format t "distance-stack: ~a~%"
      ;;         (length (delta-distance-stack action)))

      )

(let* ((p1 a-n80-k10-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy *random-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 60))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%"))))) ;; time

      ;; (format t "distance-stack: ~a~%"
      ;;         (length (delta-distance-stack action)))

      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy
                     ;;(random-improvement-smart 0.7)
                      *first-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 50))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))))
      ;; time

      ;; (format t "distance-stack: ~a~%"
      ;;         (length (delta-distance-stack action)))

      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-initial-random-cvrp-solution p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing DNS with a-n65-k9-problem")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      ;; rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy
                     ;; (random-improvement-smart 0.7)
                      *first-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 500))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))))
      ;; time

      ;; (format t "distance-stack: ~a~%"
      ;;         (length (delta-distance-stack action)))

      )

(let* ((p1 a-n65-k9-problem)
       (s1 (make-basic-cvrp-solution-from-list
            1 `(( 2 )
                ( 23 )( 48 )( 35  46 )( 45  6 )( 63 )( 55 )
                ( 51  38 )( 40 )( 28  11 )( 25 )(43  22  4 )
                ( 29  42  50  60  19 )( 57 )( 49 )( 53 )( 41 )
                ( 44  37 )( 52 )( 16  15  5 )( 10  58 )
                ( 64  24  39 )( 61 )( 47  21 )( 9 )( 14  32 )
                ( 56 )( 33  12  34  27 )( 62 )( 20  26  1 )
                ( 17 )( 18 )( 13  59 )( 54  36 30  7  3  8 )
                ( 31 ))
            a-n65-k9-problem))
            (best-solution-exhaustive nil)
            (action (delta-cvrp-action*))
            (action-for-shake (delta-cvrp-action))
            (results nil)
            (max-iterations 200)
            (cvrp-action (basic-cvrp-action))
            )


           (bformat t "Testing DNS with a-n65-k9-problem")

           ;; first we compute the cost of the solution
           (simulate-solution s1 p1 cvrp-action)
           (setf (cost s1) (+ (total-distance cvrp-action)
                              (total-penalty cvrp-action)))

           (format t "Original solution (with cost ~a):~%"
                   (cost s1))
           (pp-solution s1 t)

           (time
            (progn
              (setf results (vns-shake-smart
                          p1 s1
                          (list
                           rabs*              ;0
                           rarbs*             ;1
                           raracs*            ;2
                           refs*              ;3
                           reregs*            ;4
                           rehfs*             ;5
                           rerfs*             ;6
                           rads*              ;7
                           rehrfs*            ;8
                           rerehgs*           ;9
                           rehregs*           ;10
                           rehrehgs*          ;11
                           )
                          :max-iter max-iterations
                          :action action
                          :action-for-shake action-for-shake
                          :selection-strategy
                          ;; (random-improvement-smart 0.7)
                           *first-improvement*
                          :search-strategy *exhaustive-search-strategy*
                          :inner-search-max-iter 50))

           (format t "finished vns-shake~%")


           (format t "Iterations: ~a. Optimum found ~a.~%"
                   (second results) (third results))

           (setf best-solution-exhaustive (first results))

           (if best-solution-exhaustive
               (then
                 (format t "Best value through: ~a~%"
                         (cost best-solution-exhaustive))
                 (format t "Best neighbor:~%")
                 (pp-solution best-solution-exhaustive t)

                 (simulate-solution best-solution-exhaustive p1 cvrp-action)

                 (setf best-solution-exhaustive (first results))
                 (format t "Best value through Yoel's: ~a~%"
                         (get-cost-from-action cvrp-action))
                 )

               (else
                 (format t "Initial solution was optimum!~%")))))
           ;; time

           ;; (format t "distance-stack: ~a~%"
           ;;         (length (delta-distance-stack action)))

           )

(let* ((p1 ff-a-n33-k6-problem)
       (s1 (make-initial-solution-for-finite-fleet-cvrp-random
            p1))
            (best-solution-exhaustive nil)
            (action (delta-cvrp-action*))
            (action-for-shake (delta-cvrp-action))
            (results nil)
            (max-iterations 200)
            (cvrp-action (basic-cvrp-action))
            )


           (bformat t "Testing DNS with a-n65-k9-problem")

           ;; first we compute the cost of the solution
           (simulate-solution s1 p1 cvrp-action)
           (setf (cost s1) (+ (total-distance cvrp-action)
                              (total-penalty cvrp-action)))

           (format t "Original solution (with cost ~a):~%"
                   (cost s1))
           (pp-solution s1 t)

           (time
            (progn
              (setf results (vns-shake-smart
                          p1 s1
                          (list
                           rabs*              ;0
                           rarbs*             ;1
                           raracs*            ;2
                           refs*              ;3
                           reregs*            ;4
                           ;; rehfs*             ;5
                           rerfs*             ;6
                           ;; rads*              ;7
                           ;; rehrfs*            ;8
                           ;; rerehgs*           ;9
                           ;; rehregs*           ;10
                           ;; rehrehgs*          ;11
                           )
                          :max-iter max-iterations
                          :action action
                          :action-for-shake action-for-shake
                          :selection-strategy
                          ;; (random-improvement-smart 0.7)
                           *first-improvement*
                          :search-strategy *exhaustive-search-strategy*
                          :inner-search-max-iter 50))

           (format t "finished vns-shake~%")


           (format t "Iterations: ~a. Optimum found ~a.~%"
                   (second results) (third results))

           (setf best-solution-exhaustive (first results))

           (if best-solution-exhaustive
               (then
                 (format t "Best value through: ~a~%"
                         (cost best-solution-exhaustive))
                 (format t "Best neighbor:~%")
                 (pp-solution best-solution-exhaustive t)

                 (simulate-solution best-solution-exhaustive p1 cvrp-action)

                 (setf best-solution-exhaustive (first results))
                 (format t "Best value through Yoel's: ~a~%"
                         (get-cost-from-action cvrp-action))
                 )

               (else
                 (format t "Initial solution was optimum!~%")))))
           ;; time

           ;; (format t "distance-stack: ~a~%"
           ;;         (length (delta-distance-stack action)))

           )

(let* ((p1 a-n33-k6-problem)
       (s1 (make-initial-solution-for-cvrp-deterministic p1))
       (best-solution-exhaustive nil)
       (action (delta-cvrp-action*))
       (action-for-shake (delta-cvrp-action))
       (results nil)
       (max-iterations 200)
       (cvrp-action (basic-cvrp-action))
       )


      (bformat t "Testing VNS shake no output with a-n33-k6")

      ;; first we compute the cost of the solution
      (simulate-solution s1 p1 cvrp-action)
      (setf (cost s1) (+ (total-distance cvrp-action)
                         (total-penalty cvrp-action)))

      (format t "Original solution (with cost ~a):~%"
              (cost s1))
      ;; (pp-solution s1 t)

      (time
       (progn
         (setf results (vns-shake-smart-no-output
                     p1 s1
                     (list
                      rabs*              ;0
                      rarbs*             ;1
                      raracs*            ;2
                      refs*              ;3
                      reregs*            ;4
                      rehfs*             ;5
                      rerfs*             ;6
                      rads*              ;7
                      rehrfs*            ;8
                      rerehgs*           ;9
                      rehregs*           ;10
                      rehrehgs*          ;11
                      )
                     :max-iter max-iterations
                     :action action
                     :action-for-shake action-for-shake
                     :selection-strategy *random-improvement*
                     :search-strategy *exhaustive-search-strategy*
                     :inner-search-max-iter 40))

      (format t "finished vns-shake~%")


      (format t "Iterations: ~a. Optimum found ~a.~%"
              (second results) (third results))

      (setf best-solution-exhaustive (first results))

      (if best-solution-exhaustive
          (then
            (format t "Best value through: ~a~%"
                    (cost best-solution-exhaustive))
            (format t "Best neighbor:~%")
            (pp-solution best-solution-exhaustive t)

            (simulate-solution best-solution-exhaustive p1 cvrp-action)

            (setf best-solution-exhaustive (first results))
            (format t "Best value through Yoel's: ~a~%"
                    (get-cost-from-action cvrp-action))
            )

          (else
            (format t "Initial solution was optimum!~%")))))

      )

(progn (defparameter c1 (basic-cvrp-client 1 1))
       (defparameter c2 (basic-cvrp-client 2 1))
       (defparameter c3 (basic-cvrp-client 3 4))
       (defparameter c4 (basic-cvrp-client 4 3))
       (defparameter c5 (basic-cvrp-client 5 2))
       (defparameter c6 (basic-cvrp-client 6 1))

       (defparameter v1 (cvrp-vehicle 1 10))
       (defparameter v2 (cvrp-vehicle 2 10))

       (defparameter d0 (basic-depot))

       ;; Solution for the eval-graph
       ;; remember to start and end all the routes with depot
       ;; the starting depot is placed in the previous-client slot
       (defparameter r1 (route-for-simulation :id 1 :vehicle v1 :depot d0
					      :clients (list c1 c2 c3 (clone d0)) :previous-client (clone d0)))
       (defparameter r2 (route-for-simulation :id 2 :vehicle v2 :depot d0
					      :clients (list c4 c5 c6 (clone d0)) :previous-client (clone d0)))

       (defparameter s1 (basic-solution :id 1 :routes (list r1 r2)))

       (defparameter dist-mat #2A((0 1 2 3 4 5 6)
				  (1 0 9 2 7 3 2)
				  (2 9 0 2 2 2 2)
				  (3 2 2 0 8 2 1)
				  (4 7 2 8 0 2 9)
				  (5 3 2 2 2 0 1)
				  (6 2 2 1 9 1 0)))
       (defparameter problem (finite-fleet-cvrp-problem :id 1 :clients (list c1 c2 c3 c4 c5 c6)
							:depot d0 :distance-matrix dist-mat :fleet (list v1 v2) ))

       (defparameter graph (init-graph s1)))

  ;; let's evaluate the solution

  (progn
    (def-var total-distance 0 graph)
    (loop for r in (routes s1) do 
	 (progn
	   (def-var route-distance 0 graph)
	   (def-var route-demand (capacity (vehicle r)) graph) 
	   (loop for c in (clients r) do 
		(progn
		  (increment-distance (previous-client r) c route-distance dist-mat graph)
		  (decrement-demand c route-demand graph) 
		  (setf (previous-client r) c)))
	   (increment-value total-distance route-distance graph)
	   (apply-penalty route-demand total-distance 10 graph)) 
	 (return-value total-distance graph)))

  (format t "initial cost: ~a~%" (output-value (output graph)))
  (format t "initial solution: ~a~%" (solution-track graph))


  (progn
    (setf rab (make-neighborhood-criterion 
	       `((select-route r1)
		 (select-client c1 from r1)
		 (insert-client c1 to r1))
	       +exhaustive-search-strategy+ 
	       +best-improvement+))

    (setf rarb (make-neighborhood-criterion 
		`((select-route r1)
		  (select-client c1 from r1)
		  (select-route r2)
		  (insert-client c1 to r2))
		+exhaustive-search-strategy+ 
		+best-improvement+))

    (setf rarac (make-neighborhood-criterion 
		 `((select-route r1)
		   (select-client c1 from r1)
		   (select-route r2)
		   (select-client c2 from r2)
		   (swap-clients c1 c2))
		 +exhaustive-search-strategy+ 
		 +best-improvement+))

(setf ref (make-neighborhood-criterion 
	   `((select-route r1)
	     (select-subroute z1 from r1)
	     (insert-subroute z1 into r1))
	   +exhaustive-search-strategy+ 
	   +best-improvement+))

  (setf rereg (make-neighborhood-criterion 
	       `((select-route r1)
		 (select-subroute c1 from r1)
		 (select-route r2)
		 (select-subroute c2 from r2)
		 (swap-subroutes c1 c2))
	+exhaustive-search-strategy+ 
	+best-improvement+))

(setf rerf (make-neighborhood-criterion 
	    `((select-route r1)
	      (select-subroute c1 from r1)
	      (select-route r2)
	      (insert-subroute c1 into r2))
	    +exhaustive-search-strategy+ 
	    +best-improvement+))

    (setf criteria (list rab rarb rarac ref rerf))

    (format t "~a" (vns-vrp-system problem criteria graph :max-iter 10000000000)))
