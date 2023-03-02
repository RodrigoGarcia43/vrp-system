(with-basic-clients (1 2 3)
  (let* ((d0 (basic-depot))
         (d1 (basic-depot 1))
         (obj1 (make-instance 'has-one-depot :depot d0))
         (obj2 (make-instance 'has-an-end-depot
                              :depot d0
                              :end-depot d1))
         )

    (bformat t "Testing end-depot...")


    (format t "Depot for obj1 (expect d0): ~a~%" (depot obj1))
    (format t "Depot for obj2 (expect d0): ~a~%" (depot obj2))
    (format t "End depot for obj1 (expect d0): ~a~%" (end-depot obj1))
    (format t "End depot for obj2 (expect d1): ~a~%" (end-depot obj2))))

(let* ((c1 (basic-product 1 "oil"))
       (c2 (basic-product 2 "water"))
       (c3 (clone c1))
       (c4 (clone c2)))
  (bformat t "Testing basic-product...")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4)
        doing (format t "   basic-product with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c2 c2))
    (check-t (obj= c1 c3))
    (check-t (obj= c2 c4))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))))

(let* ((c1 (basic-client 1))
       (c2 (basic-client 2))
       (c3 (clone c1))
       (c4 (clone c2)))
  (format t "~%=======================
Testing basic-client...
=======================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4)
        doing (format t "   basic-client with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c2 c2))
    (check-t (obj= c1 c3))
    (check-t (obj= c2 c4))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))))

(let* ((c1 (basic-cvrp-client 1 10))
       (c2 (basic-cvrp-client 2 10))
       (c3 (basic-cvrp-client 1 20))
       (c4 (clone c1))
       (c5 (clone c2)))
  (format t "~%=======================
Testing basic-cvrp-client...
=======================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5)
        doing (format t "   basic-cvrp-client with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c4))
    (check-t (obj= c2 c5))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c2 c4))
    (check-nil (obj= c3 c5))))

(let* ((c1 (basic-cupet-client 1))
       (c2 (basic-cupet-client 2))
       (c3 (clone c1))
       (c4 (clone c2)))
  (format t "Testing basic-cvrp-client...")


  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c2 c2))
    (check-t (obj= c1 c3))
    (check-t (obj= c2 c4))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4)))
  ;; (format t "Printing the objects:~%")
  ;; (loop for e in (list c1 c2 c3 c4 )
  ;;       doing (format t "   basic-cupet-client with id ~a: ~a~%"
  ;;                     (id e) e))
  )

(let* ((c1 (basic-vehicle 1))
       (c2 (basic-vehicle 2))
       (c3 (clone c1))
       (c4 (clone c2)))
  (format t "~%=======================
Testing basic-vehicle...
=======================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4)
        doing (format t "   basic-vehicle with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c2 c2))
    (check-t (obj= c1 c3))
    (check-t (obj= c2 c4))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))))

(let* ((v1 (cvrp-vehicle 1 20))
       (v2 (cvrp-vehicle 1 30))
       (v3 (cvrp-vehicle 1 20 5))
       (v4 (cvrp-vehicle 2 20))
       (v6 (clone v1))
       (v7 (clone v2)))
  (format t "~%=======================
Testing cvrp-vehicle...
=======================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list v1 v2 v3 v4 v6 v7)
        doing (format t "   cvrp-vehicle with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= v1 v1))
    (check-t (obj= v1 v6))
    (check-t (obj= v2 v7))

    (check-nil (obj= v1 v2))
    (check-nil (obj= v1 v3))
    (check-nil (obj= v1 v4))
    (check-nil (obj= v1 v2))
    (check-nil (obj= v1 v7))
    (check-nil (obj= v2 v6))))

(let* ((v1 (cvrp-vehicle 1 20))
       (v2 (cvrp-vehicle 1 30))
       (v3 (cvrp-vehicle 1 20 5))
       (v4 (cvrp-vehicle 2 20))
       (v6 (clone v1))
       (v7 (clone v2)))
  (format t "~%=======================
Testing cvrp-vehicle...
=======================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list v1 v2 v3 v4 v6 v7)
        doing (format t "   cvrp-vehicle with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= v1 v1))
    (check-t (obj= v1 v6))
    (check-t (obj= v2 v7))

    (check-nil (obj= v1 v2))
    (check-nil (obj= v1 v3))
    (check-nil (obj= v1 v4))
    (check-nil (obj= v1 v2))
    (check-nil (obj= v1 v7))
    (check-nil (obj= v2 v6))))

(let* ((c0 (basic-depot))
       (c1 (basic-depot 1))
       (c2 (basic-depot 2))
       (c3 (clone c0))
       (c4 (clone c1))
       (c5 (clone c2)))
  (format t "~%======================
Testing basic-depot...
======================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c0 c1 c2 c3 c4 c5)
        doing (format t "   basic-depot with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c0 c0))
    (check-t (obj= c1 c1))
    (check-t (obj= c2 c2))
    (check-t (obj= c1 c4))
    (check-t (obj= c2 c5))
    (check-t (obj= c3 c0))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))
    (check-nil (obj= c0 c1))))

(let* ((c1 (g-depot 1 10 10))
       (c2 (g-depot 2 0 0))
       (c3 (g-depot 3 0 0))
       (c4 (clone c1))
       (c5 (clone c2))
       (c6 (clone c3)))
  (format t "~%======================
Testing g-depot...
======================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5 c6)
        doing (format t "   g-depot with id ~a and coords (~a ~a): ~a~%"
                      (id e) (x-coord e) (y-coord e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c2 c2))
    (check-t (obj= c1 c4))
    (check-t (obj= c2 c5))
    (check-t (obj= c3 c6))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c6 c1))))

(with-basic-clients (1 2 3 4 5 6)
  (let* ((v1 (basic-vehicle 1))
         (v2 (basic-vehicle 2))
         (d0 (basic-depot))
         (d1 (basic-depot 1))
         (r1 (basic-route :id 1 :vehicle v1 :depot d0
                          :clients (list c1 c2 c3)))
         (r2 (basic-route :id 2 :vehicle v2 :depot d0
                          :clients (list c4 c5 c6)))
         (r3 (basic-route :id 1 :vehicle v2 :depot d0
                          :clients (list c1 c2 c3)))
         (r4 (basic-route :id 1 :vehicle v1 :depot d1
                          :clients (list c1 c2 c3)))
         (r5 (basic-route :id 2 :vehicle v1 :depot d0
                          :clients (list c1 c2 c3)))
         (r6 (clone r1)))

    (bformat t "Testing basic-route")

    (format t "Printing the objects:~%")
    (loop for e in (list r1 r2 r3 r4 r5 r6)
          doing (format t "   basic-route with id ~a: ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= r1 r1))
      (check-t (obj= r1 r6))

      (check-nil (obj= r1 r2))
      (check-nil (obj= r1 r2))
      (check-nil (obj= r1 r3))
      (check-nil (obj= r1 r4))
      (check-nil (obj= r2 r5)))))

(with-basic-clients (1 2 3 4 5 6)
  (let* ((v1 (basic-vehicle 1))
         (v2 (basic-vehicle 2))
         (d0 (basic-depot))
         (d1 (basic-depot 1))
         (r1 (route-for-simulation :id 1 :vehicle v1 :depot d0
                          :clients (list c1 c2 c3)))
         (r2 (route-for-simulation :id 2 :vehicle v2 :depot d0
                          :clients (list c4 c5 c6)))
         (r3 (route-for-simulation :id 1 :vehicle v2 :depot d0
                          :clients (list c1 c2 c3)))
         (r4 (route-for-simulation :id 1 :vehicle v1 :depot d1
                          :clients (list c1 c2 c3)))
         (r5 (route-for-simulation :id 2 :vehicle v1 :depot d0
                          :clients (list c1 c2 c3)))
         (r6 (clone r1)))

    (format t "~%===============================
Testing route-for-simulation...
===============================~2%")

    (format t "Printing the objects:~%")
    (loop for e in (list r1 r2 r3 r4 r5 r6)
          doing (format t "   route-for-simulation with id ~a: ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= r1 r1))
      (check-t (obj= r1 r6))

      (check-nil (obj= r1 r2))
      (check-nil (obj= r1 r2))
      (check-nil (obj= r1 r3))
      (check-nil (obj= r1 r4))
      (check-nil (obj= r2 r5)))))

(with-basic-clients (1 2 3 4 5 6)
  (let* ((v1 (basic-vehicle 1))
         (v2 (basic-vehicle 2))
         (d0 (basic-depot))
         (d1 (basic-depot 7))
         (r1 (route-for-simulation-with-end-depot
              :id 1
              :vehicle v1
              :depot d0
              :end-depot d1
              :clients (list c1 c2 c3)))
         (r2 (route-for-simulation-with-end-depot
              :id 2
              :vehicle v2
              :depot d0
              :end-depot d1
              :clients (list c4 c5 c6)))
         (r3 (route-for-simulation-with-end-depot
              :id 1
              :vehicle v2
              :depot d0
              :end-depot d1
              :clients (list c1 c2 c3)))
         (r4 (route-for-simulation-with-end-depot
              :id 1
              :vehicle v1
              :depot d1
              :end-depot d1
              :clients (list c1 c2 c3)))
         (r5 (route-for-simulation-with-end-depot
              :id 2
              :vehicle v1
              :depot d0
              :end-depot d1
              :clients (list c1 c2 c3)))
         (r6 (clone r1)))

    (bformat t "Testing route-for-simulation-with-end-depot...")

    (format t "Printing the objects:~%")
    (loop for e in (list r1 r2 r3 r4 r5 r6)
          doing (format t "   route-for-simulation with id ~a: ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= r1 r1))
      (check-t (obj= r1 r6))

      (check-nil (obj= r1 r2))
      (check-nil (obj= r1 r2))
      (check-nil (obj= r1 r3))
      (check-nil (obj= r1 r4))
      (check-nil (obj= r2 r5)))

    (format t "end-depot of r1 (expect d7): ~a~%" (end-depot r1))
    (format t "depot of r1 (expect d0): ~a~%" (depot r1))))

(with-basic-clients (1 2 3 4 5 6)
  (let* ((v1 (basic-vehicle 1))
         (v2 (basic-vehicle 2))
         (d0 (basic-depot))
         (r1 (basic-route :id 1 :vehicle v1 :depot d0
                          :clients (list c1 c2 c3)))
         (r2 (basic-route :id 2 :vehicle v2 :depot d0
                          :clients (list c4 c5 c6)))
         (s1 (basic-solution :id 1 :routes (list r1 r2)))
         (s2 (basic-solution :id 2 :routes (list r1 r2)))
         (s3 (basic-solution :id 1 :routes (list r2 r1)))
         (s4 (clone s1))
         (s5 (basic-solution :id 1 :routes (list r1 r2) :cost 1)))

    (bformat t "Testing basic-solution")

    (format t "Printing the objects:~%")
    (loop for e in (list s1 s2 s3 s4)
          doing (format t "   basic-solution with id ~a:~% ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= s1 s1))
      (check-t (obj= s1 s4))

      (check-nil (obj= s1 s2))
      (check-nil (obj= s1 s3))
      (check-nil (obj= s1 s5))
      (check-nil (obj= s2 s3))
      (check-nil (obj= s2 s4)))))

(with-basic-clients (1 2 3 4 5 6)
  (let* ((v1 (basic-vehicle 1))
         (v2 (basic-vehicle 2))
         (d0 (basic-depot))
         (r1 (basic-route :id 1 :vehicle v1 :depot d0
                          :clients (list c1 c2 c3)))
         (r2 (basic-route :id 2 :vehicle v2 :depot d0
                          :clients (list c4 c5 c6)))
         (s1 (basic-cvrp-solution :id 1 :routes (list r1 r2)))
         (s2 (basic-cvrp-solution :id 2 :routes (list r1 r2)))
         (s3 (basic-cvrp-solution :id 1 :routes (list r2 r1)))
         (s4 (clone s1))
         (s5 (basic-cvrp-solution :id 1 :routes (list r1 r2) :cost 1))
         (s6 (basic-solution :id 1 :routes (list r1 r2) :cost 1)))

    (bformat t "Testing basic-solution")

    (format t "Printing the objects:~%")
    (loop for e in (list s1 s2 s3 s4)
          doing (format t "   basic-solution with id ~a:~% ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= s1 s1))
      (check-t (obj= s1 s4))

      (check-nil (obj= s1 s2))
      (check-nil (obj= s1 s3))
      (check-nil (obj= s1 s5))
      (check-nil (obj= s2 s3))
      (check-nil (obj= s2 s4))
      (check-nil (obj= s5 s6)))))

(with-basic-clients (1 2 3 4 5 6)
  (let* ((d0 (basic-depot))
         (d1 (basic-depot 1))
         (p1 (basic-problem :id 1 :depot d0
                            :clients (list c1 c2 c3 c4 c5 c6)))
         (p2 (basic-problem :id 2 :depot d0
                            :clients (list c1 c2 c3 c4 c5 c6)))
         (p3 (basic-problem :id 1 :depot d1
                            :clients (list c1 c2 c3 c4 c5 c6)))
         (p4 (basic-problem :id 1 :depot d1
                            :clients (list c1 c2 c3 c4 c5)))
         (p5 (clone p1)))

    (bformat t "Testing basic-problem...")

    (format t "Printing the objects:~%")
    (loop for e in (list p1 p2 p3 p4 p5)
          doing (format t "   basic-problem with id ~a:~% ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= p1 p1))
      (check-t (obj= p1 p5))

      (check-nil (obj= p1 p2))
      (check-nil (obj= p1 p3))
      (check-nil (obj= p2 p4))
      (check-nil (obj= p2 p4)))))

(with-basic-clients (1 2 3)
  (let* ((d0 (basic-depot))
         (d1 (basic-depot 1))
         (c1 (basic-client 1))
         (c2 (basic-client 2))
         (c3 (basic-client 3))
         (p1 (basic-product 1 "p1"))
         (p2 (basic-product 2 "p2"))
         (p3 (basic-product 3 "p3"))
         (pr1 (cupet-problem :id 1 :depot d0
                             :clients (list c1 c2 c3)
                             :products (list p1 p2)
                             :distance-matrix #2A((0 1 2 3)
                                                  (1 0 4 6)
                                                  (2 4 0 2)
                                                  (3 6 2 0))))
         (pr2 (cupet-problem :id 2 :depot d0
                             :clients (list c1 c2 c3)
                             :products (list p1 p2 p3)
                             :distance-matrix #2A((0 1 2 3)
                                                  (1 0 4 6)
                                                  (2 4 0 2)
                                                  (3 6 2 0))))

         (pr3 (clone pr1)))

    (bformat t "Testing")

    (format t "Printing the objects:~%")
    (loop for e in (list pr1 pr2 pr3)
          doing (format t "   cupet-problem with id ~a:~%      ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= pr1 pr1))
      (check-t (obj= pr1 pr3))

      (check-nil (obj= pr1 pr2))
      (check-nil (obj= pr2 pr3)))))

(with-basic-clients (1 2 3)
  (let* ((d0 (basic-depot))
         (d1 (basic-depot 1))
         (p1 (cvrp-problem :id 1 :depot d0
                           :clients (list c1 c2 c3)
                           :capacity 40
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 0))))
         (p2 (cvrp-problem :id 2 :depot d0
                           :clients (list c1 c2 c3)
                           :capacity 40
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 0))))
         (p3 (cvrp-problem :id 1 :depot d0
                           :clients (list c1 c3 c2)
                           :capacity 40
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 0))))
         (p4 (cvrp-problem :id 1 :depot d0
                           :clients (list c1 c2 c3)
                           :capacity 10
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 0))))
         (p5 (cvrp-problem :id 1 :depot d0
                           :clients (list c1 c2 c3)
                           :capacity 10
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 8))))
         (p6 (clone p1)))

    (format t "~%=======================
    Testing cvrp-problem...
    =======================~2%")

    (format t "Printing the objects:~%")
    (loop for e in (list p1 p2 p3 p4 p5 p6)
          doing (format t "   cvrp-problem with id ~a:~% ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= p1 p1))
      (check-t (obj= p1 p6))

      (check-nil (obj= p1 p2))
      (check-nil (obj= p1 p3))
      (check-nil (obj= p1 p4))
      (check-nil (obj= p1 p5)))))

(with-basic-clients (1 2 3)
  (let* ((d0 (basic-depot))
         (v1 (cvrp-vehicle 1 10))
         (v2 (cvrp-vehicle 2 20))
         (v3 (cvrp-vehicle 3 30))
         (v4 (cvrp-vehicle 4 40))
         (p1 (finite-fleet-cvrp-problem :id 1 :depot d0
                           :clients (list c1 c2 c3)
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 0))
                           :fleet (list v1 v2 v3 v4)))
         (p2 (finite-fleet-cvrp-problem :id 2 :depot d0
                           :clients (list c1 c2 c3)
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 0))
                           :fleet (list v1 v2 v3 v4)))
         (p3 (finite-fleet-cvrp-problem :id 1 :depot d0
                           :clients (list c1 c3 c2)
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 0))
                           :fleet (list v1 v2 v3 v4)))
         (p4 (finite-fleet-cvrp-problem :id 1 :depot d0
                           :clients (list c1 c2 c3)
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 0))
                           :fleet (list v1 v2 v3)))
         (p5 (finite-fleet-cvrp-problem :id 1 :depot d0
                           :clients (list c1 c2 c3)
                           :distance-matrix #2A((0 1 2 3)
                                                (1 0 4 6)
                                                (2 4 0 2)
                                                (3 6 2 8))
                           :fleet (list v1 v2 v3 v4)))

         (p6 (clone p1)))

    (bformat t "Testing finite-fleet-cvrp-problem...")

    (format t "Printing the objects:~%")
    (loop for e in (list p1 p2 p3 p4 p5 p6)
          doing (format t "   finite-fleet-cvrp-problem with id ~a:~% ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= p1 p1))
      (check-t (obj= p1 p6))

      (check-nil (obj= p1 p2))
      (check-nil (obj= p1 p3))
      (check-nil (obj= p1 p4))
      (check-nil (obj= p1 p5)))))

(with-basic-clients (1 2 3)
  (let* ((d0 (basic-depot))
         (d1 (basic-depot 5))
         (v1 (cvrp-vehicle 1 10))
         (v2 (cvrp-vehicle 2 20))
         (v3 (cvrp-vehicle 3 30))
         (v4 (cvrp-vehicle 4 40))
         (p1 (finite-fleet-end-depot-cvrp-problem
              :id 1
              :depot d0
              :end-depot d1
              :clients (list c1 c2 c3)
              :distance-matrix #2A((0 1 2 3 4)
                                   (1 0 4 6 1)
                                   (2 4 0 2 3)
                                   (3 6 2 0 2)
                                   (8 2 1 4 0))
              :fleet (list v1 v2 v3 v4)))
         (p2 (finite-fleet-end-depot-cvrp-problem
              :id 2
              :depot d0
              :end-depot d1
              :clients (list c1 c2 c3)
              :distance-matrix #2A((0 1 2 3 4)
                                   (1 0 4 6 1)
                                   (2 4 0 2 3)
                                   (3 6 2 0 2)
                                   (8 2 1 4 0))
              :fleet (list v1 v2 v3 v4)))
         (p3 (finite-fleet-end-depot-cvrp-problem
              :id 1
              :depot d0
              :end-depot d1
              :clients (list c1 c3 c2)
              :distance-matrix #2A((0 1 2 3 4)
                                   (1 0 4 6 1)
                                   (2 4 0 2 3)
                                   (3 6 2 0 2)
                                   (8 2 1 4 0))
              :fleet (list v1 v2 v3 v4)))
         (p4 (finite-fleet-end-depot-cvrp-problem
              :id 1
              :depot d0
              :end-depot d1
              :clients (list c1 c2 c3)
              :distance-matrix #2A((0 1 2 3 4)
                                   (1 0 4 6 1)
                                   (2 4 0 2 3)
                                   (3 6 2 0 2)
                                   (8 2 1 4 0))
              :fleet (list v1 v2 v3)))
         (p5 (finite-fleet-end-depot-cvrp-problem
              :id 1
              :depot d0
              :end-depot d0
              :clients (list c1 c2 c3)
              :distance-matrix #2A((0 1 2 3 4)
                                   (1 0 4 6 1)
                                   (2 4 0 2 3)
                                   (3 6 2 0 2)
                                   (8 2 1 4 0))
              :fleet (list v1 v2 v3 v4)))

         (p6 (clone p1)))

    (bformat t "Testing finite-fleet-with-cvrp-problem...")

    (format t "d0: ~a, d1: ~a~%" d0 d1 )
    (format t "p1: ~a, p5: ~a~%" (end-depot p1) (end-depot p5) )
    (format t "d1 == d0: ~a~%" (obj= (end-depot p1) (end-depot p5)))

    (format t "Printing the objects:~%")
    (loop for e in (list p1 p2 p3 p4 p5 p6)
          doing (format t "   finite-fleet-end-depot-cvrp-problem with id ~a:~% ~a~%"
                        (id e) e))

    (deftests "Testing obj="
      (check-t (obj= p1 p1))
      (check-t (obj= p1 p6))

      (check-nil (obj= p1 p2))
      (check-nil (obj= p1 p3))
      (check-nil (obj= p1 p4))
      (check-nil (obj= p1 p5)))))

(let* ((c1 (route-distance-action 1 10))
       (c2 (route-distance-action 2 10))
       (c3 (route-distance-action 1 20))
       (c4 (clone c1))
       (c5 (clone c2)))
  (format t "~%===============================
Testing route-distance-action...
================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5)
        doing (format t "   route-distance-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c4))
    (check-t (obj= c2 c5))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c2 c4))
    (check-nil (obj= c3 c5))))

(let* ((c1 (basic-vehicle-capacity-action 1 0))
       (c2 (basic-vehicle-capacity-action 2 0))
       (c3 (basic-vehicle-capacity-action 1 20))
       (c4 (clone c1))
       (c5 (clone c2)))
  (format t "~%=======================================
Testing basic-vehicle-capacity-action...
========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5)
        doing (format t "   basic-vehicle-capacity-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c4))
    (check-t (obj= c2 c5))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c2 c4))
    (check-nil (obj= c3 c5))))

(let* ((a (simulate-load-action)))

  (format t "===============================
Testing simulate-load-action...
===============================~2%")

(format t "A simulate-load-action: ~a~%" a))

(let* ((c1 (basic-solution-distance-action ))
       (c2 (basic-solution-distance-action 10))
       (c3 (clone c1))
       (c4 (clone c2)))
  (format t "~%=========================================
Testing basic-solution-distance-action...
=========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4)
        doing (format t "   basic-solution-distance-action ~a:~%" e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c3))
    (check-t (obj= c2 c4))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))))

(let* ((c1 (basic-capacity-penalty-action ))
       (c2 (basic-capacity-penalty-action :id 1 :penalty-factor 100))
       (c3 (basic-capacity-penalty-action :id 1 :capacity-violation 5))
       (c4 (basic-capacity-penalty-action :id 2))
       (c5 (clone c1)))
  (format t "~%=======================================
Testing basic-capacity-penalty-action...
========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5)
        doing (format t "   basic-capacity-penalty-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c5))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))
    (check-nil (obj= c4 c5))
    ))

(let* ((c1 (basic-capacity-penalty-action ))
       (c2 (basic-capacity-penalty-action :id 1 :penalty-factor 100))
       (c3 (basic-capacity-penalty-action :id 1 :capacity-violation 5))
       (c4 (basic-capacity-penalty-action :id 2))
       (c5 (clone c1)))
  (format t "~%=======================================
Testing basic-capacity-penalty-action...
========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5)
        doing (format t "   basic-capacity-penalty-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c5))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))
    (check-nil (obj= c4 c5))
    ))

(let* ((c1 (delta-distance-action ))
       (c2 (delta-distance-action :id 1 :delta-distance 0))
       (c3 (delta-distance-action :id 2 ))
       (c4 (delta-distance-action :id 1 :delta-distance 10))
       (c5 (clone c1)))
  (format t "~%=======================================
Testing delta-distance-action...
========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5)
        doing (format t "   delta-distance-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c5))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))
    (check-nil (obj= c4 c5))))

(let* ((c1 (delta-basic-capacity-action 3))
       (c2 (delta-basic-capacity-action 3 :id 1))
       (c3 (delta-basic-capacity-action
            3 :id 1 :routes-feasibility #(0 0 0 0) ))
       (c4 (delta-basic-capacity-action 4))
       (c5 (delta-basic-capacity-action 3 :id 2))
       (c6 (delta-basic-capacity-action
            3 :id 1 :routes-feasibility #(0 1 0 0)))
       (c7 (clone c1)))
  (format t "~%=======================================
Testing delta-basic-capacity-action...
========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5 c6 c7)
        doing (format t "   delta-basic-capacity-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c2))
    (check-t (obj= c1 c3))
    (check-t (obj= c1 c7))

    (check-nil (obj= c1 c4))
    (check-nil (obj= c1 c5))
    (check-nil (obj= c1 c6))
    (check-nil (obj= c3 c5))))

(let* ((c1 (delta-basic-capacity-penalty-action))
       (c2 (delta-basic-capacity-penalty-action
            :penalty-factor 200))
       (c3 (delta-basic-capacity-penalty-action
            :delta-routes-feasibility #(0 3 0 0)))
       (c4 (delta-basic-capacity-penalty-action
            :original-routes-feasibility #(1 3)))
       (c5 (delta-basic-capacity-penalty-action
            :total-penalty 500))
       (c6 (delta-basic-capacity-penalty-action
            :total-penalty 3
            :id 2
            :penalty-factor 4
            :delta-routes-feasibility #(0 1 0 0)
            :original-routes-feasibility #(0)))
       (c7 (clone c1)))
  (format t "~%=======================================
Testing delta-basic-cvrp-action...
========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5 c6 c7)
        doing (format t "   delta-basic-cvrp-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c7))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c1 c5))
    (check-nil (obj= c1 c6))
    (check-nil (obj= c3 c5))))

(let* ((c1 (delta-cvrp-action))
       (c2 (delta-cvrp-action :delta-distance 1))
       (c3 (delta-cvrp-action :delta-routes-feasibility #(0 0 0 0) ))
       (c3 (delta-cvrp-action
            :original-routes-feasibility #(0 0 0 0) ))
       (c4 (delta-cvrp-action :id 2))
       (c5 (delta-cvrp-action :total-penalty 2))
       (c6 (delta-cvrp-action :penalty-factor 2))
       (c7 (delta-cvrp-action
            :delta-distance 3
            :id 1
            :delta-routes-feasibility #(0 1 0 0)))
       (c8 (clone c1)))
  (format t "~%=======================================
Testing delta-basic-cvrp-action...
========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5 c6 c7)
        doing (format t "   delta-basic-cvrp-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c8))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c1 c5))
    (check-nil (obj= c1 c6))
    (check-nil (obj= c1 c7))
    (check-nil (obj= c3 c5))))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5)
                                    (5 0 6 7 2 )
                                    (1 8 0 3 9)
                                    (4 5 1 0 5)
                                    (4 5 1 5 0))
                       :demands '(20 10 15 40)
                       :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2) (3) (4)) p1)
    (let* ((wc1 (basic-working-copy s1))
           (c1 (delta-cvrp-action
                :delta-distance 3
                :id 1
                :total-penalty 400
                :penalty-factor 300
                :original-routes-feasibility nil
                :delta-routes-feasibility #(0 1 0))))
      (bformat t "Testing delta-basic-cvrp-action initialization")
      (check-= 3 (delta-distance c1))
      (check-obj= #(0 1 0) (delta-routes-feasibility c1))
      (check-obj= #(0 1 2) (delta-routes-feasibility c1))
      (check-nil (original-routes-feasibility c1))
      (check-= 300 (penalty-factor c1))
      (check-= 400 (total-penalty c1))

      ;; let's initialize
      (initialize-action-for-delta-cost-computation wc1 p1 c1)

      ;; after initialization
      (check-= 0 (delta-distance c1))
      (check-obj= #(0 0 0 0) (delta-routes-feasibility c1))
      (check-obj= #(0 10 25 0) (original-routes-feasibility c1))
      (check-= 300 (penalty-factor c1))
      (check-= 0 (total-penalty c1)))))

(let* ((c1 (delta-distance-action* ))
       (c2 (delta-distance-action* :id 1 :delta-distance 0
                                   :delta-distance-stack nil))
       (c3 (delta-distance-action* :id 2))
       (c4 (delta-distance-action* :id 1 :delta-distance 10))
       (c5 (delta-distance-action* :id 1 :delta-distance 0
                                   :delta-distance-stack (list 0)))
       (c6 (clone c1)))
  (bformat t "Testing delta-distance-action")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5)
        doing (format t "   delta-distance-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c2))
    (check-t (obj= c1 c6))

    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c1 c5))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))
    (check-nil (obj= c4 c5))))

(let* ((c1 (delta-basic-capacity-action* 3))
       (c2 (delta-basic-capacity-action* 3 :id 1))
       (c3 (delta-basic-capacity-action*
            3 :id 1 :routes-feasibility #(0 0 0 0) ))
       (c4 (delta-basic-capacity-action*
            3
            :id 1
            :routes-feasibility #(0 0 0 0)
            :delta-routes-feasibility-stack nil))
       (c5 (delta-basic-capacity-action* 4))
       (c6 (delta-basic-capacity-action* 3 :id 2))
       (c7 (delta-basic-capacity-action*
            3
            :id 1
            :routes-feasibility #(0 1 0 0)))
       (c8 (clone c1))
       )
  (format t "~%=======================================
Testing delta-basic-capacity-action...
========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4
                       c5 c6 c7 c8)
        doing (format t "   delta-basic-capacity-action* ~a: ~a~%"
                       (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c2))
    (check-t (obj= c1 c3))
    (check-t (obj= c1 c4))
    (check-t (obj= c1 c8))

    (check-nil (obj= c1 c5))
    (check-nil (obj= c1 c6))
    (check-nil (obj= c3 c5))))

(let* ((c1 (delta-basic-capacity-penalty-action*))
       (c2 (delta-basic-capacity-penalty-action*
            :penalty-factor 200))
       (c3 (delta-basic-capacity-penalty-action*
            :delta-routes-feasibility #(0 3 0 0)))
       (c4 (delta-basic-capacity-penalty-action*
            :original-routes-feasibility #(1 3)))
       (c5 (delta-basic-capacity-penalty-action*
            :total-penalty 500))
       (c6 (delta-basic-capacity-penalty-action*
            :total-penalty 3
            :id 2
            :penalty-factor 4
            :delta-routes-feasibility #(0 1 0 0)
            :original-routes-feasibility #(0)))
       (c7 (clone c1))
       (c8 (delta-basic-capacity-penalty-action*
            :total-penalty 0
            :id 1
            :penalty-factor 1000
            :delta-routes-feasibility nil
            :original-routes-feasibility nil
            :delta-routes-feasibility-stack nil
            :total-penalty-stack nil))
       (c9 (delta-basic-capacity-penalty-action*
            :total-penalty 0
            :id 1
            :penalty-factor 1000
            :delta-routes-feasibility nil
            :original-routes-feasibility nil
            :delta-routes-feasibility-stack `((1 2 0))
            :total-penalty-stack nil))
       (c10 (delta-basic-capacity-penalty-action*
            :total-penalty 0
            :id 1
            :penalty-factor 1000
            :delta-routes-feasibility nil
            :original-routes-feasibility nil
            :delta-routes-feasibility-stack `((1 ,(+ 1 1) 0))
            :total-penalty-stack nil)))
  (bformat t "Testing delta-basic-cvrp-action")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5 c6 c7)
        doing (format t "   delta-basic-cvrp-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c7))
    (check-t (obj= c1 c8))
    (check-t (obj= c9 c10))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c1 c5))
    (check-nil (obj= c1 c6))
    (check-nil (obj= c3 c5))
    (check-nil (obj= c1 c9))))

(let* ((c1 (delta-cvrp-action*))
       (c2 (delta-cvrp-action* :delta-distance 1))
       (c3 (delta-cvrp-action* :delta-routes-feasibility #(0 0 0 0) ))
       (c3 (delta-cvrp-action*
            :original-routes-feasibility #(0 0 0 0) ))
       (c4 (delta-cvrp-action* :id 2))
       (c5 (delta-cvrp-action* :total-penalty 2))
       (c6 (delta-cvrp-action* :penalty-factor 2))
       (c7 (delta-cvrp-action*
            :delta-distance 3
            :id 1
            :delta-routes-feasibility #(0 1 0 0)))
       (c8 (clone c1)))
  (format t "~%=======================================
Testing delta-basic-cvrp-action...
========================================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5 c6 c7)
        doing (format t "   delta-basic-cvrp-action with id ~a: ~a~%"
                      (id e) e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c8))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c1 c5))
    (check-nil (obj= c1 c6))
    (check-nil (obj= c1 c7))
    (check-nil (obj= c3 c5))))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5)
                                    (5 0 6 7 2 )
                                    (1 8 0 3 9)
                                    (4 5 1 0 5)
                                    (4 5 1 5 0))
                       :demands '(20 10 15 40)
                       :capacity 40)
  (with-basic-cvrp-solution (s1 ((1 2) (3) (4)) p1)
    (let* ((wc1 (basic-working-copy s1))
           (c1 (delta-cvrp-action
                :delta-distance 3
                :id 1
                :total-penalty 400
                :penalty-factor 300
                :original-routes-feasibility nil
                :delta-routes-feasibility #(0 1 0))))
      (bformat t "Testing delta-basic-cvrp-action initialization")
      (check-= 3 (delta-distance c1))
      (check-obj= #(0 1 0) (delta-routes-feasibility c1))
      (check-obj= #(0 1 2) (delta-routes-feasibility c1))
      (check-nil (original-routes-feasibility c1))
      (check-= 300 (penalty-factor c1))
      (check-= 400 (total-penalty c1))

      ;; let's initialize
      (initialize-action-for-delta-cost-computation wc1 p1 c1)

      ;; after initialization
      (check-= 0 (delta-distance c1))
      (check-obj= #(0 0 0 0) (delta-routes-feasibility c1))
      (check-obj= #(0 10 25 0) (original-routes-feasibility c1))
      (check-= 300 (penalty-factor c1))
      (check-= 0 (total-penalty c1)))))

(let* ((c1 (op-select-client 1 1 0))
       (c2 (op-select-client 1 1 1))
       (c3 (op-select-client 1 2 0))
       (c4 (op-select-client 2 1 0))
       (c5 (clone c1)))
  (format t "~%===========================
Testing op-select-client...
===========================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5)
        doing (format t "   op-select-client: ~a~%" e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c5))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))
    (check-nil (obj= c4 c5))))

(let* ((c1 (op-insert-client 1 1 0))
       (c2 (op-insert-client 1 1 1))
       (c3 (op-insert-client 1 2 0))
       (c4 (op-insert-client 2 1 0))
       (c5 (clone c1)))
  (format t "~%===========================
Testing op-insert-client...
===========================~2%")

  (format t "Printing the objects:~%")
  (loop for e in (list c1 c2 c3 c4 c5)
        doing (format t "   op-insert-client: ~a~%" e))

  (deftests "Testing obj="
    (check-t (obj= c1 c1))
    (check-t (obj= c1 c5))

    (check-nil (obj= c1 c2))
    (check-nil (obj= c1 c3))
    (check-nil (obj= c1 c4))
    (check-nil (obj= c2 c3))
    (check-nil (obj= c3 c4))
    (check-nil (obj= c4 c5))))

(with-basic-solution (s1 ((1 2 3) (4 5)))
  (with-basic-solution (s2 ((1 2 3) (5 4)))
    (let* ((wc1 (basic-working-copy s1))
           (wc2 (basic-working-copy s2))
           (wc3 (clone wc1))
           (wc4 (clone wc2)))
      (format t "~%==========================
Testing basic-working-copy
==========================~2%")

      (format t "Printing the objects:~%")
      (loop for e in (list wc1 wc2 wc3 wc4)
            doing (format t "   basic-working-copy: ~a~%" e))

      (deftests "Testing obj="
        (check-t (obj= wc1 wc1))
        (check-t (obj= wc1 wc3))
        (check-t (obj= wc2 wc4))

        (check-nil (obj= wc1 wc2))
        (check-nil (obj= wc1 wc4))
        (check-nil (obj= wc2 wc3))))))

(with-basic-solution (s1 ((1 2 3) (4 5)))
  (let* ((wc (basic-working-copy s1)))

    (bformat t "Checking cost in the working-copy")
    (check-= 0 (cost s1))
    (check-= 0 (cost wc))
    (setf (cost s1) 100)
    (check-= 100 (cost wc))
    (setf (cost wc) 200)
    (check-= 200 (cost wc))
    (check-= 200 (cost (solution wc)))))

(with-basic-solution (s1 ((1 2 3) (4 5)))
  (let* ((wc (basic-working-copy s1)))

    (bformat t "Checking depot in the working-copy")
    (check-obj= (basic-depot) (depot wc))))

(with-basic-solution (s1 ((1 2 3) (4 5)))
  (let* ((wc (basic-working-copy s1)))

    (bformat t "Checking depot in the working-copy")
    (check-obj= (basic-depot) (depot wc))))

(with-basic-solution (s1 ((1 2 3) (4 5)))
  (with-basic-solution (s2 ((1 2 3) (5 4)))
    (let* ((wc1 (working-copy-with-infinite-fleet s1))
           (wc2 (working-copy-with-infinite-fleet s2))
           (wc3 (clone wc1))
           (wc4 (clone wc2)))
      (format t "Testing working-copy-with-infinite-fleet")

      (format t "Printing the objects:~%")
      (loop for e in (list wc1 wc2 wc3 wc4)
            doing (format t "   working-copy-with-infinite-fleet: ~a~%" e))

      (deftests "Testing obj="
        (check-t (obj= wc1 wc1))
        (check-t (obj= wc1 wc3))
        (check-t (obj= wc2 wc4))

        (check-nil (obj= wc1 wc2))
        (check-nil (obj= wc1 wc4))
        (check-nil (obj= wc2 wc3))))))

(with-basic-solution (s1 ((1 2 3) (4 5 6)))
  (let* ((wc (make-working-copy s1)))
    (format t "Expect basic-working-copy: ~a~%"
            (type-of wc))))

(with-basic-cvrp-solution (s1 ((1 2 3) (4 5 6)) a-n33-k6-problem )
  (let* ((wc (make-working-copy s1)))
    (format t "What is s1: ~a~%"
            (subtypep (type-of s1) 'has-infinite-fleet))
    (format t "Expect working-copy-with-inifinite-fleet: ~a~%"
            (type-of wc))))

(with-basic-clients (1 2 3 4 5 6)

  (format t "===========================
Testing get-client-with-id:
===========================~2%")
  (let* ((d1 (basic-depot 1))
         (p1 (basic-problem
              :id 1 :clients (list c1 c2 c3 c4 c5 c6) :depot d1)))
    (loop for i from 1 to 6
          doing (format t "With id ~a: ~a~%" i
                        (get-client-with-id i p1)))

    (format t "~%A client that is not there (7): ~a~%"
            (get-client-with-id 7 p1))))

(with-basic-clients (1 2 3 4 5 6)

  (bformat t "Testing get-vehicle-with-id:")
  (let* ((d1 (basic-depot 1))
         (v1 (basic-vehicle 1))
         (v2 (basic-vehicle 2))
         (v3 (basic-vehicle 3))
         (p1 (finite-fleet-cvrp-problem 
              :id 1
              :clients (list c1 c2 c3 c4 c5 c6)
              :depot d1
              :fleet (list v1 v2 v3))))
    (loop for i from 1 to 3
          doing (format t "With id ~a: ~a~%" i
                        (get-vehicle-with-id i p1)))

    (format t "~%A vehicle that is not there (4): ~a~%"
            (get-vehicle-with-id 7 p1))))

(let* ((distance #2A ((0 1 2 3)
                      (1 0 4 5)
                      (2 4 0 6)
                      (3 5 6 0)))
       (dp (make-instance 'distance-problem
                          :distance-matrix distance))
       (d0 (basic-depot))
       (c1 (basic-client 1))
       (c2 (basic-client 2))
       (c3 (basic-client 3)))
  (format t "=============================
Testing get-distance-from-to:
=============================~2%")
  (check-= 0 (get-distance-from-to d0 d0 dp))
  (check-= 1 (get-distance-from-to d0 c1 dp))
  (check-= 2 (get-distance-from-to d0 c2 dp))
  (check-= 3 (get-distance-from-to d0 c3 dp))

  (check-= 1 (get-distance-from-to c1 d0 dp))
  (check-= 0 (get-distance-from-to c1 c1 dp))
  (check-= 4 (get-distance-from-to c1 c2 dp))
  (check-= 5 (get-distance-from-to c1 c3 dp)))

(let* ((p1 (basic-product 1 "oil"))
       (p2 (basic-product 2 "gas"))
       (p3 (basic-product 3 "bread"))
       (p4 (basic-product 4 "meat"))
       (plist (list p1 p2 p3 p4))
       (c1 (make-instance 'multi-product-demand-client
                          :products (list p1 p2 p3 p4))))
  ;; let's initialize the demands
  (loop for p in plist
        for i from 1
        doing (setf (gethash (id p) (products-demand c1)) (* 10 i)))

  (bformat t "Testing products-demand")

  (loop for p in plist
        for i from 1
        doing (check-= (* 10 i) (product-demand p c1))))

(let* ((p1 (basic-product 1 "oil"))
       (p2 (basic-product 2 "gas"))
       (p3 (basic-product 3 "bread"))
       (p4 (basic-product 4 "meat"))
       (plist (list p1 p2 p3 p4))
       (c1 (make-instance 'multi-product-demand-client
                          :products (list p1 p2 p3 p4))))
  ;; let's initialize the demands
  (loop for p in plist
        for i from 1 to 3
        doing (set-product-demand p c1 (* 10 i)))

  (bformat t "Testing set-products-demand")

  (loop for p in plist
        for i from 1 to 3
        doing (check-= (* 10 i) (product-demand p c1)))

  (check-nil (product-demand p4 c1)))

(let* ((p1 (basic-product 1 "oil"))
       (p2 (basic-product 2 "gas"))
       (p3 (basic-product 3 "bread"))
       (p4 (basic-product 4 "meat"))
       (plist (list p1 p2 p3 p4))
       (v1 (make-instance 'multi-compartment-vehicle
                          :number-of-compartments 4)))
  ;; let's initialize the demands
  (loop for p in plist
        for i from 1 to 3
        doing (setf (gethash (id p) (compartments-capacity v1)) (* 10 i)))

  (bformat t "Testing compartment-capacity")

  (loop for p in plist
        for i from 1 to 3
        doing (check-= (* 10 i) (compartment-capacity p v1)))

  (check-nil (compartment-capacity p4 v1)))

(let* ((p1 1)
       (p2 2)
       (p3 3)
       (p4 4)
       (plist (list p1 p2 p3 p4))
       (v1 (make-instance 'multi-compartment-vehicle
                          :number-of-compartments 4)))
  ;; let's initialize the demands
  (loop for p in plist
        for i from 1 to 3
        doing (setf (gethash p (compartments-capacity v1)) (* 10 i)))

  (bformat t "Testing compartment-capacity with numbers")

  (loop for p in plist
        for i from 1 to 3
        doing (check-= (* 10 i) (compartment-capacity p v1)))

  (check-nil (compartment-capacity p4 v1)))

(let* ((p1 1)
       (p2 2)
       (p3 3)
       (p4 4)
       (plist (list p1 p2 p3 p4))
       (v1 (make-instance 'multi-compartment-vehicle
                          :number-of-compartments 4)))
  ;; let's initialize the demands
  (loop for p in plist
        for i from 1 to 3
        doing (setf (gethash p (compartments-capacity v1)) (* 10 i)))

  (bformat t "Testing compartment-capacity with numbers")

  (loop for p in plist
        for i from 1 to 3
        doing (check-= (* 10 i) (compartment-capacity p v1)))

  (check-nil (compartment-capacity p4 v1)))

(let* ((p1 1)
       (p2 2)
       (p3 3)
       (p4 4)
       (plist (list p1 p2 p3 p4))
       (v1 (make-instance 'multi-compartment-vehicle
                          :number-of-compartments 4)))
  ;; let's initialize the demands
  (loop for p in plist
        for i from 1 to 3
        doing (setf (gethash p (compartments-capacity v1)) (* 10 i)))

  (bformat t "Testing compartment-capacity with numbers")

  (loop for p in plist
        for i from 1 to 3
        doing (check-= (* 10 i) (compartment-capacity p v1)))

  (check-nil (compartment-capacity p4 v1)))

(let* ((p1 (basic-product 1 "oil"))
       (p2 (basic-product 2 "gas"))
       (p3 (basic-product 3 "bread"))
       (p4 (basic-product 4 "meat"))
       (plist (list p1 p2 p3 p4))
       (v1 (make-instance 'multi-compartment-vehicle
                          :number-of-compartments 4)))
  ;; let's initialize the demands
  (loop for p in plist
        for i from 1 to 3
        doing (set-compartment-capacity p v1  (* 10 i)))

  (bformat t "Testing set-compartment-capacity")

  (loop for p in plist
        for i from 1 to 3
        doing (check-= (* 10 i) (compartment-capacity p v1)))

  (check-nil (compartment-capacity p4 v1)))

(let* ((p1 1)
       (p2 2)
       (p3 3)
       (p4 4)
       (plist (list p1 p2 p3 p4))
       (v1 (make-instance 'multi-compartment-vehicle
                          :number-of-compartments 4)))
  ;; let's initialize the demands
  (loop for p in plist
        for i from 1 to 3
        doing (setf (gethash p (compartments-capacity v1)) (* 10 i)))

  (bformat t "Testing compartment-capacity with numbers")

  (loop for p in plist
        for i from 1 to 3
        doing (check-= (* 10 i) (compartment-capacity p v1)))

  (check-nil (compartment-capacity p4 v1)))

(let* ((c1 (basic-client 1))
       (c2 (basic-client 2))
       (c3 (basic-client 3))
       (c4 (basic-client 4))
       (v1 (make-instance 'clients-constrained-vehicle
                          :compatible-clients (list c1 c3)))
       (v2 (make-instance 'clients-constrained-vehicle
                          :compatible-clients (list c1 c2 c3))))
  ;; let's initialize the demands


  (bformat t "Testing client-is-compatible-with-vehicle")

  (check-non-nil (client-is-compatible-with-vehicle c1 v1))
  (check-non-nil (client-is-compatible-with-vehicle c3 v1))
  (check-non-nil (client-is-compatible-with-vehicle c1 v2))
  (check-non-nil (client-is-compatible-with-vehicle c2 v2))
  (check-non-nil (client-is-compatible-with-vehicle c3 v2))
  (check-nil     (client-is-compatible-with-vehicle c2 v1)))

(with-basic-solution (s1 ((1 2 3)))
  (let* ((wc (basic-working-copy s1))
         (op1 (op-select-client 1 1 0))
         (op2 (op-insert-client 1 1 1)))
    (check-obj= (list op1) (get-simpler-operations-from op1 wc))
    (check-obj= (list op2) (get-simpler-operations-from op2 wc))))

(progn
  (bformat t "Testing with-basic-clients")
  (with-basic-clients (1 2 3 4 5)
    (format t "  ~a~%" c1)
    (format t "  ~a~%" c2)
    (format t "  ~a~%" c3)
    (format t "  ~a~%" c4)
    (format t "  ~a~%" c5)))

(print (make-basic-route-from-list 1 '(1 2 3) 1))
(print (make-basic-route-from-list 2 '(4 5) 2))
(print (make-basic-route-from-list 3 '(6) 3))
(print (make-basic-route-from-list 3 '(6) 3 2))

(print (make-basic-solution-from-list 1 '((1 2 3) (4 5) (6))))
(print (make-basic-solution-from-list 2 '((1 3) (2 4 5) (6))))

(with-basic-solution (s1 ((1 2 3) (4 5) (6)))
  (format t "~%Id of the solution: ~a" (id s1))
  (print s1))

(with-basic-solution (s3 ((1 2 3) (4 5) (6)) 2)
  (format t "~%Id of the solution: ~a" (id s3))
  (print s3))

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (v1 (cvrp-vehicle 1 50))
       (v2 (cvrp-vehicle 2 50))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 2 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       ;; (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (p1 (make-instance 'cvrp-problem
                          :depot d0
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :capacity 50
                          :distance-matrix distance))
       (action (basic-cvrp-action))
       (*vrp-logging* 0))
  (format t "=======================================
Testing make-basic-cvrp-route-from-list
=======================================~2%")
  (format t "Basic route from (1 2 3):~%  ~a~%"
          (make-basic-cvrp-route-from-list 1 '(1 2 3) 1 p1))
  (check-obj= r1 (make-basic-cvrp-route-from-list 1 '(1 2) 1 p1))
  (check-obj= r2 (make-basic-cvrp-route-from-list 2 '(3 4) 2 p1))
  (check-= 1 1)
  ;; (simulate-solution s1 dp action)
  ;; (check-= 23 (total-distance action))
  ;; (check-= 20 (capacity-violation action))
  ;; (check-= 20000 (total-penalty action))
  )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (v1 (cvrp-vehicle 1 50))
       (v2 (cvrp-vehicle 2 50))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 2 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-cvrp-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (p1 (make-instance 'cvrp-problem
                          :depot d0
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :capacity 50
                          :distance-matrix distance))
       (s2 (make-basic-cvrp-solution-from-list 1 '((1 2) (4 3)) p1))
       (action (basic-cvrp-action))
       (*vrp-logging* 0))

  (bformat t "Testing make-basic-cvrp-solution-from-list")

  (format t "Basic solution from ((1 2) (4 3)):~%~a~%"
          (make-basic-cvrp-solution-from-list 1 '((1 2) (4 3)) p1))

  (check-obj= s1 (make-basic-cvrp-solution-from-list 1 '((1 2) (3 4)) p1))

  (format t "Testing the simulation with the original solution:~%")
  (simulate-solution s1 p1 action)
  (check-= 23 (total-distance action))
  (check-= 40 (capacity-violation action))
  (check-= 40000 (total-penalty action))

  (format t "Testing the simulation with the automatic solution:~%")
  (simulate-solution s2 p1 action)
  (check-= 23 (total-distance action))
  (check-= 40 (capacity-violation action))
  (check-= 40000 (total-penalty action))
  )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (v1 (cvrp-vehicle 1 50))
       (v2 (cvrp-vehicle 2 50))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 2 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (p1 (make-instance 'cvrp-problem
                          :depot d0
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :capacity 50
                          :distance-matrix distance))
       (action (basic-cvrp-action))
       (*vrp-logging* 0))

  (bformat t "Testing make-basic-cvrp-solution-from-list")

  (with-basic-cvrp-solution (s2 ((1 2) (3 4)) p1)
    (format t "Basic solution from ((1 2) (4 3)):~%~a~%"
            s2)

    (format t "type-of S1: ~a~%" (type-of S1))
    (check-obj= s1 s2)

    (format t "Testing the simulation with the original solution:~%")
    (simulate-solution s1 p1 action)
    (check-= 23 (total-distance action))
    (check-= 40 (capacity-violation action))
    (check-= 40000 (total-penalty action))

    (format t "Testing the simulation with the automatic solution:~%")
    (simulate-solution s2 p1 action)
    (check-= 23 (total-distance action))
    (check-= 40 (capacity-violation action))
    (check-= 40000 (total-penalty action))))

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (v1 (cvrp-vehicle 1 50))
       (v2 (cvrp-vehicle 2 10))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 2 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       ;; (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (p1 (make-instance 'finite-fleet-cvrp-problem
                          :depot d0
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :fleet (list v1 v2)
                          :distance-matrix distance))
       (*vrp-logging* 0))
  (bformat t "Testing make-basic-cvrp-route-from-list")
  (format t "Basic route from (1 2 3):~%  ~a~%"
          (make-basic-cvrp-route-from-list 1 '(1 2 3) 2 p1))
  (check-obj= r1 (make-finite-fleet-cvrp-route-from-list 1 '(1 2) 1 p1))
  (check-obj= r2 (make-finite-fleet-cvrp-route-from-list 2 '(3 4) 2 p1))
  )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (v1 (cvrp-vehicle 1 50))
       (v2 (cvrp-vehicle 2 50))
       (r1 (route-for-simulation :id 1 :vehicle v2
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 2 :vehicle v1
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (p1 (make-instance 'finite-fleet-cvrp-problem
                          :depot d0
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :fleet (list v1 v2)
                          :distance-matrix distance))
       (s2 (make-finite-fleet-cvrp-solution-from-list
            1 '((2 1 2) (1 4 3)) p1))
       (action (basic-cvrp-action))
       (*vrp-logging* 0))

  (bformat t "Testing make-basic-cvrp-solution-from-list")

  (format t "Basic solution from ((1 2) (4 3)):~%~a~%"
          (make-finite-fleet-cvrp-solution-from-list
           1 '((2 1 2) (1 3 4)) p1))

  (check-obj= s1 (make-finite-fleet-cvrp-solution-from-list
                  1 '((2 1 2) (1 3 4)) p1))

  (format t "Testing the simulation with the original solution:~%")
  (simulate-solution s1 p1 action)
  (check-= 23 (total-distance action))
  (check-= 40 (capacity-violation action))
  (check-= 40000 (total-penalty action))

  ;; (format t "Testing the simulation with the automatic solution:~%")
  ;; (simulate-solution s2 p1 action)
  ;; (check-= 23 (total-distance action))
  ;; (check-= 40 (capacity-violation action))
  ;; (check-= 40000 (total-penalty action))
  )

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (v1 (cvrp-vehicle 1 50))
       (v2 (cvrp-vehicle 2 50))
       (r1 (route-for-simulation :id 1 :vehicle v1
                                 :depot d0 :clients (list c1 c2)))
       (r2 (route-for-simulation :id 2 :vehicle v2
                                 :depot d0 :clients (list c3 c4)))
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (distance #2A ((0 1 2 3 5)
                      (1 0 4 5 6)
                      (2 4 0 6 7)
                      (3 5 6 0 8)
                      (5 6 7 8 0)))
       (p1 (make-instance 'finite-fleet-cvrp-problem
                          :depot d0
                          :id 1
                          :clients (list c1 c2 c3 c4)
                          :fleet (list v1 v2)
                          :distance-matrix distance))
       (action (basic-cvrp-action))
       (*vrp-logging* 0))

  (bformat t "Testing make-basic-cvrp-solution-from-list")

  (with-finite-fleet-cvrp-solution (s2 ((1 1 2) (2 3 4)) p1)
    (format t "Basic solution from ((1 1 2) (2 3 4)):~%~a~%"
            s2)

    (format t "type-of S1: ~a~%" (type-of S1))
    (format t "type-of p1: ~a~%" (type-of p1))
    (check-obj= s1 s2)

    (format t "Testing the simulation with the original solution:~%")
    (simulate-solution s1 p1 action)
    (check-= 23 (total-distance action))
    (check-= 40 (capacity-violation action))
    (check-= 40000 (total-penalty action))

    (format t "Testing the simulation with the automatic solution:~%")
    (simulate-solution s2 p1 action)
    (check-= 23 (total-distance action))
    (check-= 40 (capacity-violation action))
    (check-= 40000 (total-penalty action))
    ))

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (v1 (cvrp-vehicle 1 50))
       (v2 (cvrp-vehicle 2 10))
       (r1 (route-for-simulation-with-end-depot
            :id 1
            :vehicle v1
            :depot d0
            :end-depot d1
            :clients (list c1 c2)))
       (r2 (route-for-simulation-with-end-depot
            :id 2
            :vehicle v2
            :depot d0
            :end-depot d1
            :clients (list c3 c4)))

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
       (*vrp-logging* 0))
  (bformat t "Testing make-basic-cvrp-")
  (format t "Basic route with end-depot from (1 2 3):~%  ~a~%"
          (make-finite-fleet-with-end-depot-route-cvrp-from-list
           1 '(1 2 3) 2 p1))

  (check-obj= r1 (make-finite-fleet-with-end-depot-route-cvrp-from-list
                  1 '(1 2) 1 p1))
  (check-obj= r2 (make-finite-fleet-with-end-depot-route-cvrp-from-list
                  2 '(3 4) 2 p1)))

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (v1 (cvrp-vehicle 1 50))
       (v2 (cvrp-vehicle 2 10))
       (r1 (route-for-simulation-with-end-depot
            :id 1
            :vehicle v1
            :depot d0
            :end-depot d1
            :clients (list c1 c2)))
       (r2 (route-for-simulation-with-end-depot
            :id 2
            :vehicle v2
            :depot d0
            :end-depot d1
            :clients (list c3 c4)))

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
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (*vrp-logging* 0))
  (bformat t "Testing make-finite-fleet-with-end-depot-route-cvrp-from-list")
  (format t "Basic solution with end-depot from ((1 1 2) (2 3 4)):~%  ~a~%"
          (make-finite-fleet-with-end-depot-cvrp-solution-from-list
           1 '((1 1 2) (2 3 4)) p1))

  (check-obj= s1 (make-finite-fleet-with-end-depot-cvrp-solution-from-list
                   1 '((1 1 2) (2 3 4)) p1)))

(let* ((*vrp-unit-testing-display-output* nil)
       (*vrp-unit-testing-display-results* t)
       (c1 (basic-cvrp-client 1 30))
       (c2 (basic-cvrp-client 2 20))
       (c3 (basic-cvrp-client 3 40))
       (c4 (basic-cvrp-client 4 50))
       (d0 (basic-depot))
       (d1 (basic-depot 5))
       (v1 (cvrp-vehicle 1 50))
       (v2 (cvrp-vehicle 2 10))
       (r1 (route-for-simulation-with-end-depot
            :id 1
            :vehicle v1
            :depot d0
            :end-depot d1
            :clients (list c1 c2)))
       (r2 (route-for-simulation-with-end-depot
            :id 2
            :vehicle v2
            :depot d0
            :end-depot d1
            :clients (list c3 c4)))

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
       (s1 (basic-solution :id 1 :routes (list r1 r2)))

       (action (basic-cvrp-action))

       (*vrp-logging* 0))
  (bformat t "Testing with-finite-fleet-with-end-depot-solution")

  (with-finite-fleet-end-depot-cvrp-solution (s2 ((1 1 2) (2 3 4)) p1)

    (format t "Basic solution with the macro:~%  ~a~%" s2)

    (check-t (obj= s1 s2))

    (format t "Checking the evaluation of the solution s1:~%")
    (simulate-solution s1 p1 action)
    (format t "distance: ~a~%" (total-distance action))
    (format t "penalty: ~a~%" (total-penalty action))


    (format t "~%Checking the evaluation of the solution s2:~%")
    (setf action (basic-cvrp-action))
    (simulate-solution s2 p1 action)
    (format t "distance: ~a~%" (total-distance action))
    (format t "penalty: ~a~%" (total-penalty action))

    ))

(progn

  (bformat t "Testing make-demand-clients-from-demands")

  (format t "Clients: ~a~%"
          (make-demand-clients-from-demands '(1 2 3 4 5 6)))
  (format t "Clients: ~a~%"
          (make-demand-clients-from-demands '(10 20 30 40 50 60)))
  (format t "Clients: ~a~%"
          (make-demand-clients-from-demands '(10 30 20 50 40 60))))

(let* ((p1 (make-cvrp-from-lists `((0 2 3 4 5)
                                   (5 0 6 7 6)
                                   (1 8 0 9 8)
                                   (4 5 1 0 6)
                                   (1 2 3 4 0))
                                  `(1 3 5 2)
                                  6)))
  (print p1)
  (print (distance-matrix p1))
  (print (id p1))
  (print (clients p1)))

(with-cvrp-problem (p1 :distances `((0 2 3 4 5)
                                     (5 0 6 7 2 )
                                     (1 8 0 3 9)
                                     (4 5 1 0 5)
                                     (4 5 1 5 0))
                        :demands '(1 3 5 2)
                        :capacity 6)
 (print p1)
 (print (distance-matrix p1))
 (print (id p1))
 (print (clients p1)))

(progn

  (bformat t "Testing make-demand-clients-from-demands")

  (format t "Vehicles: ~a~%"
          (make-cvrp-vehicles-list-from-capacities
           '(1 2 3 4 5 6)))
  (format t "Vehicles: ~a~%"
          (make-cvrp-vehicles-list-from-capacities
           '(10 20 30 40 50 60)))
  (format t "Vehicles: ~a~%"
          (make-cvrp-vehicles-list-from-capacities
           '(10 30 20 50 40 60))))

(let* ((p1 (make-finite-fleet-cvrp-from-lists `((0 2 3 4 5)
                                                (5 0 6 7 6)
                                                (1 8 0 9 8)
                                                (4 5 1 0 6)
                                                (1 2 3 4 0))
                                              `(1 3 5 2)
                                              `(1 2 5 4))))
  (format t "Finite-Fleet-Problem: ~a~%" p1)
  (format t "distance matrix: ~a~%" (distance-matrix p1))
  (format t "id: ~a~%" (id p1))
  (format t "clients: ~a~%" (clients p1))
  (format t "vehicles: ~a~%" (fleet p1)))

(with-finite-fleet-cvrp-problem (p1 :distances `((0 2 3 4 5)
                                                 (5 0 6 7 2 )
                                                 (1 8 0 3 9)
                                                 (4 5 1 0 5)
                                                 (4 5 1 5 0))
                                    :demands '(1 3 5 2)
                                    :capacities '(4 5 3))

  (bformat t "Testing with-finite-fleet-cvrp-problem...")

  (format t "Finite-Fleet-Problem: ~a~%" p1)
  (format t "distance matrix: ~a~%" (distance-matrix p1))
  (format t "id: ~a~%" (id p1))
  (format t "clients: ~a~%" (clients p1))
  (format t "vehicles: ~a~%" (fleet p1)))

(let* ((p1 (make-finite-fleet-end-cvrp-from-lists `((0 2 3 4 5 6)
                                                    (5 0 6 7 8 9)
                                                    (1 8 0 9 1 2)
                                                    (4 5 1 0 3 4)
                                                    (4 3 9 2 0 2)
                                                    (8 1 2 5 2 0))
                                                  `(1 3 5 2)
                                                  `(2 5 8))))
  (format t "finite-fleet-end-depot-cvrp-problem: ~a~%" p1)

  (format t "distance matrix: ~a~3%" (distance-matrix p1))
  (format t "id:        ~a~%" (id p1))
  (format t "clients:   ~a~%" (clients p1))
  (format t "vehicles:  ~a~%" (fleet p1))
  (format t "depot:     ~a~%" (depot p1))
  (format t "end-depot: ~a~%" (end-depot p1)))

(with-finite-fleet-end-depot-cvrp-problem
    (p1 :distances `((0 2 3 4 5)
                     (5 0 6 7 8)
                     (1 8 0 9 1)
                     (4 5 1 0 3)
                     (8 1 2 5 2))
         :demands '(1 3 2)
         :capacities '(4 5 3))

  (format t "finite-fleet-end-depot-cvrp-problem: ~a~%" p1)

  (format t "distance matrix: ~a~3%" (distance-matrix p1))
  (format t "id:        ~a~%" (id p1))
  (format t "clients:   ~a~%" (clients p1))
  (format t "vehicles:  ~a~%" (fleet p1))
  (format t "depot:     ~a~%" (depot p1))
  (format t "end-depot: ~a~%" (end-depot p1)))
