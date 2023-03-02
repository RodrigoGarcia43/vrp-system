(in-package :vrp)

(defparameter *search-strategies-for-classical-criteria*
  `((best +best-improvement+)
    (first +first-improvement+)
    (random +random-improvement+))
  "A list with the search strategies that can be used to automatically create classical neighboorhood criteria.  The first element is a symbol to append to the name of the criterion, and the second is a symbol whose symbol-value should be an instance of a select-strategy class.")

(defmacro make-classical-criterion
    (name description
     &key
       (strategies *search-strategies-for-classical-criteria*))
  ;; (declare (ignore strategies))
  (let* ((criterion-description name))
    ;; let's create the description
    `(progn
       (defparameter ,criterion-description ',description)
       ;; now we add one criterion for each strategy
       ,@(loop for (strat instance) in strategies
               collecting `
               (defparameter ,(symb name "-" strat)
                 (make-neighborhood-criterion
                  ,criterion-description
                  +exhaustive-search-strategy+
                  ,instance)))
       )))

(defgeneric clone-working-copy-as-solution (working-copy)
  (:documentation "Receives a working-copy and returns a new basic-solution that represents the current state of the working-copy."))

(defmethod clone-working-copy-as-solution ((wc basic-working-copy))
  (let* ((solution (solution wc))
         (new-solution (basic-solution :id (id solution)))
         (r1 (first (routes solution)))
         (d1 (depot r1))
         (number-of-routes (number-of-routes wc)))

    ;; let's create the routes
    (loop for r-id from 1 to number-of-routes
          do (progn
               (push
                (basic-route :id r-id
                             :vehicle (vehicle
                                       (nth (1- r-id)
                                            (routes solution)))
                             :depot d1
                             :clients
                             (loop for c from 1 to (route-length r-id wc)
                                   collect (clone (client-at
                                                   (list r-id c)
                                                   wc))))
                (routes new-solution))))

    ;; let's reverse the routes
    (setf (routes new-solution) (reverse (routes new-solution)))

    ;; let's set the cost
    (setf (cost new-solution) (cost solution))

    ;; return the new-solution
    new-solution))

(defmacro explore-neighborhood-criteria-v2 (criterion
                                            &key
                                              smart-criterion
                                              smart-version-info
                                              name-for-bformat
                                              print-smart-repeated
                                              print-intersection
                                              show-intersection
                                              print-difference
                                              print-smart-neighborhood
                                              print-neighborhood
                                              print-smart-intersection
                                              (solution `((1 2 3 4))))

  (let* (
         ;; here we write the code for the analysis of the
         ;; criterion
         (code-for-the-criterion
          `(progn
             ;; here we create a new solution from a working-copy
             (setf current-neighbor
                   (clone-working-copy-as-solution wc))
             ;; (loop for r from 1 to (number-of-routes wc)
             ;;       do (loop for c from 1 to (route-length r wc)
             ;;                collect (clone
             ;;                         (client-at (list r i) wc))))


             ;; let's see how many repeated are there
             (if (member current-neighbor all-neighborhood
                         :test 'obj=)
                 ;; then
                 (pushnew current-neighbor repeated-neighbors
                          :test 'obj=)
                 ;; else
                 (push current-neighbor once-only-neigbors))

             (push current-neighbor all-neighborhood)))
         ;; this is the code for the criterion
         ;; with the macro headings
         (code-with-headings-for-criterion
          (append (car (last criterion))
                  (list code-for-the-criterion)))
         ;; here is the code with the instructions for
         ;; the smart version
         (smart-version-format-str
          ;; if we want to print some info,
          ;; first, let's build the format string
          (if smart-version-info
              (with-output-to-string (s)
                (format s "[~~a")
                (loop for i in (rest smart-version-info)
                      do (format s " |~~a"))
                (format s "]"))))
         ;; here we write how we build the
         ;; current-neighbor representation
         (build-solution-to-store-code
          `(setf current-neighbor
                   (clone-working-copy-as-solution wc)))
         ;; the code which defines what info
         ;; we should store about the current-neighbor
         (initial-code-for-the-smart-version
          (if smart-version-info
              (list
               build-solution-to-store-code
               ;; let's get the info from this neighbor
               `(setf info-to-print
                      (format nil ,smart-version-format-str
                              ,@smart-version-info)))
              (list
               build-solution-to-store-code
               `(setf info-to-print ""))))
         ;; and this is the code we always execute
         ;; to count the repeated neighbors and so
         (rest-of-the-code-to-store
          `(
            ;; let's see if the current neighbor has already
            ;; been visited
            (setf membership-info
                  (first (member current-neighbor smart-neighborhood
                                 :test (lambda (x y) (obj= x (first y))))))

            ;; now in membership-info we have a list
            ;; where the 1st element is the neighbor and the
            ;; 2nd is the "coords" from the moment it was added
            ;; if it is nil, the neighbor is new.

            (if membership-info 

                ;; then it is repeated, so let's add it
                ;; to the repeated ones
                ;; but before we need to know if it was already
                ;; added to the repeated ones
                (progn
                  (setf repeated-info
                        (first (member current-neighbor
                                       smart-repeated-neighbors
                                       :test (lambda (x y)
                                               (obj= x (first y))))))
                  ;; if repeated-info is non nil, the it has all the
                  ;; info we need about the repeated-neighbor.
                  ;; AND we need to remove it from the
                  ;; smart-repeated-neighbors
                  (if repeated-info
                      (then
                        (setf smart-repeated-neighbors
                              (remove current-neighbor
                                      smart-repeated-neighbors
                                      :test (lambda (x y)
                                              (obj= x (first y))))))
                      (else
                        ;; let's set it to an appropriate value
                        ;; in this case, to membership-info
                        (setf repeated-info
                              membership-info)))

                  ;; now we should add the current-neighbor to
                  ;; the list of repeated-neighbors.
                  (push (append
                         repeated-info
                         (list info-to-print))
                        smart-repeated-neighbors))
                ;; else
                (push current-neighbor smart-once-only-neigbors))

            ;; in any case
            (push (list current-neighbor
                        info-to-print)
                  smart-neighborhood)))
         ;; here we append everything in a single
         ;; block of code
         (code-to-use-in-the-smart-version
          (append
           initial-code-for-the-smart-version
           rest-of-the-code-to-store))
         ;; here we add the code for the smart
         ;; version with the headings included
         (code-with-headings-for-smart-version
          (append (car (last smart-criterion))
                  code-to-use-in-the-smart-version))
         ;; and this is the code we use to print
         ;; the info we want
         (code-to-print-the-info
          (if criterion
              `((progn
                  (format t "neighborhood size:        ~a~%"
                          (length all-neighborhood))
                  (format t "repeated elements:        ~a~%"
                          (length repeated-neighbors))
                  (format t "once-only elements:       ~a~%"
                          (length once-only-neigbors))))))
         ;; here I'll add the code to print info
         ;; about the repeated elements in the
         ;; smart version
         ;; first, a way to deal with several formats
         ;; to print the info
         (what-to-print-in-the-repeated
          (cond
            ((eql print-smart-repeated 'all)
             `(" ~a~%~{   ~a~%~}~%"
               (first repeated)
               (rest repeated)))
            (t `(" ~a~%"
                 (first repeated)))))
         ;; and here is the actual code to print
         (code-to-print-the-smart-repeated-neighbors
          `((if (and ',print-smart-repeated
                     (> (length smart-repeated-neighbors) 0))
                (progn
                  (format t "Repeated elements (~a):~%"
                          (length smart-repeated-neighbors))
                  (loop for repeated in smart-repeated-neighbors
                        do (format t ,(first what-to-print-in-the-repeated)
                                   ,@(rest what-to-print-in-the-repeated)
                                   ))))))
         ;; Now I'll add the code to print the intersection
         ;; of the criterion and the smart version
         ;; (what-to-print-in-the-intersection
         ;;  (cond
         ;;    ((eql print-intersection 'all)
         ;;     `(" ~a~%~{   ~a~%~}~%"
         ;;       (first i)
         ;;       (rest i)))
         ;;    (t `(" ~a~%"
         ;;         i))))
         ;; and here is the actual code to print
         ;; the intersection
         (code-to-print-the-intersection
          `((if (and (or ',print-intersection
                         ',show-intersection
                         ',print-smart-intersection)
                     ',smart-criterion
                     ',criterion)
                (progn
                  (setf intersection
                        (intersection once-only-neigbors
                                      smart-once-only-neigbors
                                      :test (lambda (x y)
                                              (obj= x y))
                                      ))))
            ;; here we display the actual info
            ;; first, only the data
            (if ',show-intersection
                (format t "Elements in the intersection: ~a~%"
                        (length intersection)))
            ;; now, if the user asked for it,
            ;; we print the intersection 
            (if (and ',print-intersection
                     (> (length intersection) 0))
                (then
                  (loop for i in intersection
                        do (progn
                             (pp-solution i t) (terpri)))))))
         ;; here we'll write the code for the difference
         ;; the difference will always be
         ;; original minus smart
         (what-to-print-in-the-difference
          (cond
            ((eql print-difference 'all)
             `(" ~a~%"
               d))
            (t `(" ~a~%"
                 d))))
         ;; and here is the code to actually print
         ;; the difference
         (code-to-print-the-difference
          `((if (and ',print-difference
                     ',smart-criterion
                     ',criterion)
                (setf difference
                      (set-difference once-only-neigbors
                                      (mapcar (lambda (x)
                                                (first x))
                                              smart-neighborhood)
                                      :test #'obj=
                                      )))
            (if (and ',print-difference
                     (> (length difference) 0))
                (progn
                  (format t "Missing elements: ~a~%"
                          (length difference))
                  (loop for d in difference
                        do (format t ,(first what-to-print-in-the-difference)
                                   ,@(rest what-to-print-in-the-difference)
                                   ))
                  )
                ;; else
                (if ',print-difference
                    (format t "There are not missing elements.~%")))))
         ;; here I'll add the code to print the
         ;; smart neighborhood
         (code-to-print-the-smart-neighborhood
          `((if (and ',print-smart-neighborhood)
                (progn
                  (format t "Smart-neighborhood (~a):~%"
                          (length smart-neighborhood))
                  (loop for elt in (reverse smart-neighborhood)
                        do (progn
                             (pp-solution (first elt) t)
                             (if (eql ',print-smart-neighborhood 'all)
                                 (format t "~{ ~a~%~}"
                                         (rest elt))))

                        ;; (terpri)
                        )))))
         (code-to-print-the-original-neighborhood
          `((if (and ',print-neighborhood)
                (progn
                  (format t "Original-neighborhood (~a):~%"
                          (length all-neighborhood))
                  (loop for elt in (reverse all-neighborhood)
                        do (pp-solution elt t)
                        ;; (terpri)
                        )))))
         )


    ;; let's build the code with what to do
    ;; inside the criterion
    (loop for h in (reverse (butlast criterion))
          doing (setf code-with-headings-for-criterion
                      (append h (list code-with-headings-for-criterion))))

    ;; if we have a smart version, let's build the code
    (loop for h in (reverse (butlast smart-criterion))
          do (setf code-with-headings-for-smart-version
                   (append h (list code-with-headings-for-smart-version))))

    ;; let's write the code to print the info
    (setf code-to-print-the-info
          (append code-to-print-the-info
                  (if smart-criterion
                      `((progn
                          (format t "smart neighborhood size:  ~a~%"
                                  (length smart-neighborhood))
                          (format t "smart repeated elements:  ~a~%"
                                  (length smart-repeated-neighbors))
                          (format t "smart once-only elements: ~a~%"
                                  (length smart-once-only-neigbors))
                          )))))



   `(with-basic-solution (s1 ,solution)

      (bformat t "Exploring neighborhood ~a"
               ,name-for-bformat)

      ;; (format t "code-to-use-in-the-smart-version: ~a~%"
      ;;         ',code-to-use-in-the-smart-version)

      ;; (format t "inner code for the smart version: ~a~%"
      ;;         ',inner-code-for-the-smart-version)

      (let* ((wc (basic-working-copy s1))
             (ops-list nil)
             (current-neighbor nil)
             (all-neighborhood nil)
             (repeated-neighbors nil)
             (once-only-neigbors nil)
             (smart-neighborhood nil)
             (smart-repeated-neighbors nil)
             (smart-once-only-neigbors nil)
             (smart-intersection nil)
             (repeated-info nil)
             (intersection nil)
             (difference nil)
             (membership-info nil)
             (info-to-print nil)
             )

        (prepare-solution-for-neighborhood-exploration wc)

        ;; here we execute the criterion with the code

        ,code-with-headings-for-criterion

        (prepare-solution-for-neighborhood-exploration wc)
        ;; now let's test the "smart" version
        ,code-with-headings-for-smart-version

        ,@code-to-print-the-info

        ,@code-to-print-the-smart-repeated-neighbors

        ,@code-to-print-the-intersection

        ,@code-to-print-the-difference

        ,@code-to-print-the-smart-neighborhood

        ,@code-to-print-the-original-neighborhood

        (if (and ,print-smart-intersection
                 intersection)
            ;; let's compute the smart intersection
            (progn
              (loop for elt in intersection
                    doing (push (first (member elt smart-neighborhood
                                               :test (lambda (x y)
                                                       (obj= x
                                                             (first y)))))
                                smart-intersection))
              ;; and now let's print it:
              (format t "Elements in the (smart) intersection: ~a~%~{~a~%~%~}"
                      (length smart-intersection)
                      (reverse smart-intersection))))

        ;; (format t "repeated neighborhood:~%~a~%"
        ;;         smart-neighborhood)

        ;; (format t "intersection:~%~{~a~%~}~%"
        ;;         (intersection all-neighborhood
        ;;                             smart-neighborhood
        ;;                             :test (lambda (x y)
        ;;                                     (obj= x (first y)))
        ;;                             ))

        ))))

(defmacro explore-neighborhood-criteria (criterion
                                         &key
                                           smart-criterion
                                           smart-version-info
                                           name-for-bformat
                                           print-smart-repeated
                                           print-intersection
                                           print-difference
                                           (solution `((1 2 3 4))))

  (let* (
         ;; here we write the code for the analysis of the
         ;; criterion
         (code-for-the-criterion
          `(progn
             (setf current-neighbor
                   (loop for i from 1 to (route-length 1 wc)
                         collecting (id (client-at (list 1 i) wc))))

             ;; let's see how many repeated are there
             (if (member current-neighbor all-neighborhood
                         :test 'obj=)
                 ;; then
                 (pushnew current-neighbor repeated-neighbors
                          :test 'obj=)
                 ;; else
                 (push current-neighbor once-only-neigbors))

             (push current-neighbor all-neighborhood)))
         ;; this is the code for the criterion
         ;; with the macro headings
         (code-with-headings-for-criterion
          (append (car (last criterion))
                  (list code-for-the-criterion)))
         ;; here is the code with the instructions for
         ;; the smart version
         (smart-version-format-str
          ;; if we want to print some info,
          ;; first, let's build the format string
          (if smart-version-info
              (with-output-to-string (s)
                (format s "[~~a")
                (loop for i in (rest smart-version-info)
                      do (format s " |~~a"))
                (format s "]"))))
         ;; here we write how we build the
         ;; current-neighbor representation
         (build-solution-to-store-code
          `(setf current-neighbor
                 (loop for i from 1 to (route-length 1 wc)
                       collecting (id (client-at (list 1 i) wc)))))
         ;; the code which defines what info
         ;; we should store about the current-neighbor
         (initial-code-for-the-smart-version
          (if smart-version-info
              (list
               build-solution-to-store-code
               ;; let's get the info from this neighbor
               `(setf info-to-print
                      (format nil ,smart-version-format-str
                              ,@smart-version-info)))
              (list
               build-solution-to-store-code)))
         ;; and this is the code we always execute
         ;; to count the repeated neighbors and so
         (rest-of-the-code-to-store
          `(
            ;; let's see if the current neighbor has already
            ;; been visited
            (setf membership-info
                  (first (member current-neighbor smart-neighborhood
                                 :test (lambda (x y) (obj= x (first y))))))

            ;; now in membership-info we have a list
            ;; where the 1st element is the neighbor and the
            ;; 2nd is the "coords" from the moment it was added
            ;; if it is nil, the neighbor is new.

            (if membership-info 

                ;; then it is repeated, so let's add it
                ;; to the repeated ones
                ;; but before we need to know if it was already
                ;; added to the repeated ones
                (progn
                  (setf repeated-info
                        (first (member current-neighbor
                                       smart-repeated-neighbors
                                       :test (lambda (x y)
                                               (obj= x (first y))))))
                  ;; if repeated-info is non nil, the it has all the
                  ;; info we need about the repeated-neighbor.
                  ;; AND we need to remove it from the
                  ;; smart-repeated-neighbors
                  (if repeated-info
                      (then
                        (setf smart-repeated-neighbors
                              (remove current-neighbor
                                      smart-repeated-neighbors
                                      :test (lambda (x y)
                                              (obj= x (first y))))))
                      (else
                        ;; let's set it to an appropriate value
                        ;; in this case, to membership-info
                        (setf repeated-info
                              membership-info)))

                  ;; now we should add the current-neighbor to
                  ;; the list of repeated-neighbors.
                  (push (append
                         repeated-info
                         (list info-to-print))
                        smart-repeated-neighbors))
                ;; else
                (push current-neighbor smart-once-only-neigbors))

            ;; in any case
            (push (list current-neighbor
                        info-to-print)
                  smart-neighborhood)))
         ;; here we append everything in a single
         ;; block of code
         (code-to-use-in-the-smart-version
          (append
           initial-code-for-the-smart-version
           rest-of-the-code-to-store))
         ;; here we add the code for the smart
         ;; version with the headings included
         (code-with-headings-for-smart-version
          (append (car (last smart-criterion))
                  code-to-use-in-the-smart-version))
         ;; and this is the code we use to print
         ;; the info we want
         (code-to-print-the-info
          (if criterion
              `((progn
                  (format t "neighborhood size:        ~a~%"
                          (length all-neighborhood))
                  (format t "repeated elements:        ~a~%"
                          (length repeated-neighbors))
                  (format t "once-only elements:       ~a~%"
                          (length once-only-neigbors))))))
         ;; here I'll add the code to print info
         ;; about the repeated elements in the
         ;; smart version
         ;; first, a way to deal with several formats
         ;; to print the info
         (what-to-print-in-the-repeated
          (cond
            ((eql print-smart-repeated 'all)
             `(" ~a~%~{   ~a~%~}~%"
               (first repeated)
               (rest repeated)))
            (t `(" ~a~%"
                 (first repeated)))))
         ;; and here is the actual code to print
         (code-to-print-the-smart-repeated-neighbors
          `((if (and ',print-smart-repeated
                     (> (length smart-repeated-neighbors) 0))
                (progn
                  (format t "Repeated elements (~a):~%"
                          (length smart-repeated-neighbors))
                  (loop for repeated in smart-repeated-neighbors
                        do (format t ,(first what-to-print-in-the-repeated)
                                   ,@(rest what-to-print-in-the-repeated)
                                   ))
                  ))))
         ;; Now I'll add the code to print the intersection
         ;; of the criterion and the smart version
         (what-to-print-in-the-intersection
          (cond
            ((eql print-intersection 'all)
             `(" ~a~%~{   ~a~%~}~%"
               (first repeated)
               (rest repeated)))
            (t `(" ~a~%"
                 (first repeated)))))
         ;; and here is the actual code to print
         ;; the intersection
         (code-to-print-the-intersection
          `((if (and ',print-intersection
                     ',smart-criterion
                     ',criterion)
                (setf intersection
                      (intersection all-neighborhood
                                    smart-neighborhood
                                    :test (lambda (x y)
                                            (obj= x (first y)))
                                    )))
            (if (and ',print-intersection
                     (> (length intersection) 0))
                (progn
                  (format t "Elements in the intersection: ~a~%"
                          (length smart-repeated-neighbors))
                  (loop for repeated in smart-repeated-neighbors
                        do (format t ,(first what-to-print-in-the-intersection)
                                   ,@(rest what-to-print-in-the-intersection)
                                   ))
                  )
                ;; else
                (if ',print-intersection
                    (format t "Intersection is empty.~%")))))
         ;; here we'll write the code for the difference
         ;; the difference will always be
         ;; original minus smart
         (what-to-print-in-the-difference
          (cond
            ((eql print-difference 'all)
             `(" ~a~%"
               d))
            (t `(" ~a~%"
                 d))))
         ;; and here is the code to actually print
         ;; the difference
         (code-to-print-the-difference
          `((if (and ',print-difference
                     ',smart-criterion
                     ',criterion)
                (setf difference
                      (set-difference once-only-neigbors
                                      (mapcar (lambda (x)
                                                (first x))
                                              smart-neighborhood)
                                      :test #'obj=
                                      )))
            (if (and ',print-difference
                     (> (length difference) 0))
                (progn
                  (format t "Missing elements: ~a~%"
                          (length difference))
                  (loop for d in difference
                        do (format t ,(first what-to-print-in-the-difference)
                                   ,@(rest what-to-print-in-the-difference)
                                   ))
                  )
                ;; else
                (if ',print-difference
                    (format t "There are not missing elements.~%")))))

         )


    ;; let's build the code with what to do
    ;; inside the criterion
    (loop for h in (reverse (butlast criterion))
          doing (setf code-with-headings-for-criterion
                      (append h (list code-with-headings-for-criterion))))

    ;; if we have a smart version, let's build the code
    (loop for h in (reverse (butlast smart-criterion))
          do (setf code-with-headings-for-smart-version
                   (append h (list code-with-headings-for-smart-version))))

    ;; let's write the code to print the info
    (setf code-to-print-the-info
          (append code-to-print-the-info
                  (if smart-criterion
                      `((progn
                          (format t "smart neighborhood size:  ~a~%"
                                  (length smart-neighborhood))
                          (format t "smart repeated elements:  ~a~%"
                                  (length smart-repeated-neighbors))
                          (format t "smart once-only elements: ~a~%"
                                  (length smart-once-only-neigbors))
                          )))))



   `(with-basic-solution (s1 ,solution)

      (bformat t "Exploring neighborhood ~a"
               ,name-for-bformat)

      ;; (format t "code-to-use-in-the-smart-version: ~a~%"
      ;;         ',code-to-use-in-the-smart-version)

      ;; (format t "inner code for the smart version: ~a~%"
      ;;         ',inner-code-for-the-smart-version)

      (let* ((wc (basic-working-copy s1))
             (ops-list nil)
             (current-neighbor nil)
             (all-neighborhood nil)
             (repeated-neighbors nil)
             (once-only-neigbors nil)
             (smart-neighborhood nil)
             (smart-repeated-neighbors nil)
             (smart-once-only-neigbors nil)
             (repeated-info nil)
             (intersection nil)
             (difference nil)
             (membership-info nil)
             (info-to-print nil)
             )

        (prepare-solution-for-neighborhood-exploration wc)

        ;; here we execute the criterion with the code

        ,code-with-headings-for-criterion

        ;; now let's test the "smart" version
        ,code-with-headings-for-smart-version

        ,@code-to-print-the-info

        ,@code-to-print-the-smart-repeated-neighbors

        ,@code-to-print-the-intersection

        ,@code-to-print-the-difference

        ;; (format t "all neighborhood:~%~{ ~a~%~}~%" all-neighborhood)
        ;; (format t "~a~%" smart-neighborhood)


        ;; (format t "full neighborhood:~%~{~a~%~}~%"
        ;;         (reverse all-neighborhood)
        ;;         ;; (reverse (mapcar 'first smart-neighborhood))
        ;;         )


        ;; (format t "smart neighborhood:~%~{~a~%~}~%"
        ;;         (reverse smart-neighborhood)
        ;;         ;; (reverse (mapcar 'first smart-neighborhood))
        ;;         )
        ))))

(make-classical-criterion
 rab
 ((select-route r1)
  (select-client c1 from r1)
  (insert-client c1 into r1)))

(make-classical-criterion rabs ((select-route r1)
                               (select-client c1 from r1)
                               (insert-client c1 into r1
                                 :ex (list
                                      (list r1 c1.select.position)
                                      (list r1 (1- c1.select.position)))
                                 )))

(make-classical-criterion rabs*
   ((select-route r1)
    (select-client c1 from r1)
    (insert-client c1 into r1
       :ex (list
            (list r1 c1.select.position)
            (list r1 (1- c1.select.position))))))

(make-classical-criterion rad ((select-route r1)
                                (select-client c1 from r1)
                                (add-route c1)))

(make-classical-criterion rads ((select-route r1)
                                (select-client c1 from r1)
                                (add-route c1)))

(make-classical-criterion rads* ((select-route r1)
                                 (select-client c1 from r1)
                                 (add-route c1)))

(make-classical-criterion rarb
  ((select-route r1)
   (select-client c1 from r1)
   (select-route r2)
   (insert-client c1 into r2)))

(make-classical-criterion rarbs
  ((select-route r1)
   (select-client c1 from r1)
   (select-route r2)
   (insert-client c1 into r2
    :ex (list c1.select.coord
              (list c1.route
                    (1- c1.select.position))))))

(make-classical-criterion rarbs*
  ((select-route r1)
   (select-client c1 from r1)
   (select-route r2 :dt r1)
   ;; we don't need to add any constraint
   ;; to the insertion of the client
   ;; because the only constraints were
   ;; in the same route as in the selection
   ;; but that case is excluded now
   (insert-client c1 into r2)))

(make-classical-criterion rarac ((select-route r1)
                                 (select-client c1 from r1)
                                 (select-route r2)
                                 (select-client c2 from r2)
                                 (swap-clients c1 c2)))

(make-classical-criterion raracs ((select-route r1)
                                 (select-client c1 from r1)
                                 (select-route r2 :ge r1)
                                 (select-client
                                  c2 from r2
                                  :ge (list r1 c1.select.position))
                                 (swap-clients c1 c2)))

(make-classical-criterion raracs*
   ((select-route r1)
    (select-client c1 from r1)
    (select-route r2 :ge r1)
    (select-client c2 from r2
      ;; avoid the insertion in the same coord
      ;; where c1 was selected from
      ;; because this also happens in rab
      ;; and as we need to avoid insertions
      ;; before c1.coord, we can translate it
      ;; into:
      :gt c1.select.coord)
    (swap-clients c1 c2)))

(make-classical-criterion ref ((select-route r1)
                               (select-subroute z1 from r1)
                               (insert-subroute z1 into r1)))

(make-classical-criterion refs
  ((select-route r1)
   (select-subroute z1 from r1)
   (insert-subroute z1 into r1
     ;; insert beyond the selection coord
     :gt z1.select.coord)))

(make-classical-criterion refs*
  ((select-route r1)
   (select-subroute z1 from r1
     ;; avoid collisions with rabs*
     :ldt 1)
   (insert-subroute z1 into r1
     ;; insert beyond the selection coord
     :gt z1.select.coord
     ;; don't insert right after the
     ;; select coord
     ;; because this is equivalent to select
     ;; the client at that position and
     ;; insert it right before the subroute
     :dt (list r1 (1+ z1.position)))))

(make-classical-criterion rerf ((select-route r1)
                                (select-subroute c1 from r1)
                                (select-route r2)
                                (insert-subroute c1 into r2)))

(make-classical-criterion rerfs ((select-route r1)
                                 (select-subroute z1 from r1)
                                 (select-route r2)
                                 (insert-subroute z1 into r2
                                    :ex-cond (and
                                              (= r1 r2)
                                              (<= z1.ins.pos
                                                  z1.position)))))

(make-classical-criterion rerfs* 
  ((select-route r1)
   (select-subroute c1 from r1
      ;; avoid collisions with rarbs*
      :ldt 1)
   (select-route r2
     ;; avoid collisions with rerfs
     :dt r1)
   (insert-subroute c1 into r2
      ;; avoid collisions with itself
      :ex-cond (and
                (= r1 r2)
                (<= c1.ins.pos c1.position)))))

(make-classical-criterion rereg ((select-route r1)
                                 (select-subroute c1 from r1)
                                 (select-route r2)
                                 (select-subroute c2 from r2)
                                 (swap-subroutes c1 c2)))

(make-classical-criterion reregs ((select-route r1)
                                  (select-subroute z1 from r1)
                                  (select-route r2)
                                  (select-subroute z2 from r2
                                     :ge z1.select.coord)
                                  (swap-subroutes z1 z2)))

(make-classical-criterion reregs* ((select-route r1)
                                   (select-subroute z1 from r1)
                                   (select-route r2 :ge r1)
                                   (select-subroute z2 from r2
                                      :lex-cond
                                      (= z1.length z2.length 1)
                                      :ge z1.select.coord)
                                   (swap-subroutes z1 z2)))

(make-classical-criterion rehf ((select-route r1)
                                (select-subroute z1 from r1)
                                (reverse-subroute z1)
                                (insert-subroute z1 into r1)))

(make-classical-criterion rehfs ((select-route r1)
                                 (select-subroute z1 from r1)
                                 (reverse-subroute z1)
                                 (insert-subroute z1 into r1
                                   :ex-cond
                                   (or (= z1.ins.pos
                                          z1.select.position)
                                       (= z1.ins.pos
                                          (1- z1.select.position))))))

(make-classical-criterion rehfs*
    ((select-route r1)
     (select-subroute z1 from r1
       ;; avoid collisions with rabs*
       :ldt 1)
     (reverse-subroute z1)
     (insert-subroute z1 into r1
        ;; avoid collisions with itself
        :ex-cond
        (or (= z1.ins.pos
               z1.select.position)
            (= z1.ins.pos
               (1- z1.select.position))))))

(make-classical-criterion rehrf ((select-route r1)
                                 (select-subroute z1 from r1)
                                 (select-route r2)
                                 (reverse-subroute z1)
                                 (insert-subroute z1 into r2)))

(make-classical-criterion rehrfs ((select-route r1)
                                  (select-subroute z1 from r1)
                                  (select-route r2)
                                  (reverse-subroute z1)
                                  (insert-subroute z1 into r2
                                      :ex-cond
                                      (and
                                       (= r1 r2)
                                       (or
                                        (= z1.ins.pos
                                           z1.select.position)
                                        (= z1.ins.pos
                                           (1- z1.select.position)))))))

(make-classical-criterion rehrfs*
  ((select-route r1)
   (select-subroute z1 from r1
      ;; to avoid collisions with rarb
      :ldt 1)
   (select-route r2
      ;; to avoid collisions with rehf
      :dt r1)
   (reverse-subroute z1)
   (insert-subroute z1 into r2
     ;; 
     ;; we don't need to avoid collisions
     ;; with itself because these collisions
     ;; appear when r1 = r2 and we have
     ;; have already forbidden that.
     )))

(make-classical-criterion rehreg ((select-route r1)
                                  (select-subroute c1 from r1)
                                  (select-route r2)
                                  (select-subroute c2 from r2)
                                  (reverse-subroute c1)
                                  (swap-subroutes c1 c2)))

(make-classical-criterion rehregs
   ((select-route r1)
    (select-subroute c1 from r1)
    (select-route r2)
    (select-subroute c2 from r2
      :ex-cond               
      (or
       (and ;; both routes with length 1 and r1 < r2
        (= c1.length c2.length 1)
        (< r1 r2)
        )
       (and ;; |c1| = |c2| = 1, r1 = r2, and c2 < c1
        (= c1.length c2.length 1)
        (= r1 r2)
        (< c2.position c1.position))
       ;; otherwise we'll apply the constraints
       ;; if we are in the same route
       (and
        (= r1 r2)
        (or ;; each case in isolation
         ;; we'll split the analysis in several cases
         ;; that I'll write in the comments
         ;; |c1| = 1 and |c2| = 1
         (and ;; these are the case where both length are 1
          (= c1.length 1)
          (= c2.length 1)
          (or ;; pivoting effect
           ;; the first two remove the "pivoting effect"
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (+ c1.position 1))
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (- c1.position 2))))

         ;; |c1| = 1 and |c2| > 1 
         (and
          (= c1.length 1)
          (> c2.length 1)
          (or
           (and ;; don't go pass me
            (< c2.position c1.position)
            (>= (1-
                 (+ c2.position c2.length))
                c1.position))
           (or ;; pivoting effect
            ;; the first two remove the "pivoting effect"
            ;; this is the case where c2.pos < c1.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; this is the case where c2.pos > c1.pos
            (= c2.position
               (+ c1.position 1)))))
         ;; TODO |c1| > 1 and |c2| = 1
         (and
          (> c1.length 1)
          (= c2.length 1)
          (or ;; pivot and symmetry
           ;; the first two remove the "pivoting effect"
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (+ c1.position 1))
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (- c1.position 2))
           ;; and this one removes the
           ;; symmetry effect
           (= c2.position
              (- c1.position 1))))

         ;; |c1| > 1 and |c2| > 1
         (and
          (> c1.length 1)
          (> c2.length 1)
          (or
           (and
            (< c2.position c1.position)
            (>= (1- (+ c2.position c2.length))
                c1.position))
           (or ;; pivoting effect  
            ;; this is the case where c2.pos < c1.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; this is the case where c2.pos > c1.pos
            (= c2.position
               (+ c1.position 1)))))
         ))))
    (reverse-subroute c1)
    (swap-subroutes c1 c2)))

(make-classical-criterion rehregs*
   ((select-route r1)
    (select-subroute c1 from r1
       ;; if c1.length is 1 then we don't
       ;; reverse anything and all those
       ;; neighboors are explored in rereg
       :ldt 1)
    (select-route r2)
    (select-subroute c2 from r2
       ;; to avoid collisions with rarac
       ;; don't select both subroutes with length 1
       :lex-cond (= c1.length c2.length 1)
       ;; ;; but also we should not select c2
       ;; ;; right where c1 ends, if c1.length is 2
       ;; ;; I'm adding this constraint
       ;; ;; to the ex-cond

      :ex-cond               
      (or
       (and ;; avoid other collisions with rarac
         ;; we should avoid the case
         ;; when c1.length is 2, c2.length is 1
         ;; and we are in the same route, and c2 is
         ;; selected from the same place as c1
         ;; this can also be obtained in rarac
         (= r1 r2)
         (= c1.length 2)
         (= c2.length 1)
         (= c2.position c1.position))

       ;; (and ;; |c1| = |c2| = 1 and r1 < r2
       ;;  ;; we don't need this because we have already
       ;;  ;; forbidden r1 = r2.
       ;;  (= c1.length c2.length 1)
       ;;  (< r1 r2))
       ;; (and ;; |c1| = |c2| = 1, r1 = r2, and c2 < c1
       ;;  ;; we don't need this because we have already
       ;;  ;; forbidden r1 = r2.
       ;;  (= r1 r2)
       ;;  (= c1.length c2.length 1)
       ;;  (< c2.position c1.position))
       ;; otherwise we'll apply the constraints
       ;; if we are in the same route
       (and
        (= r1 r2)
        (or ;; each case in isolation
         ;; we'll split the analysis in several cases
         ;; that I'll write in the comments

         ;; |c1| = 1 and |c2| = 1
         ;; we don't need this because we have already
         ;; forbidden r1 = r2
         ;; to avoid collisions with rarac

         ;; |c1| = 1 and |c2| > 1
         ;; no need to analyze this because
         ;; we have already ruled out this case
         ;; to avoid collisions with rereg


         (and ;; |c1| > 1 and |c2| = 1
          (> c1.length 1)
          (= c2.length 1)
          (or ;; pivot and symmetry
           ;; the first two remove the "pivoting effect"
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (+ c1.position 1))
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (- c1.position 2))
           ;; and this one removes the
           ;; symmetry effect
           (= c2.position
              (- c1.position 1))))

         ;; |c1| > 1 and |c2| > 1
         (and
          (> c1.length 1)
          (> c2.length 1)
          (or
           (and
            (< c2.position c1.position)
            (>= (1- (+ c2.position c2.length))
                c1.position))
           (or ;; pivoting effect  
            ;; this is the case where c2.pos < c1.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; this is the case where c2.pos > c1.pos
            (= c2.position
               (+ c1.position 1)))))))))
    (reverse-subroute c1)
    (swap-subroutes c1 c2)))

(make-classical-criterion rerehg ((select-route r1)
                                  (select-subroute c1 from r1)
                                  (select-route r2)
                                  (select-subroute c2 from r2)
                                  (reverse-subroute c2)
                                  (swap-subroutes c1 c2)))

(make-classical-criterion rerehgs
    ((select-route r1)
     (select-subroute c1 from r1)
     (select-route r2)
     (select-subroute c2 from r2
        :ex-cond               
        (or
         ;; if both subroutes have length 1
         ;; exclude if r2 < r1
         (and
          (= c1.length c2.length 1)
          (< r2 r1))
         ;; let's apply the following rules
         ;; if both subroutes are selected
         ;; from the same route
         (and ;; both subroutes are from the same subroute
          (= r1 r2)
          (or ;; let's analyze all the cases
           (and ;; |c1| = |c2| = 1
            (= c1.length 1)
            (= c2.length 1)
            (or
             ;; c2 is "before" c1
             (< c2.position c1.position)
             (or ;; exclued the pivoting effect
              ;; c1.pos < c2.pos
              (= c2.position
                 (+ c1.position 1)))))
           (and ;; |c1| = 1, |c2| > 1
            (= c1.length 1)
            (> c2.length 1)
            ;; let's remove the pivoting effect
            (or ;; pivoting and symmetric effect
             ;; c1.pos < c2.pos
             (= c2.position
                (+ c1.position 1))
             ;; c1.pos > c2.pos
             (= c2.position
                (- c1.position c2.length 1))
             ;; let's remove the symmetric repetitions
             (= c2.position
                (- c1.position c2.length)))




            )
           (and ;; |c1| > 1, |c2| = 1
            ;; there are not repetitions in this case :-o
            ;; I tested up to 10 clients
            ;; but when we mix it with the other
            ;; cases, some repeated elements appear :-/
            ;; so we should remove the pivoting effect
            (and
             (> c1.length 1)
             (= c2.length 1)
             (or ;; let's remove the pivoting effect
              ;; this is the case where c2.pos > c1.pos
              (= c2.position
                 (+ c1.position 1))         
              ;; this is the case where c2.pos < c1.pos
              (= c2.position
                 (- c1.position c2.length 1))
              ;; and this one is for the symmetric effect
              (= c2.position
                 (- c1.position 1)))))
           (and ;; |c1| > 1, |c2| > 1
            (> c1.length 1)
            (> c2.length 1)
            (or ;; let's analyze the possible problems

             (and ;; don't stop before me
              ;;  ;; It worked!!!
              ;;  ;; Now Iosvanny must find out why :-o
              (< c2.position c1.position)

              (= (+ c2.position c2.length)
                 c1.position))

             (or ;; let's remove the pivoting effect
              ;; this is the case where c2.pos > c1.pos
              (= c2.position
                 (+ c1.position 1))         
              ;; this is the case where c2.pos < c1.pos
              (= c2.position
                 (- c1.position c2.length 1)))
             ))))))
     (reverse-subroute c2)
     (swap-subroutes c1 c2)))

(make-classical-criterion rerehgs*
    ((select-route r1)
     (select-subroute c1 from r1)
     ;; (select-route r2)
     (select-subroute c2 from r1
       ;; to avoid collisions with rarac                         
       ;; avoid |c1| = |c2| = 1
       :lex-cond (= c1.length c2.length 1)
       ;; we also need to avoid c2.pos = c1.pos
       ;; because we can get the same neighbor
       ;; with rarac. This is the same restriction
       ;; as in rehreg. I'll also add it to the
       ;; ex-cond.

       ;; the following line avoids collisions
       ;; with rereg
       :ldt 1

       ;; an from here on is to avoid
       ;; collisions with itself
       :ex-cond               
       (or
        (and ;; avoid other collisions with rarac
         ;; we should avoid the case
         ;; when |c1| = 1, |c2| = 2, r1 = r2,
         ;; and c2 is selected from the same place
         ;; as c1 this can also be obtained in rarac.
         (= c2.length 2)
         (= c1.length 1)
         (= c2.position c1.position))
        ;; |c1| = |c2| = 1 ;; ruled out by rarac
        (and ;; |c1| = 1, |c2| > 1
         (= c1.length 1)
         (> c2.length 1)

         (or ;; all the constraints here
          (or ;; pivoting effect and symmetry
           ;; c1.pos < c2.pos
           (= c2.position
              (+ c1.position 1))
           ;; c1.pos > c2.pos
           (= c2.position
              (- c1.position c2.length 1))
           ;; let's remove the symmetric repetitions
           (= c2.position
              (- c1.position c2.length))
           ;; don't go beyond me!!! to avoid
           ;; collisions with rereg
           (and ;; |c1| = 1, |c2| > 1
            (< c2.position c1.position)
            (= (+ c2.position c2.length -1)
               c1.position))
           )
          ;; avoid |c2| >= 3 and concatenation
          (and ;; avoid |c2| >= 3 and concatenation
           (>= c2.length 3)
           (= c1.position c2.position))
          (or ;; to avoid collisions with rehreg pivots with length
           (or ;; c2.pos > c1.pos
            (> c2.position (+ c1.position 1))
            ;; c2.pos < c1.pos
            (> c1.position (+ c2.position c2.length))))
          ) ;; here ends |c1| = 1, |c2| > 1

         )
        ;; |c1| > 1, |c2| = 1 ;; no need [it is rereg]
        (and ;; |c1| > 1, |c2| > 1
         (> c1.length 1)
         (> c2.length 1)
         (or ;; let's analyze the possible problems
          (and ;; [don't stop before me]
           ;;  ;; It worked!!!
           (< c2.position c1.position)
           (= (+ c2.position c2.length)
              c1.position))

          ;; |c1| > 1, |c2| > 1
          (and ;; don't go beyond me!!! to avoid rereg
           ;; collisions with rereg
           ;; |c1| > 1, |c2| > 1
           (< c2.position c1.position)
           (= (+ c2.position c2.length -1)
              c1.position))
          (and ;; to avoid collisions with rereg
           ;; |c2| = 2 and c2.pos = c1.pos + c1.length - 1
           ;; this is equivalent to a rereg with pivot
           ;; 
           (= c2.length 2)
           (= (+ c2.position c2.position)))
          (or ;; let's remove the pivoting effect
           ;; this is the case where c2.pos > c1.pos
           (= c2.position
              (+ c1.position 1))         
           ;; this is the case where c2.pos < c1.pos
           (= c2.position
              (- c1.position c2.length 1)))
          ;; avoid |c2| >= 3 and concatenation
          (and ;; avoid |c2| >= 3 and concatenation
           (>= c2.length 3)
           (= c1.position c2.position))
          (or ;; to avoid collisions with rehreg pivots with length
           (or ;; c2.pos > c1.pos
            (> c2.position (+ c1.position 1))
            ;; c2.pos < c1.pos
            (> c1.position (+ c2.position c2.length)))))
         )
        ))
     (reverse-subroute c2)
     (swap-subroutes c1 c2)))

(make-classical-criterion rehrehg ((select-route r1)
                                   (select-subroute c1 from r1)
                                   (select-route r2)
                                   (select-subroute c2 from r2)
                                   (reverse-subroute c1)
                                   (reverse-subroute c2)
                                   (swap-subroutes c1 c2)))

(make-classical-criterion rehrehgs
   ((select-route r1)
    (select-subroute c1 from r1)
    (reverse-subroute c1)
    (select-route r2 :ge r1)
    (select-subroute c2 from r2
       :ex-cond               
       (or
        ;; both subroutes are from the same route
        ;; because we exclude the situation
        ;; r2 < r1 in the selection of r2.
        (and
         (= r1 r2)
         (or ;; the general constraints
          (and ;; |c1| = |c2| = 1
           (= c1.length 1)
           (= c2.length 1)
           (or ;; the constraints here
            ;; c2 should be selected after c1
            (< c2.position c1.position)
            (or ;; pivoting effect
             ;; this is the case where c2.pos > c1.pos
             ;; (the case c2.pos < c1.pos is excluded
             ;; in the previous "or")
             (= c2.position
                (+ c1.position 1)))))
          (and ;; |c1| = 1 and |c2| > 1
           (= c1.length 1)
           (> c2.length 1)
           (or ;; pivoting and symmetric effects
            ;; c1.pos < c2.pos
            (= c2.position
               (+ c1.position 1))
            ;; c1.pos > c2.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; let's remove the symmetric repetitions
            (= c2.position
               (- c1.position c2.length))
            ))
          (and ;; |c1| > 1 and |c2| = 1
           (> c1.length 1)
           (= c2.length 1)
           (or ;; the constraints here
            (or ;; pivot and symmetry
             ;; the first two remove the "pivoting effect"
             ;; this is the case where c2.pos > c1.pos
             (= c2.position
                (+ c1.position 1))
             ;; this is the case where c2.pos > c1.pos
             (= c2.position
                (- c1.position 2))
             ;; ;; and this one removes the
             ;; ;; symmetry effect
             (= c2.position
                (- c1.position 1)))
            (or ;; don't append to end of a subroute
             (= c2.position c1.position))
            (or ;; the anti pivot??? :-/
             ;; c2 > c1
             (/= c2.position
                 (+ c1.position 1))
             ;; c2 < c1
             (/= c2.position
                 (- c1.position 2)))))
          (and ;; |c1| > 1 and |c2| > 1
           (> c1.length 1)
           (> c2.length 1)
           (or ;; the constraints here
            (or ;; pivoting effect
             ;; this is the case where c2.pos < c1.pos
             (= c2.position
                (- c1.position c2.length 1))
             ;; this is the case where c2.pos > c1.pos
             (= c2.position
                (+ c1.position 1)))
            (or ;; "don't touch me! (if we are both subroutes)"
             (and ;; c1.pos <= c2.pos
              (>= c2.position c1.position)
              (= c2.position c1.position))
             (and ;; c1.pos >= c2.pos
              (<= c2.position c1.position)
              (= (+ c2.position c2.length)
                 c1.position)))
            (or ;; the anti pivot??? :-/
             ;; let's remove the second one when c2 > c1
             (> c2.position
                (+ c1.position 1)))
            ))))))
    (reverse-subroute c2)
    (swap-subroutes c1 c2)))

(make-classical-criterion rehrehgs*
   ((select-route r1)
    (select-subroute c1 from r1)
    (reverse-subroute c1)
    (select-route r2 :ge r1)
    (select-subroute c2 from r2
       ;; to avoid collisions with rarac
       :lex-cond (or
                  (= c1.length 1)
                  (= c2.length 1))
       :ex-cond               
       (or
        (or     ;; the general constraints
         ;; we have ruled out the following cases:
         ;; |c1| = |c2| = 1 because it is rarac
         ;; |c1| = 1 and |c2| > 1 because it is rerehg
         ;; |c1| > 1 and |c2| = 1 because it is rehreg
         (and ;; |c1| > 1 and |c2| > 1
          (> c1.length 1)
          (> c2.length 1)
          (or    ;; the constraints here
           (or   ;; pivoting effect
            ;; this is the case where c2.pos < c1.pos
            (= c2.position
               (- c1.position c2.length 1))
            ;; this is the case where c2.pos > c1.pos
            (= c2.position
               (+ c1.position 1)))
           (or     ;; "don't touch me! (if we are both subroutes)"
            (and   ;; c1.pos <= c2.pos
             (>= c2.position c1.position)
             (= c2.position c1.position))
            (and ;; c1.pos >= c2.pos
             (<= c2.position c1.position)
             (= (+ c2.position c2.length)
                c1.position)))
           (or ;; the anti pivot??? :-/
            ;; let's remove the second one when c2 > c1
            (> c2.position
               (+ c1.position 1)))
           )))))
    (reverse-subroute c2)
    (swap-subroutes c1 c2)))

(make-classical-criterion rereehf
   ((select-route route1)
    (select-subroute subroute1 from route1)
    (select-route route2)
    (select-subroute subroute2 from route2)
    (select-subroute subroute3 from route2)
    (swap-subroutes subroute3 subroute2)
    (insert-subroute subroute1 into route1)
    ))
